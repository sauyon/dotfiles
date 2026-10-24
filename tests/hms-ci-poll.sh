#!/usr/bin/env bash
# Cases for `hms`'s CI-polling half (defined in home.nix).
#
# The bug that prompted these: hms wrote the Authorization header into a curl
# config **once**, before the poll began, and every request for the next hour
# reused it. forge.ko.ag is an OAuth grant (`fj auth login`), and its access
# token carries an expires_at roughly an hour out -- the same hour the poll is
# bounded by. So any run that outlived the token's remaining life started 401ing
# mid-poll, and hms sat there printing "still waiting" at a dead token until the
# deadline. The push in the same invocation kept working, because that goes
# through git-credential-fj, which re-mints an expired grant -- today by
# exec'ing `fj git-credential`, which refreshes under fj's own lock; when this
# was written, by poking `fj whoami` under flock(1). Only the polling path
# lacked it.
#
# So the contract these pin is narrow and mechanical: hms asks the credential
# helper for a token **per request**, not once per run. Case 1 is the regression;
# it fails against the old script by observing exactly one token fetch.
#
# They drive the real built script through its four seams -- HMS_REPO (a
# throwaway git repo with a bare origin, so nothing touches the real one),
# HMS_CURL (a stub forge), HMS_TOKEN_CMD (a stub credential helper that counts
# its calls) and HMS_SWITCH_CMD (so a "switch" is a marker file, not an actual
# home-manager activation).
#
#   ./tests/hms-ci-poll.sh                  # builds hms, then tests it
#   ./tests/hms-ci-poll.sh /path/to/hms     # tests one you already have
set -u

SCRIPT="${1:-}"
if [ -z "$SCRIPT" ]; then
  host=$(uname -n)
  drv=$(nix eval --raw ".#homeConfigurations.$host.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: p.name or "" == "hms") ps)).drvPath') \
    || { echo "could not evaluate hms for $host" >&2; exit 1; }
  SCRIPT="$(nix-store --realise "$drv" | tail -1)/bin/hms"
  echo "testing $SCRIPT"
fi
[ -x "$SCRIPT" ] || { echo "not executable: $SCRIPT" >&2; exit 1; }

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0
# A caller's own setting would leak into every case through run() and stand in
# for the shipped default that "two transient failures in a row" and "three
# transient failures" are there to pin.
unset HMS_RETRIES

# A throwaway repo whose origin is a bare repo next to it, so hms's fetch/push
# half runs for real without touching anything that matters.
setup_repo() {
  rm -rf "$D/origin.git" "$D/repo"
  git init --quiet --bare "$D/origin.git"
  git init --quiet "$D/repo"
  git -C "$D/repo" config user.email "t@t"
  git -C "$D/repo" config user.name "t"
  echo x > "$D/repo/f"
  git -C "$D/repo" add f
  git -C "$D/repo" commit --quiet -m first
  git -C "$D/repo" branch -M master
  git -C "$D/repo" remote add origin "$D/origin.git"
  git -C "$D/repo" push --quiet -u origin master
  # One unpushed commit, so hms takes its normal "push, then wait" path.
  echo y > "$D/repo/f"
  git -C "$D/repo" commit --quiet -am second
  git -C "$D/repo" rev-parse HEAD > "$D/sha"
}

# Stub credential helper. Appends a line per invocation, so a case can assert how
# many times hms asked, and emits whatever token the case staged. Mirrors
# git-credential-fj's contract: `get` on argv, key=value on stdin, password= out.
mktoken() { # mktoken <token-to-emit>
  : > "$D/token-calls"
  printf '%s' "$1" > "$D/token-value"
  cat > "$D/token-cmd" <<STUB
#!/bin/sh
echo call >> "$D/token-calls"
cat >/dev/null
printf 'username=oauth2\npassword=%s\n' "\$(cat "$D/token-value")"
STUB
  chmod +x "$D/token-cmd"
}

# A credential helper that changes its answer on the second call, the way a real
# refresh does: the first read hands back the expired grant, the poke rewrites
# keys.json, and the next read picks up the new one.
mktoken_refreshing() { # mktoken_refreshing <stale> <fresh>
  : > "$D/token-calls"
  cat > "$D/token-cmd" <<STUB
#!/bin/sh
echo call >> "$D/token-calls"
cat >/dev/null
if [ "\$(wc -l < "$D/token-calls")" -le 1 ]; then
  printf 'username=oauth2\npassword=%s\n' '$1'
else
  printf 'username=oauth2\npassword=%s\n' '$2'
fi
STUB
  chmod +x "$D/token-cmd"
}

# A helper that answers with more than one password line. git-credential-fj emits
# exactly one, but the curlrc is a line-oriented format: a token that arrives as
# two lines does not become a longer token, it becomes a truncated header plus a
# stray directive on the next line. Taking the first match is what keeps a
# malformed answer from turning into a malformed config.
mktoken_multi() {
  : > "$D/token-calls"
  cat > "$D/token-cmd" <<STUB
#!/bin/sh
echo call >> "$D/token-calls"
cat >/dev/null
printf 'username=oauth2\npassword=good\npassword=evil\n'
STUB
  chmod +x "$D/token-cmd"
}

# Stub forge. Answers on the URL, and -- the point of the whole exercise --
# on the token it was actually handed, which it reads back out of the curl
# config hms wrote. `-w '\n%{http_code}'` means the body must be followed by a
# newline and the status code, which is how hms tells 401 from 200.
mkcurl() { # mkcurl <good-token> <run-status>
  cat > "$D/curl" <<STUB
#!/usr/bin/env bash
url=""; cfg=""
while [ \$# -gt 0 ]; do
  case "\$1" in
    -K) cfg="\$2"; shift 2 ;;
    https://*) url="\$1"; shift ;;
    *) shift ;;
  esac
done
tok=\$(sed -n 's/.*Authorization: token \([^"]*\)".*/\1/p' "\$cfg")
echo "\$url \$tok" >> "$D/requests"
if [ "\$tok" != '$1' ]; then
  printf 'unauthorized\n401\n'
  exit 0
fi
case "\$url" in
  *actions/tasks*)
    printf '{"workflow_runs":[{"head_sha":"%s","workflow_id":"nix-home.yml","run_number":42,"status":"%s"}]}\n200\n' \\
      "\$(cat "$D/sha")" '$2' ;;
  *jobs/0*)   printf 'job_id 7 not found\n200\n' ;;
  *jobs/*logs*) printf 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxerror: the build broke\n200\n' ;;
  *)          printf '{}\n200\n' ;;
esac
STUB
  chmod +x "$D/curl"
}

# A stub forge that has NO run for our sha, but does have one for some other
# commit, in a status the case picks. This is the shape Forgejo presents while
# nix-home.yml's concurrency group is occupied: the run holding the group is
# listed, and the one queued behind it does not exist yet.
mkcurl_other() { # mkcurl_other <good-token> <other-run-status>
  cat > "$D/curl" <<STUB
#!/usr/bin/env bash
url=""; cfg=""
while [ \$# -gt 0 ]; do
  case "\$1" in
    -K) cfg="\$2"; shift 2 ;;
    https://*) url="\$1"; shift ;;
    *) shift ;;
  esac
done
tok=\$(sed -n 's/.*Authorization: token \([^"]*\)".*/\1/p' "\$cfg")
echo "\$url \$tok" >> "$D/requests"
if [ "\$tok" != '$1' ]; then
  printf 'unauthorized\n401\n'
  exit 0
fi
case "\$url" in
  *actions/tasks*)
    printf '{"workflow_runs":[{"head_sha":"%s","workflow_id":"nix-home.yml","run_number":191,"status":"%s"}]}\n200\n' \\
      00000000000000000000000000000000000000ff '$2' ;;
  *) printf '{}\n200\n' ;;
esac
STUB
  chmod +x "$D/curl"
}

# A stub forge that reports the group busy on its first task listing and then
# goes unreachable. Real shape: a 5xx or a dropped connection somewhere inside
# the hour-long busy wait.
mkcurl_other_then_down() { # mkcurl_other_then_down <good-token>
  : > "$D/task-calls"
  cat > "$D/curl" <<STUB
#!/usr/bin/env bash
url=""; cfg=""
while [ \$# -gt 0 ]; do
  case "\$1" in
    -K) cfg="\$2"; shift 2 ;;
    https://*) url="\$1"; shift ;;
    *) shift ;;
  esac
done
tok=\$(sed -n 's/.*Authorization: token \([^"]*\)".*/\1/p' "\$cfg")
if [ "\$tok" != '$1' ]; then
  printf 'unauthorized\n401\n'
  exit 0
fi
case "\$url" in
  *actions/tasks*)
    echo call >> "$D/task-calls"
    if [ "\$(wc -l < "$D/task-calls")" -le 1 ]; then
      printf '{"workflow_runs":[{"head_sha":"%s","workflow_id":"nix-home.yml","run_number":191,"status":"running"}]}\n200\n' \\
        00000000000000000000000000000000000000ff
    else
      printf 'bad gateway\n502\n'
    fi ;;
  *) printf '{}\n200\n' ;;
esac
STUB
  chmod +x "$D/curl"
}

# A "switch" is a marker file: the cases care that hms decided to switch, not
# that a home-manager activation ran.
mkswitch() {
  rm -f "$D/switched"
  cat > "$D/switch" <<STUB
#!/bin/sh
echo "\$@" > "$D/switched"
STUB
  chmod +x "$D/switch"
}

run() {
  rm -f "$D/requests"
  # HMS_WAIT_SECONDS bounds the status poll, which ships at an hour. Without it
  # a case that never reaches a terminal status -- which is exactly what the
  # unfixed script does with a stale token -- would hang the suite for that hour.
  # HMS_FIRST_RUN_SECONDS bounds the *other* poll -- the one waiting for a run
  # to be created at all, which ships at 90s. The cases below that exercise it
  # would otherwise sit through that window twice.
  # HMS_RETRIES only when a case sets it: a defaulted value here would be what
  # every case reads, and the script's own default would go untested. Unlike
  # the two waits, whose shipped values are too long to sit through.
  timeout 180 env -i PATH="$PATH" HOME="$HOME" ${HMS_RETRIES+"HMS_RETRIES=$HMS_RETRIES"} \
    HMS_REPO="$D/repo" HMS_CURL="$D/curl" HMS_TOKEN_CMD="$D/token-cmd" \
    HMS_SWITCH_CMD="$D/switch" HMS_WAIT_SECONDS="${HMS_WAIT_SECONDS:-20}" HMS_FIRST_RUN_SECONDS=1 \
    "$SCRIPT" 2>&1
}

report() { # report <name> <ok?> <detail>
  n=$((n + 1))
  if [ "$2" = ok ]; then
    printf 'ok %d - %s\n' "$n" "$1"
  else
    printf 'FAIL %d - %s\n      %s\n' "$n" "$1" "$3"
    fails=$((fails + 1))
  fi
}

# --- the regression -------------------------------------------------------
# One token fetch for the whole run is the bug. hms must ask again for each
# request, so that a token which expired since the last one gets refreshed.
setup_repo; mktoken good; mkcurl good success; mkswitch
out=$(run); rc=$?
calls=$(wc -l < "$D/token-calls")
if [ "$calls" -gt 1 ]; then
  report "token is fetched per request, not once per run" ok
else
  report "token is fetched per request, not once per run" no \
    "credential helper called $calls time(s); want >1. rc=$rc out=[$out]"
fi

# --- and the point of it: a stale token recovers ---------------------------
# The first request goes out with the expired grant and 401s; the refresh the
# helper performs is picked up by the next request, which succeeds. Before the
# fix this case cannot pass at all -- the 401 token is the only one hms ever has.
setup_repo; mktoken_refreshing stale good; mkcurl good success; mkswitch
out=$(run); rc=$?
# "switched" alone does not prove recovery: hms also switches when it gives up
# on reaching the forge at all ("request failed — switching locally"), which is
# exactly what a permanently-stale token produces. The run has to have been seen
# green for this to mean the refresh worked.
if [ -f "$D/switched" ] && printf '%s' "$out" | grep -q 'run 42 green'; then
  report "a 401 from an expired token recovers after refresh" ok
else
  report "a 401 from an expired token recovers after refresh" no \
    "switched=$([ -f "$D/switched" ] && echo y || echo n), never saw the run go green. rc=$rc out=[$out]"
fi

# --- behaviour these must not break ---------------------------------------
setup_repo; mktoken good; mkcurl good success; mkswitch
out=$(run); rc=$?
if [ -f "$D/switched" ] && [ "$rc" -eq 0 ]; then
  report "green run switches" ok
else
  report "green run switches" no "switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc out=[$out]"
fi

setup_repo; mktoken good; mkcurl good failure; mkswitch
out=$(run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ]; then
  report "failed run does not switch, and exits nonzero" ok
else
  report "failed run does not switch, and exits nonzero" no \
    "switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc out=[$out]"
fi

# The CI error lines are the reason hms fetches the log at all; a failure that
# prints nothing useful sends you to the web UI for no reason.
if printf '%s' "$out" | grep -q 'error: the build broke'; then
  report "a failed run prints CI's error lines" ok
else
  report "a failed run prints CI's error lines" no "out=[$out]"
fi

# An empty token must not go out as a request that 401s confusingly.
setup_repo; mktoken ""; mkcurl good success; mkswitch
out=$(run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ]; then
  report "no token is an error, not a silent 401 loop" ok
else
  report "no token is an error, not a silent 401 loop" no \
    "switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc out=[$out]"
fi

# `--option fallback true` is hard-coded into `switch_now` (home.nix) so a NAR
# the in-cluster
# attic Service can't deliver — Service reload, an attestation blip, anything
# short of the cluster actually being unreachable — degrades to a local build for
# that one path, rather than aborting the whole switch after ten minutes of
# transfer. Mirrors `--fallback` on the CI build step (`.forgejo/workflows/nix-home.yml`).
# Pin here so a future edit that drops the flag breaks loudly instead of
# reviving the 10-minute-stall class on box-side switches.
setup_repo; mktoken good; mkcurl good success; mkswitch
run >/dev/null
# Spelled as home-manager takes it, not as nix-build does: the CLI has no `--`
# passthrough and drops bare nix flags, so a test that greps for `--fallback`
# passes only against a switch that silently lost the flag. This assertion was
# the old spelling and had gone red against the shipped script.
if [ -f "$D/switched" ] && grep -q -F -- '--option fallback true' "$D/switched"; then
  report "switch_now passes 'fallback' to home-manager switch" ok
else
  report "switch_now passes 'fallback' to home-manager switch" no \
    "switch argv=[$(cat "$D/switched" 2>/dev/null)]"
fi

# Only the first password line is the token. Asserting through a real request
# rather than by reading the curlrc: what matters is that the forge receives a
# usable header, and a multiline token fails that whether it truncates, splits
# across directives, or makes curl reject the config outright.
setup_repo; mktoken_multi; mkcurl good success; mkswitch
out=$(run); rc=$?
if [ -f "$D/switched" ] && printf '%s' "$out" | grep -q 'run 42 green'; then
  report "a multi-line credential answer yields the first token, not a broken config" ok
else
  report "a multi-line credential answer yields the first token, not a broken config" no \
    "switched=$([ -f "$D/switched" ] && echo y || echo n), never saw the run go green. rc=$rc out=[$out]"
fi

# --- a run that does not exist YET is not a run that will never exist --------
# The incident: 9b6890a changed only .forgejo/workflows/nix-home.yml, which the
# workflow's own `paths:` filter lists, so a run was due. None had been created
# 90s later, because run 191 still held the concurrency group and Forgejo does
# not create the queued run until the holder finishes. hms read "no run" as
# "nothing CI-relevant changed" and built the closure on the laptop -- the one
# outcome it exists to prevent, announced with a reason it had not checked.
#
# An occupied group is visible in the very list hms already fetches, so it must
# keep waiting rather than assert a cause and fall back.
setup_repo; mktoken good; mkcurl_other good running; mkswitch
out=$(run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] \
   && ! printf '%s' "$out" | grep -q 'nothing CI-relevant changed'; then
  report "a busy concurrency group extends the wait instead of switching locally" ok
else
  report "a busy concurrency group extends the wait instead of switching locally" no \
    "switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc out=[$out]"
fi

# --- but with nothing in flight, the old conclusion still holds --------------
# No run for our sha and no run occupying the group means no run is coming:
# a docs-only commit, which should switch locally rather than stall for an hour.
# This is the case the fix above must not break.
setup_repo; mktoken good; mkcurl_other good success; mkswitch
out=$(run); rc=$?
if [ -f "$D/switched" ] \
   && printf '%s' "$out" | grep -q 'nothing CI-relevant changed'; then
  report "no run and an idle group still switches locally" ok
else
  report "no run and an idle group still switches locally" no \
    "switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc out=[$out]"
fi

# --- a blip during the busy wait does not become a local build --------------
# The busy wait runs for up to wait_seconds, so a single unreachable poll
# inside it is likely. Before the guard, that dropped straight through to
# "request failed — switching locally", which is the laptop build the whole
# branch exists to avoid. A failed request is not evidence the group freed up.
setup_repo; mktoken good; mkcurl_other_then_down good; mkswitch
out=$(run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] \
   && ! printf '%s' "$out" | grep -q 'switching locally'; then
  report "a forge blip during the busy wait keeps waiting, not switches" ok
else
  report "a forge blip during the busy wait keeps waiting, not switches" no \
    "switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc out=[$out]"
fi

# --- a rejected token stops hms; it is not a blip --------------------------
# Every request re-mints the header through the credential helper, so a 401 or
# 403 that comes back anyway is a login that cannot be refreshed: no amount of
# waiting fixes it, only `fj auth login` does. Before this, the discovery loop
# read a 401 as "request failed — switching locally" and built the closure on
# the laptop, and the status poll printed "still waiting" at the dead token for
# the rest of the hour (2026-10-23, run 248, which had in fact gone red).
#
# A stub forge that answers its first <n-ok> task listings with our run in
# progress, then 401s everything -- a token revoked mid-wait.
mkcurl_revoked() { # mkcurl_revoked <n-ok>
  : > "$D/task-calls"
  cat > "$D/curl" <<STUB
#!/usr/bin/env bash
url=""
for a in "\$@"; do case "\$a" in https://*) url="\$a" ;; esac; done
case "\$url" in
  *actions/tasks*)
    echo call >> "$D/task-calls"
    if [ "\$(wc -l < "$D/task-calls")" -le $1 ]; then
      printf '{"workflow_runs":[{"head_sha":"%s","workflow_id":"nix-home.yml","run_number":42,"status":"running"}]}\n200\n' \\
        "\$(cat "$D/sha")"
      exit 0
    fi ;;
esac
printf 'unauthorized\n401\n'
STUB
  chmod +x "$D/curl"
}

setup_repo; mktoken good; mkcurl_revoked 0; mkswitch
out=$(run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] \
   && printf '%s' "$out" | grep -q 'rejected our token' \
   && ! printf '%s' "$out" | grep -q 'switching locally'; then
  report "a 401 while looking for the run exits, not switches locally" ok
else
  report "a 401 while looking for the run exits, not switches locally" no \
    "switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc out=[$out]"
fi

setup_repo; mktoken good; mkcurl_revoked 1; mkswitch
start=$SECONDS; out=$(run); rc=$?; took=$((SECONDS - start))
# HMS_WAIT_SECONDS is 20: an hms that keeps polling a dead token runs it out.
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] && [ "$took" -lt 10 ] \
   && printf '%s' "$out" | grep -q 'rejected our token' \
   && ! printf '%s' "$out" | grep -q 'still waiting'; then
  report "a 401 mid-poll exits at once, not waits out the deadline" ok
else
  report "a 401 mid-poll exits at once, not waits out the deadline" no \
    "took=${took}s switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc out=[$out]"
fi

# --- a transient infra failure is retried, not handed back ------------------
# Run 252 (2026-10-24) went red on `error: RPC failed; HTTP 503` -- the forge
# 503'd the runner's git fetch, the commit was fine -- and hms stopped with "Fix
# CI". The fix was a workflow_dispatch and a second wait, which hms can do
# itself. Only when *every* error line in the log is infrastructure-shaped,
# though: a run that also says `error: builder ... failed` is a real failure,
# and re-dispatching it would just burn another build.
#
# A stub forge whose run for our sha fails, serving <log-file> as its log,
# until <n-fail> dispatches have been made, then goes green. The real shape of
# a dispatch, not a convenient one: the run it asks for does not exist for the
# next two task listings, and all the while the forge keeps listing the run
# that failed, first. That is what makes hms tell the retry's run from the
# failed one by number, and wait for it rather than give up.
#
# <mode>, optional: "moved" advances origin/master to another commit as the log
# is fetched (someone pushed mid-run), "nodispatch" makes the dispatch itself
# fail with a 503, "theirs" has another box's hms dispatch the retry (run 43,
# green) while ours is reading the log, "blip" 502s a listing during the retry
# wait, and "cancelled" reports the run cancelled, not failed.
mkcurl_flaky() { # mkcurl_flaky <n-fail> <log-file> [moved|nodispatch|theirs|blip|cancelled]
  echo 42 > "$D/run-number"; : > "$D/dispatches"; : > "$D/dispatch-tries"
  rm -f "$D/pending" "$D/theirs" "$D/blipped"
  git -C "$D/repo" rev-parse HEAD~1 > "$D/other-sha"
  cp "$2" "$D/log"
  cat > "$D/curl" <<STUB
#!/usr/bin/env bash
url=""
for a in "\$@"; do case "\$a" in https://*) url="\$a" ;; esac; done
listing() { # listing <run> <status>
  printf '{"workflow_runs":[{"head_sha":"%s","workflow_id":"nix-home.yml","run_number":%s,"status":"%s"},{"head_sha":"%s","workflow_id":"nix-home.yml","run_number":41,"status":"failure"}]}\n200\n' \\
    "\$(cat "$D/sha")" "\$1" "\$2" "\$(cat "$D/sha")"
}
case "\$url" in
  *actions/workflows/nix-home.yml/dispatches)
    echo call >> "$D/dispatch-tries"
    if [ '${3:-}' = nodispatch ]; then printf 'unavailable\n503\n'; exit 0; fi
    echo call >> "$D/dispatches"; echo 2 > "$D/pending"
    printf '\n204\n' ;;
  *actions/tasks*)
    if [ -f "$D/pending" ]; then
      # The second listing, not the first: only a poll past the give-up
      # deadline decides anything, and the first one comes before it.
      if [ '${3:-}' = blip ] && [ ! -f "$D/blipped" ] && [ "\$(cat "$D/pending")" -eq 1 ]; then
        touch "$D/blipped"; printf 'bad gateway\n502\n'; exit 0
      fi
      p=\$(cat "$D/pending")
      if [ "\$p" -gt 0 ]; then
        echo \$((p - 1)) > "$D/pending"
        listing "\$(cat "$D/run-number")" failure; exit 0
      fi
      rm "$D/pending"; echo \$((\$(cat "$D/run-number") + 1)) > "$D/run-number"
    fi
    if [ -f "$D/theirs" ] || [ "\$(wc -l < "$D/dispatches")" -ge $1 ]; then st=success
    elif [ '${3:-}' = cancelled ]; then st=cancelled; else st=failure; fi
    listing "\$(cat "$D/run-number")" "\$st" ;;
  *jobs/0*)     printf 'job_id 7 not found\n200\n' ;;
  *jobs/*logs*)
    [ '${3:-}' != moved ] || git -C "$D/origin.git" update-ref refs/heads/master "\$(cat "$D/other-sha")"
    [ '${3:-}' != theirs ] || { touch "$D/theirs"; echo 43 > "$D/run-number"; }
    cat "$D/log"; printf '200\n' ;;
  *)            printf '{}\n200\n' ;;
esac
STUB
  chmod +x "$D/curl"
}

# Run 252's real log, and that log with one more line in it -- the line a
# real failure would add. Timestamps are the 29-char prefix hms strips.
FIX="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/fixtures/hms-run252-git-503.log"
with_line() { # with_line <text> -> path of the run 252 log plus that line
  { cat "$FIX"; printf '2026-10-24T04:47:29.0000000Z %s\n' "$1"; } > "$D/log-$n"
  echo "$D/log-$n"
}
ndisp() { wc -l < "$D/dispatches"; }
sw() { [ -f "$D/switched" ] && echo y || echo n; }

setup_repo; mktoken good; mkcurl_flaky 1 "$FIX"; mkswitch
out=$(run); rc=$?
if [ -f "$D/switched" ] && [ "$(ndisp)" -eq 1 ] \
   && printf '%s' "$out" | grep -q 'no run for the retry yet' \
   && printf '%s' "$out" | grep -q 'run 43 green'; then
  report "a transient CI failure is re-dispatched, waited for, and the retry switches" ok
else
  report "a transient CI failure is re-dispatched, waited for, and the retry switches" no \
    "dispatches=$(ndisp) switched=$(sw) rc=$rc out=[$out]"
fi

# A real failure followed by a flood of transient lines, ~500KB: well past the
# pipe buffer, so `grep -q` quitting on the first line outside the allowlist
# would SIGPIPE whatever is still writing to it, and under pipefail that 141
# reads as "every line was transient".
big="$D/log-big"
{ cat "$FIX"
  printf '2026-10-24T04:47:29.0000000Z %s\n' 'error: builder for /nix/store/x.drv failed with exit code 1'
  for i in $(seq 5000); do
    printf '2026-10-24T04:47:29.0000000Z %s\n' 'error: RPC failed; HTTP 503 curl 22 The requested URL returned error: 503'
  done; } > "$big"
setup_repo; mktoken good; mkcurl_flaky 1 "$big"; mkswitch
out=$(run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] && [ "$(ndisp)" -eq 0 ]; then
  report "a real failure in a 500KB log of 503s is not retried" ok
else
  report "a real failure in a 500KB log of 503s is not retried" no \
    "dispatches=$(ndisp) switched=$(sw) rc=$rc out=[$(printf '%s' "$out" | tail -5)]"
fi

for c in 'error: builder for /nix/store/x.drv failed with exit code 1' \
         'Error: attic push: cache rejected the upload'; do
  setup_repo; mktoken good; mkcurl_flaky 1 "$(with_line "$c")"; mkswitch
  out=$(run); rc=$?
  if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] && [ "$(ndisp)" -eq 0 ]; then
    report "a 503 alongside '${c%%:*}: ...' is a real failure, not retried" ok
  else
    report "a 503 alongside '${c%%:*}: ...' is a real failure, not retried" no \
      "dispatches=$(ndisp) switched=$(sw) rc=$rc out=[$out]"
  fi
done

# The shipped default, two retries, both spent: the second pass has to skip
# runs 42 *and* 43, and decrement again.
setup_repo; mktoken good; mkcurl_flaky 2 "$FIX"; mkswitch
out=$(HMS_WAIT_SECONDS=40 run); rc=$?
if [ -f "$D/switched" ] && [ "$(ndisp)" -eq 2 ] \
   && printf '%s' "$out" | grep -q 'run 44 green'; then
  report "two transient failures in a row: two retries, then switch" ok
else
  report "two transient failures in a row: two retries, then switch" no \
    "dispatches=$(ndisp) switched=$(sw) rc=$rc out=[$out]"
fi

# And no more than two: the case above holds for any default of two or more,
# and every retry past the second is up to two more hours of waiting.
setup_repo; mktoken good; mkcurl_flaky 3 "$FIX"; mkswitch
out=$(HMS_WAIT_SECONDS=40 run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] && [ "$(ndisp)" -eq 2 ] \
   && printf '%s' "$out" | grep -q 'gave up after 2 retr'; then
  report "three transient failures: the default gives up after two retries" ok
else
  report "three transient failures: the default gives up after two retries" no \
    "dispatches=$(ndisp) switched=$(sw) rc=$rc out=[$out]"
fi

# Bounded: a forge that 503s every run is down, not flaky, and hms must stop --
# and say it gave up on a transient failure, since a bare "Fix CI" over a 503
# log is the very message this whole feature exists to retire.
setup_repo; mktoken good; mkcurl_flaky 99 "$FIX"; mkswitch
out=$(HMS_RETRIES=1 run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] && [ "$(ndisp)" -eq 1 ] \
   && printf '%s' "$out" | grep -q 'gave up after 1 retr'; then
  report "transient retries stop at HMS_RETRIES, and say so" ok
else
  report "transient retries stop at HMS_RETRIES, and say so" no \
    "dispatches=$(ndisp) switched=$(sw) rc=$rc out=[$out]"
fi

# A dispatch builds the branch tip. Once origin/master has moved, a "retry" is
# a build of someone else's commit and an hour waiting on a run for ours that
# will never exist, so hms must hand the failure back instead.
setup_repo; mktoken good; mkcurl_flaky 1 "$FIX" moved; mkswitch
out=$(run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] && [ "$(wc -l < "$D/dispatch-tries")" -eq 0 ] \
   && printf '%s' "$out" | grep -q 'moved on'; then
  report "no retry once origin/master has moved past the failed commit" ok
else
  report "no retry once origin/master has moved past the failed commit" no \
    "tries=$(wc -l < "$D/dispatch-tries") switched=$(sw) rc=$rc out=[$out]"
fi

# Every Linux box's hms waits on the same run, so when it fails transiently
# each of them reaches the dispatch. One retry is enough: a box that finds a
# newer run for this commit already listed adopts it instead of queueing a
# second full build behind it.
setup_repo; mktoken good; mkcurl_flaky 1 "$FIX" theirs; mkswitch
out=$(run); rc=$?
if [ -f "$D/switched" ] && [ "$(wc -l < "$D/dispatch-tries")" -eq 0 ] \
   && printf '%s' "$out" | grep -q 'run 43 green'; then
  report "a retry another box already dispatched is adopted, not doubled" ok
else
  report "a retry another box already dispatched is adopted, not doubled" no \
    "tries=$(wc -l < "$D/dispatch-tries") switched=$(sw) rc=$rc out=[$out]"
fi

# A forge blip while waiting for the retry's run is not "nothing is coming":
# falling through to "switching locally" there is the laptop build hms exists
# to prevent.
# The blip costs a 10s poll on top of the two listings the dispatched run is
# already absent for, so the run shows up near t=30s: past the 21s that the
# default 20s wait leaves, which made this case flaky on whole-second SECONDS.
setup_repo; mktoken good; mkcurl_flaky 1 "$FIX" blip; mkswitch
out=$(HMS_WAIT_SECONDS=60 run); rc=$?
if [ -f "$D/switched" ] && [ -f "$D/blipped" ] \
   && printf '%s' "$out" | grep -q 'run 43 green' \
   && ! printf '%s' "$out" | grep -q 'switching locally'; then
  report "a forge blip during the retry wait keeps waiting" ok
else
  report "a forge blip during the retry wait keeps waiting" no \
    "blipped=$([ -f "$D/blipped" ] && echo y || echo n) switched=$(sw) rc=$rc out=[$out]"
fi

# A run cancelled by hand is someone's decision, not a failure to retry, even
# when the log it left behind is transient-shaped.
setup_repo; mktoken good; mkcurl_flaky 1 "$FIX" cancelled; mkswitch
out=$(run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] && [ "$(wc -l < "$D/dispatch-tries")" -eq 0 ]; then
  report "a cancelled run is not re-dispatched" ok
else
  report "a cancelled run is not re-dispatched" no \
    "tries=$(wc -l < "$D/dispatch-tries") switched=$(sw) rc=$rc out=[$out]"
fi

# A dispatch the forge refuses is the end of it, said once, not a loop.
setup_repo; mktoken good; mkcurl_flaky 1 "$FIX" nodispatch; mkswitch
out=$(run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] && [ "$(wc -l < "$D/dispatch-tries")" -eq 1 ] \
   && printf '%s' "$out" | grep -q 'retry dispatch did not go through'; then
  report "a refused dispatch hands the failure back" ok
else
  report "a refused dispatch hands the failure back" no \
    "tries=$(wc -l < "$D/dispatch-tries") switched=$(sw) rc=$rc out=[$out]"
fi



# --- a strictly behind checkout fast-forwards ------------------------------
# These boxes push to each other constantly, so "another box already pushed" is
# the common case, not the exception. The old script stopped with "pull, then
# re-run" — a second command for what is mechanically a fast-forward — and the
# checkout stayed stale until a human ran it. A stale checkout is not a history
# decision; rebasing still is, and that refusal is unchanged.
#
# Staged per case: setup_repo leaves HEAD one commit ahead of origin/master,
# with that sha in $D/sha. advance_origin pushes that commit (so origin and
# the checkout agree before the split), then adds two commits from a scratch
# clone so origin/master is 2 ahead of the common ancestor — and re-points
# $D/sha at the new tip, because that is the SHA hms waits on after
# fast-forwarding (the stub forge answers for it).
# The variant that skips the first push is for the diverged case: there the
# checkout's commit is local-only on purpose, and pushing it would erase the
# divergence under test.
advance_origin() { # advance_origin: +2 commits on origin/master
  [ -n "${push_skip:-}" ] || git -C "$D/repo" push --quiet origin master
  git init --quiet "$D/other"
  git -C "$D/other" config user.email "t@t"
  git -C "$D/other" config user.name "t"
  git -C "$D/other" fetch --quiet "$D/origin.git" master
  git -C "$D/other" checkout --quiet FETCH_HEAD
  for i in 1 2; do
    echo "z$i" >> "$D/other/f"
    git -C "$D/other" add f
    git -C "$D/other" commit --quiet -m "remote$i"
  done
  git -C "$D/other" push --quiet "$D/origin.git" HEAD:master
  git -C "$D/repo" fetch --quiet origin master
  git -C "$D/repo" rev-parse origin/master > "$D/sha"
}
advance_origin_nopush() { push_skip=1 advance_origin; unset push_skip; }

# 1. Behind on master: fast-forward to the fetched SHA, then wait and switch.
setup_repo; advance_origin; mktoken good; mkcurl good success; mkswitch
out=$(run); rc=$?
expect=$(git -C "$D/repo" rev-parse origin/master)
if [ "$rc" -eq 0 ] && [ -f "$D/switched" ] \
   && [ "$(git -C "$D/repo" rev-parse HEAD)" = "$expect" ] \
   && printf '%s' "$out" | grep -q 'fast-forwarded 2 commit(s)'; then
  report "a strictly behind master checkout fast-forwards to origin/master" ok
else
  report "a strictly behind master checkout fast-forwards to origin/master" no \
    "switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc head=$(git -C "$D/repo" rev-parse HEAD) out=[$out]"
fi

# 2. Diverged: refuse, leave HEAD alone, never touch the switch. Staging:
# reset to what origin holds, add a local-only commit, then advance origin by
# two — ahead>0 and behind>0 is divergence, and neither side contains the other.
setup_repo; git -C "$D/repo" reset --quiet --hard origin/master
git -C "$D/repo" commit --quiet --allow-empty -m local
advance_origin_nopush; mktoken good; mkcurl good success; mkswitch
head_before=$(git -C "$D/repo" rev-parse HEAD)
out=$(run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] \
   && [ "$(git -C "$D/repo" rev-parse HEAD)" = "$head_before" ] \
   && printf '%s' "$out" | grep -q 'rebase, then re-run'; then
  report "a diverged checkout refuses and keeps HEAD" ok
else
  report "a diverged checkout refuses and keeps HEAD" no \
    "switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc head=$(git -C "$D/repo" rev-parse HEAD) out=[$out]"
fi

# 3. Behind on some other branch, or on a detached HEAD: hms never moves a ref
#    it was not asked to move. Word-for-word the old refusal.
check_unmoved_refusal() { # check_unmoved_refusal <name>
  local name="$1" out rc
  head_before=$(git -C "$D/repo" rev-parse HEAD)
  git -C "$D/repo" for-each-ref --format='%(refname) %(objectname)' refs/heads > "$D/refs-before"
  out=$(run); rc=$?
  git -C "$D/repo" for-each-ref --format='%(refname) %(objectname)' refs/heads > "$D/refs-after"
  if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] \
     && [ "$(git -C "$D/repo" rev-parse HEAD)" = "$head_before" ] \
     && cmp -s "$D/refs-before" "$D/refs-after" \
     && printf '%s' "$out" | grep -q 'strictly behind — pull, then re-run' \
     && printf '%s' "$out" | grep -q 'origin/master'; then
    report "$name" ok
  else
    report "$name" no \
      "switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc head=$(git -C "$D/repo" rev-parse HEAD) out=[$out]"
  fi
}

setup_repo; advance_origin
git -C "$D/repo" checkout --quiet -b other
mktoken good; mkcurl good success; mkswitch
check_unmoved_refusal "a behind checkout on a non-master branch is refused, refs untouched"

setup_repo; advance_origin
git -C "$D/repo" checkout --quiet --detach HEAD
mktoken good; mkcurl good success; mkswitch
check_unmoved_refusal "a behind detached HEAD is refused, refs untouched"

# 4. A dirty tree never gets pulled, even when behind: the refusal runs first.
setup_repo; advance_origin
echo dirty > "$D/repo/f"
mktoken good; mkcurl good success; mkswitch
head_before=$(git -C "$D/repo" rev-parse HEAD)
out=$(run); rc=$?
if [ ! -f "$D/switched" ] && [ "$rc" -ne 0 ] \
   && [ "$(git -C "$D/repo" rev-parse HEAD)" = "$head_before" ] \
   && printf '%s' "$out" | grep -q 'working tree dirty'; then
  report "a dirty tree is refused before any fetch-and-decide, even when behind" ok
else
  report "a dirty tree is refused before any fetch-and-decide, even when behind" no \
    "switched=$([ -f "$D/switched" ] && echo y || echo n) rc=$rc head=$(git -C "$D/repo" rev-parse HEAD) out=[$out]"
fi

printf '\n%d/%d passed\n' "$((n - fails))" "$n"
[ "$fails" -eq 0 ]
