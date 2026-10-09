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
  host=$(cat /etc/hostname)
  drv=$(nix eval --raw ".#homeConfigurations.$host.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: p.name or "" == "hms") ps)).drvPath') \
    || { echo "could not evaluate hms for $host" >&2; exit 1; }
  SCRIPT="$(nix-store --realise "$drv" | tail -1)/bin/hms"
  echo "testing $SCRIPT"
fi
[ -x "$SCRIPT" ] || { echo "not executable: $SCRIPT" >&2; exit 1; }

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

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
  env -i PATH="$PATH" HOME="$HOME" \
    HMS_REPO="$D/repo" HMS_CURL="$D/curl" HMS_TOKEN_CMD="$D/token-cmd" \
    HMS_SWITCH_CMD="$D/switch" HMS_WAIT_SECONDS=20 HMS_FIRST_RUN_SECONDS=1 \
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

printf '\n%d/%d passed\n' "$((n - fails))" "$n"
[ "$fails" -eq 0 ]
