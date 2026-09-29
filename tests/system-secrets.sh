#!/usr/bin/env bash
# Cases for system/secrets.sh, the sops half of ./deploy.
#
# Two bugs prompted these, and shiori found both at once.
#
# 1. ./deploy hardcoded $HOME/.config/sops/gcp-key.json as the GCP credential.
#    shiori is the sole WIF host, and the whole point of that migration is that
#    it has no decryption key on disk (home.nix's wifHosts comment says so).
#    So sops could never decrypt there. home.nix already decides this per host,
#    in `sops.environment`, so the fix is to read that rather than re-derive a
#    copy of the host list in bash -- which is what these cases pin.
#
#    The subtlety: that attrset also carries PATH="", which sops-nix sets for
#    sops-install-secrets' own sandboxed activation. Exporting it verbatim would
#    blank ./deploy's PATH, so it has to be dropped, and a case says so.
#
# 2. The install pattern was `sops ... | sed ... | sudo install /dev/stdin DEST`.
#    All three stages run concurrently, so install created and truncated DEST
#    before sops had finished failing, then exited 0 having written nothing. A
#    failed deploy therefore destroyed a working netrc and left a 0-byte file
#    that looks provisioned -- and `set -e` aborted the script there, so every
#    later step silently never ran. That is exactly how shiori ended up with a
#    0-byte /etc/determinate/netrc.custom and no /etc/nix/netrc at all.
#
# Seams, following ../system/pacman.sh: SUDO is `${SUDO-sudo}` (empty means
# "write as me"), and SOPS / NIX / JQ stand in for the real binaries.
#
#   ./tests/system-secrets.sh
set -u

cd "$(dirname "$0")/.." || exit 1
# shellcheck source=../system/secrets.sh
. ./system/secrets.sh || { echo "could not source system/secrets.sh" >&2; exit 1; }

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

# The privilege seam. Not SUDO="" -- these cases run as a normal user, who
# cannot chown to root, so a bare `install -o root -g root` fails for a reason
# that has nothing to do with what is being tested.
#
# The stub therefore strips the ownership flags before running install, but
# *records* them first. Dropping them silently would mean `-o root -g root`
# could be deleted from install_secret and every case would still pass, while
# the attic token and the mari SSH private key landed owned by whoever ran
# deploy. Recording turns the ownership into something a case can assert.
cat > "$D/sudo" <<STUB
#!/usr/bin/env bash
out=()
while [ \$# -gt 0 ]; do
  case "\$1" in
    -o|-g) echo "\$1 \$2" >> "$D/ownflags"; shift 2 ;;
    *) out+=("\$1"); shift ;;
  esac
done
exec "\${out[@]}"
STUB
chmod +x "$D/sudo"
export SUDO="$D/sudo"

report() { # report <name> <ok?> <detail>
  n=$((n + 1))
  if [ "$2" = ok ]; then
    printf 'ok %d - %s\n' "$n" "$1"
  else
    printf 'FAIL %d - %s\n      %s\n' "$n" "$1" "$3"
    fails=$((fails + 1))
  fi
}

# A stub `nix` that answers `eval` with whatever JSON the case staged.
mknix() { printf '%s' "$1" > "$D/nix-json"
  cat > "$D/nix" <<STUB
#!/bin/sh
cat "$D/nix-json"
STUB
  chmod +x "$D/nix"; export NIX="$D/nix"; }

# A stub `sops` that emits the staged value and exits with the staged code.
mksops() { # mksops <exit> <stdout>
  printf '%s' "$2" > "$D/sops-out"
  cat > "$D/sops" <<STUB
#!/bin/sh
cat "$D/sops-out"
exit $1
STUB
  chmod +x "$D/sops"; export SOPS="$D/sops"; }

# ── sops_env_for: read the flake's decision, do not re-derive it ─────────────

mknix '{"GOOGLE_APPLICATION_CREDENTIALS":"/nix/store/xxx-wif-hosts.json","GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES":"1","PATH":""}'
got=$(sops_env_for . shiori | sort | tr '\n' ' ')
want="GOOGLE_APPLICATION_CREDENTIALS=/nix/store/xxx-wif-hosts.json GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES=1 "
if [ "$got" = "$want" ]; then
  report "a WIF host's credential config comes through" ok
else
  report "a WIF host's credential config comes through" no "want: [$want] got: [$got]"
fi

# PATH="" is sops-nix's, for its own sandboxed activation. Inheriting it here
# would blank ./deploy's PATH and break every command after this point.
if ! sops_env_for . shiori | grep -q '^PATH='; then
  report "PATH is dropped, not inherited" ok
else
  report "PATH is dropped, not inherited" no "PATH leaked into the env pairs"
fi

mknix '{"GOOGLE_APPLICATION_CREDENTIALS":"/home/u/.config/sops/gcp-key.json"}'
got=$(sops_env_for . utsuho | tr '\n' ' ')
if [ "$got" = "GOOGLE_APPLICATION_CREDENTIALS=/home/u/.config/sops/gcp-key.json " ]; then
  report "a non-WIF host still gets the service-account key path" ok
else
  report "a non-WIF host still gets the service-account key path" no "got: [$got]"
fi

# A failing `nix eval` writes its diagnostics to stderr and nothing to stdout,
# so sops_env_for yields no pairs. ./deploy's length check is what turns that
# into an error -- there is deliberately no `||` on the mapfile, because
# mapfile's status reflects mapfile and not the process substitution (the same
# trap ../system/deploy's pacman_wants comment already warns about).
cat > "$D/nix" <<'STUB'
#!/bin/sh
echo "error: flake output attribute does not exist" >&2
exit 1
STUB
chmod +x "$D/nix"; export NIX="$D/nix"
got=$(sops_env_for . nosuchhost 2>/dev/null)
if [ -z "$got" ]; then
  report "a failing nix eval yields no pairs, not junk" ok
else
  report "a failing nix eval yields no pairs, not junk" no "got: [$got]"
fi

# ── sops_env_select: the override branch must not widen anything ─────────────
#
# GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES is not a compatibility toggle. An
# external_account credential config carries credential_source.executable.command,
# an arbitrary command line, and that flag is the interlock Google ships so the
# auth library will not run it. Default-off means a credential config is data;
# on means it is code.
#
# home.nix may set it because the path it names is a writeText store path --
# immutable, and its own comment says so. An operator-supplied path carries no
# such guarantee, so the override branch must not turn the interlock on for it.
mknix '{"GOOGLE_APPLICATION_CREDENTIALS":"/nix/store/xxx-wif-hosts.json","GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES":"1","PATH":""}'
got=$(GOOGLE_APPLICATION_CREDENTIALS=/tmp/attacker.json sops_env_select . shiori | sort | tr '\n' ' ')
if [ "$got" = "GOOGLE_APPLICATION_CREDENTIALS=/tmp/attacker.json " ]; then
  report "an override does NOT enable ALLOW_EXECUTABLES" ok
else
  report "an override does NOT enable ALLOW_EXECUTABLES" no "got: [$got]"
fi

# Explicitly asking for it is still honoured -- that is the operator deciding,
# which is the whole difference.
got=$(GOOGLE_APPLICATION_CREDENTIALS=/tmp/mine.json GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES=1 \
      sops_env_select . shiori | sort | tr '\n' ' ')
if [ "$got" = "GOOGLE_APPLICATION_CREDENTIALS=/tmp/mine.json GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES=1 " ]; then
  report "an explicitly set ALLOW_EXECUTABLES is passed through" ok
else
  report "an explicitly set ALLOW_EXECUTABLES is passed through" no "got: [$got]"
fi

# With no override it is the flake's answer, unchanged.
got=$(sops_env_select . shiori | sort | tr '\n' ' ')
if [ "$got" = "GOOGLE_APPLICATION_CREDENTIALS=/nix/store/xxx-wif-hosts.json GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES=1 " ]; then
  report "with no override, the flake's pair comes through intact" ok
else
  report "with no override, the flake's pair comes through intact" no "got: [$got]"
fi

# ── install_secret: decrypt first, install only on success ───────────────────

mksops 0 'tok123'
rm -f "$D/dest" "$D/ownflags"
install_secret "$D/secrets.yaml" atticPullToken "$D/dest" 'machine attic.ko.ag password '; rc=$?
got=$(cat "$D/dest" 2>/dev/null)
if [ "$rc" -eq 0 ] && [ "$got" = "machine attic.ko.ag password tok123" ]; then
  report "a good decrypt installs the prefixed line" ok
else
  report "a good decrypt installs the prefixed line" no "rc=$rc content=[$got]"
fi

# Ownership, from what the stub recorded. Half of "not readable by other users
# on this box" is the mode; this is the other half.
own=$(sort -u "$D/ownflags" 2>/dev/null | tr '\n' ',')
if [ "$own" = "-g root,-o root," ]; then
  report "the install is root-owned" ok
else
  report "the install is root-owned" no "recorded ownership flags: [$own] want -o root -g root"
fi

# The mode is asserted separately because losing it is silent and expensive:
# without -m600 these land 0644, which publishes the attic pull token and the
# mari SSH private key to every user on the box. The SUDO stub strips only
# -o/-g, so the mode reaches the real `install` and this is a true check.
mode=$(stat -c %a "$D/dest" 2>/dev/null)
if [ "$mode" = 600 ]; then
  report "the installed file is 0600" ok
else
  report "the installed file is 0600" no "mode=$mode want 600"
fi

# Overwrite, not create -- which is the path a real host takes on every deploy
# after the first. install writes into the existing inode and chmods afterwards,
# so the mode has to be asserted here too and not inferred from the create case.
printf 'stale\n' > "$D/dest"; chmod 644 "$D/dest"
install_secret "$D/secrets.yaml" atticPullToken "$D/dest" 'machine attic.ko.ag password '
mode=$(stat -c %a "$D/dest" 2>/dev/null)
if [ "$mode" = 600 ]; then
  report "overwriting a 0644 file still lands 0600" ok
else
  report "overwriting a 0644 file still lands 0600" no "mode=$mode want 600"
fi

# The regression. The old pipeline truncated DEST before sops had failed.
#
# The stub emits output *and* fails, deliberately. Staging an empty stdout here
# would let the `[ -z "$val" ]` guard below carry the case, so it would still
# pass with the exit-status check deleted entirely -- pinning nothing.
mksops 1 'PARTIAL'
printf 'machine attic.ko.ag password PREVIOUSLYWORKING\n' > "$D/dest"
install_secret "$D/secrets.yaml" atticPullToken "$D/dest" 'machine attic.ko.ag password '; rc=$?
got=$(cat "$D/dest" 2>/dev/null)
if [ "$rc" -ne 0 ] && [ "$got" = "machine attic.ko.ag password PREVIOUSLYWORKING" ]; then
  report "a failed decrypt leaves an existing file untouched" ok
else
  report "a failed decrypt leaves an existing file untouched" no \
    "rc=$rc content=[$got] (want nonzero rc and the original line)"
fi

# A decrypt that succeeds but yields nothing is the 0-byte-netrc shape: it must
# not read as provisioned either.
mksops 0 ''
printf 'machine attic.ko.ag password PREVIOUSLYWORKING\n' > "$D/dest"
install_secret "$D/secrets.yaml" atticPullToken "$D/dest" 'machine attic.ko.ag password '; rc=$?
got=$(cat "$D/dest" 2>/dev/null)
if [ "$rc" -ne 0 ] && [ "$got" = "machine attic.ko.ag password PREVIOUSLYWORKING" ]; then
  report "an empty decrypt is a failure, not a 0-byte install" ok
else
  report "an empty decrypt is a failure, not a 0-byte install" no \
    "rc=$rc content=[$got] (want nonzero rc and the original line)"
fi

# A multi-line secret (the mari builder key is one) survives intact, and with no
# prefix argument nothing is prepended.
mksops 0 '-----BEGIN KEY-----
line2
-----END KEY-----'
rm -f "$D/dest"
install_secret "$D/secrets.yaml" mariBuilderKey "$D/dest"; rc=$?
if [ "$rc" -eq 0 ] && [ "$(wc -l < "$D/dest")" -eq 3 ] && head -1 "$D/dest" | grep -q '^-----BEGIN KEY-----$'; then
  report "a multi-line secret installs unprefixed and intact" ok
else
  report "a multi-line secret installs unprefixed and intact" no \
    "rc=$rc lines=$(wc -l < "$D/dest" 2>/dev/null)"
fi

# A prefix is a netrc-entry prefix, and a netrc entry is one line. If a secret
# that takes one ever became multi-line, prefixing only the first would write a
# file whose second line is a bare token -- syntactically fine to curl, and
# wrong. Refuse instead of silently producing it.
mksops 0 'line-one
line-two'
printf 'machine attic.ko.ag password PREVIOUSLYWORKING\n' > "$D/dest"
install_secret "$D/secrets.yaml" atticPullToken "$D/dest" 'machine attic.ko.ag password '; rc=$?
got=$(cat "$D/dest" 2>/dev/null)
if [ "$rc" -ne 0 ] && [ "$got" = "machine attic.ko.ag password PREVIOUSLYWORKING" ]; then
  report "a multi-line secret is refused where a prefix is given" ok
else
  report "a multi-line secret is refused where a prefix is given" no \
    "rc=$rc content=[$got]"
fi

# The plaintext must never reach a child process's argv: /proc/<pid>/cmdline is
# world-readable, so a secret passed as an argument is a secret published to
# every user on the box for the lifetime of the call.
mksops 0 'SUPERSECRETVALUE'
rm -f "$D/dest" "$D/argv"
cat > "$D/fakesudo" <<STUB
#!/usr/bin/env bash
echo "\$@" >> "$D/argv"
out=()
while [ \$# -gt 0 ]; do
  case "\$1" in
    -o|-g) shift 2 ;;
    *) out+=("\$1"); shift ;;
  esac
done
exec "\${out[@]}"
STUB
chmod +x "$D/fakesudo"
SUDO="$D/fakesudo" install_secret "$D/secrets.yaml" atticPullToken "$D/dest" 'machine attic.ko.ag password '
if [ -s "$D/argv" ] && ! grep -q SUPERSECRETVALUE "$D/argv"; then
  report "the plaintext never appears in a child's argv" ok
else
  report "the plaintext never appears in a child's argv" no \
    "argv=[$(cat "$D/argv" 2>/dev/null)] (empty means the install never ran, so this proved nothing)"
fi

# The decrypt-then-install order is what puts the plaintext in a shell variable,
# and `set -x` traces assignments and builtin arguments. `bash -x system/deploy`
# is how this script gets debugged and its output gets pasted into commit
# messages and issues in a public repo, so an xtrace run must not print the
# token or the mari SSH private key. The old pipeline never held the plaintext
# in the shell and so never had this exposure; keeping the new shape means
# closing it deliberately.
mksops 0 'CANARYVALUE'
rm -f "$D/dest"
traced=$( { set -x; install_secret "$D/secrets.yaml" atticPullToken "$D/dest" 'machine attic.ko.ag password '; set +x; } 2>&1 )
# The non-emptiness guard is not decoration: if the subshell ever stopped
# tracing, or a later edit stopped capturing stderr, an empty $traced would let
# this case pass having proved nothing. Same standard as the argv case above.
# The failure message prints a count, never $traced -- the obvious version of
# this assertion dumps the secret into the test output on failure.
if [ -n "$traced" ] && ! printf '%s' "$traced" | grep -q CANARYVALUE; then
  report "the plaintext does not reach the xtrace stream" ok
else
  report "the plaintext does not reach the xtrace stream" no \
    "$(printf '%s' "$traced" | grep -c CANARYVALUE) traced line(s) contain the secret; \
$(printf '%s' "$traced" | wc -l) traced line(s) total (0 means the trace was never captured)"
fi

# ── ./deploy's glue ──────────────────────────────────────────────────────────
#
# Static, because driving ./deploy for real needs sudo and writes to /etc. It
# still pins a bug that shipped: $host used to be assigned inside the else arm
# of the SKIP_HOST_PACKAGES guard, while the sops block below reads it
# unconditionally. Under `set -euo pipefail` that made
# `SKIP_HOST_PACKAGES=1 ./system/deploy` -- the documented way to re-run the
# non-package half -- abort on "host: unbound variable" after the etc/ copy and
# before every credential it exists to install.
# The check is "assigned before the guard opens", not "assigned before first
# use" and not "unindented": both of those pass with the bug present, because
# the old assignment sat immediately above its first use and that else arm is
# written flush left. Only the position relative to the guard distinguishes
# "always set" from "set unless SKIP_HOST_PACKAGES".
# mise's `sops` task consumes sops_env_for too, and mise renders a task's script
# through a Tera template before bash sees it. A bash array-length expansion
# begins with a sequence Tera reads as a comment opener, which fails the task at
# validation with "Closing comment tag not found" -- and nothing about editing
# the TOML tells you, because `mise tasks` still lists it happily. The task only
# breaks for whoever next tries to edit secrets. Running it is the only check
# that means anything.
#
# --version, not a decrypt: this asserts the task is well-formed and that the
# credential lookup inside it runs, without needing KMS.
if command -v mise >/dev/null 2>&1; then
  if mise run sops -- --version >/dev/null 2>&1; then
    report "mise's sops task is well-formed and runs" ok
  else
    report "mise's sops task is well-formed and runs" no \
      "$(mise run sops -- --version 2>&1 | grep -i 'ERROR' | head -2)"
  fi
else
  report "mise's sops task is well-formed and runs" ok  # no mise here; not this suite's business
fi

# The xtrace suppression lives entirely in the install_secret wrapper, so a
# later edit that calls _install_secret directly would reinstate the leak and
# every case above would still pass -- they all go through the wrapper. The `_`
# prefix is a naming convention, not a guard; this is the guard.
if [ "$(grep -c '_install_secret' system/deploy)" -eq 0 ]; then
  report "deploy calls install_secret, never _install_secret directly" ok
else
  report "deploy calls install_secret, never _install_secret directly" no \
    "$(grep -n '_install_secret' system/deploy | head -3)"
fi

assign=$(grep -n '^host=' system/deploy | head -1 | cut -d: -f1)
guard=$(grep -n 'SKIP_HOST_PACKAGES:-' system/deploy | head -1 | cut -d: -f1)
if [ -n "$assign" ] && [ -n "$guard" ] && [ "$assign" -lt "$guard" ]; then
  report "deploy assigns \$host before the SKIP_HOST_PACKAGES guard" ok
else
  report "deploy assigns \$host before the SKIP_HOST_PACKAGES guard" no \
    "assigned at line ${assign:-none}, guard opens at line ${guard:-none}"
fi

printf '\n%d/%d passed\n' "$((n - fails))" "$n"
[ "$fails" -eq 0 ]
