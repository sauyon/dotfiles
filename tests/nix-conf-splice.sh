#!/usr/bin/env bash
# Cases for system/nix-conf.sh, the nix.custom.conf half of ./deploy.
#
# Why this exists: nix.custom.conf's builders line is an @NIX_BUILDERS@
# placeholder spliced from dotfiles-private at install time, so the fragment
# can no longer ride the plain etc/ copy -- and the failure modes that creates
# are worth pinning. A raw placeholder reaching /etc would be read by the nix
# daemon as a broken builders spec; an empty or absent value spliced in
# silence would delete the remote-builder delegation outright. Both must be
# refusals, and a refusal must leave the installed file untouched.
#
# Seams, following ../tests/kon-wireguard-profile.sh: SUDO is `${SUDO-sudo}`
# (empty means "write as me"), and every path is an argument.
#
#   ./tests/nix-conf-splice.sh
set -u

cd "$(dirname "$0")/.." || exit 1
# shellcheck source=../system/nix-conf.sh
. ./system/nix-conf.sh || { echo "could not source system/nix-conf.sh" >&2; exit 1; }

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

TEMPLATE=./system/nix-conf/nix.custom.conf

# The privilege seam, verbatim from ../tests/system-secrets.sh: the stub strips
# -o/-g but *records* them, so a case can assert the ownership.
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

# ── the committed template carries no builder line of its own ────────────────

if [ -f "$TEMPLATE" ]; then
  report "template exists" ok
else
  report "template exists" no "$TEMPLATE is missing"
fi

if [ -f "$TEMPLATE" ] && grep -q '^builders = @NIX_BUILDERS@$' "$TEMPLATE"; then
  report "template's builders line is the @NIX_BUILDERS@ placeholder" ok
else
  report "template's builders line is the @NIX_BUILDERS@ placeholder" no "builders line is not a splice placeholder"
fi

if [ -f "$TEMPLATE" ] && grep -qE '^builders = [^@]' "$TEMPLATE"; then
  report "template carries no literal builders line" no "a builder address is committed"
else
  report "template carries no literal builders line" ok
fi

if [ -f "$TEMPLATE" ] && grep -q 'builders-use-substitutes = true' "$TEMPLATE"; then
  report "template keeps builders-use-substitutes" ok
else
  report "template keeps builders-use-substitutes" no "the flag next to the spliced line is gone"
fi

# ── install_nix_conf: splice the line, never invent one ──────────────────────

FAKE_BUILDERS="ssh-ng://fakeuser@builder.example.test aarch64-darwin /etc/nix/fake-key 4 1 - - ZmFrZQ=="
echo "$FAKE_BUILDERS" > "$D/nix-builders"

dest="$D/nix.custom.conf"
out=$(install_nix_conf "$TEMPLATE" "$dest" "$D/nix-builders" 2>&1); rc=$?

if [ "$rc" -eq 0 ]; then
  report "install over an absent destination succeeds" ok
else
  report "install over an absent destination succeeds" no "exit $rc: $out"
fi

if grep -q "^builders = $FAKE_BUILDERS$" "$dest"; then
  report "the installed fragment takes the builders file's value" ok
else
  report "the installed fragment takes the builders file's value" no "builders line was not spliced from the file"
fi

if grep -q '@NIX_BUILDERS@' "$dest"; then
  report "no placeholder survives the install" no "an @...@ placeholder reached the destination"
else
  report "no placeholder survives the install" ok
fi

if grep -q 'extra-substituters = http://attic.attic.svc.cluster.local/kube' "$dest"; then
  report "the rest of the fragment is carried through" ok
else
  report "the rest of the fragment is carried through" no "the install lost the substituter block"
fi

mode=$(stat -c '%a' "$dest")
if [ "$mode" = 644 ]; then
  report "the installed fragment is mode 644" ok
else
  report "the installed fragment is mode 644" no "mode is $mode; the daemon must read it"
fi

if grep -q 'o root' "$D/ownflags" 2>/dev/null && grep -q 'g root' "$D/ownflags" 2>/dev/null; then
  report "the installed fragment is owned root:root" ok
else
  report "the installed fragment is owned root:root" no "install was not given -o root -g root"
fi

# ── a missing or empty value is refused, and the target is left alone ─────────

dest2="$D/live.nix.custom.conf"
cat "$TEMPLATE" > "$dest2"
before=$(cat "$dest2")
install_nix_conf "$TEMPLATE" "$dest2" "$D/absent-builders" >/dev/null 2>&1; rc2=$?

if [ "$rc2" -ne 0 ]; then
  report "a missing builders file is refused" ok
else
  report "a missing builders file is refused" no "exit 0; the placeholder line was installed raw"
fi

if [ "$(cat "$dest2")" = "$before" ]; then
  report "the refused destination is left untouched" ok
else
  report "the refused destination is left untouched" no "the installed fragment was modified or truncated"
fi

: > "$D/empty-builders"
install_nix_conf "$TEMPLATE" "$dest2" "$D/empty-builders" >/dev/null 2>&1; rc3=$?
if [ "$rc3" -ne 0 ]; then
  report "an empty builders file is refused" ok
else
  report "an empty builders file is refused" no "exit 0; an empty builders line was spliced"
fi

printf '\n%d case(s), %d failure(s)\n' "$n" "$fails"
[ "$fails" -eq 0 ]
