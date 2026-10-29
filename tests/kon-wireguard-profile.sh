#!/usr/bin/env bash
# Cases for system/network.sh, the NetworkManager half of ./deploy.
#
# Why this exists: the Kon WireGuard profile was hand-made in NetworkManager and
# lived nowhere. It therefore had no `persistent-keepalive`, and nothing on the
# host reacted to a default-route change, so a tunnel established on one network
# stayed `activated` and dead across two SSID changes -- TX climbing, RX frozen,
# tx_dropped climbing, until it was deactivated and reactivated by hand.
#
# Bringing the profile into the repo means the repo owns every field EXCEPT the
# two values that are per-device or per-site: the WireGuard private key and the
# peer's public key. Those are spliced from whatever is already installed. That
# is the whole risk here -- a careless install clobbers a working tunnel's key
# and leaves a profile that looks provisioned -- so most of these cases are
# about what happens when the key is missing or the render fails.
#
# The ordering lesson is ../tests/system-secrets.sh case 2's, restated: build
# the content, check it, and only then let `install` create the destination.
# A producer that fails inside the pipeline truncates the destination first and
# exits 0 having written nothing.
#
# Seams, following ../system/pacman.sh: SUDO is `${SUDO-sudo}` (empty means
# "write as me"), NMCLI stands in for the real binary.
#
#   ./tests/kon-wireguard-profile.sh
set -u

cd "$(dirname "$0")/.." || exit 1
# shellcheck source=../system/network.sh
. ./system/network.sh || { echo "could not source system/network.sh" >&2; exit 1; }

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

TEMPLATE=./system/kon-wireguard/Kon-shiori.nmconnection

# The privilege seam, verbatim from ../tests/system-secrets.sh: these cases run
# as a normal user who cannot chown to root, so the stub strips -o/-g but
# *records* them, which is what lets a case assert the ownership rather than let
# it be silently deleted from the installer.
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

# A destination profile as a live host already has one: a real private key, a
# real peer section, and none of the fields the repo is taking over.
stage_live() { # stage_live <dest> <privkey> <peerpub>
  cat > "$1" <<LIVE
[connection]
id=Kon-shiori
type=wireguard
interface-name=Kon-shiori

[wireguard]
private-key=$2

[wireguard-peer.$3]
allowed-ips=0.0.0.0/0;::/0;
endpoint=203.0.113.9:51820

[ipv4]
method=manual
address1=10.9.0.2/32
LIVE
}

# Two values that must never reach the repo. Shaped like the real ones (44-char
# base64) so a case that greps for key material cannot pass by accident.
FAKE_PRIV="aGVsbG8gdGhpcyBpcyBub3QgYSByZWFsIGtleSBva2F5MD0="
FAKE_PEER="cGVlciBwdWJsaWMga2V5IGFsc28gbm90IHJlYWwgaGVyZTA9"

# ── the committed template carries no secrets ───────────────────────────────

if [ -f "$TEMPLATE" ]; then
  report "template exists" ok
else
  report "template exists" no "$TEMPLATE is missing"
fi

if [ -f "$TEMPLATE" ] && grep -qE '^private-key=.+' "$TEMPLATE"; then
  report "template carries no private key" no "a populated private-key= line is committed"
else
  report "template carries no private key" ok
fi

# The endpoint is not the template's to name at all: its DNS name carries the
# home WAN address, and this repo is public. The value arrives from a file at
# install time (see install_wg_profile), so the committed template must carry
# the placeholder and no literal. A bare IPv4 is refused for the same reason
# the name is: a dynamic residential lease is exactly what the spliced name
# survives and a committed address does not.
if [ -f "$TEMPLATE" ] && grep -q '^endpoint=@WG_ENDPOINT@$' "$TEMPLATE"; then
  report "template's endpoint is the @WG_ENDPOINT@ placeholder" ok
else
  report "template's endpoint is the @WG_ENDPOINT@ placeholder" no "endpoint is not a splice placeholder"
fi

if [ -f "$TEMPLATE" ] && grep -qE '^endpoint=[^@]' "$TEMPLATE"; then
  report "template carries no literal endpoint" no "an endpoint value is committed"
else
  report "template carries no literal endpoint" ok
fi

if [ -f "$TEMPLATE" ] && grep -qE 'endpoint=[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+:' "$TEMPLATE"; then
  report "endpoint is a name, not a literal IP" no "a bare IPv4 endpoint is committed"
else
  report "endpoint is a name, not a literal IP" ok
fi

if [ -f "$TEMPLATE" ] && grep -q 'persistent-keepalive=25' "$TEMPLATE"; then
  report "template sets persistent-keepalive" ok
else
  report "template sets persistent-keepalive" no "no persistent-keepalive=25; NAT mapping will age out"
fi

# No resolver over the tunnel. NetworkManager writes a profile's dns= into
# /etc/resolv.conf ahead of the wifi's, and glibc tries them in order, so while
# the tunnel carries nothing every uncached lookup on the host waits out a 6s
# timeout per tunnel server before reaching one that answers -- measured at
# 8-17s for gcloud's metadata.google.internal probe, which stalled every new
# shell's first prompt. The fleet names it was kept for resolve publicly.
if [ -f "$TEMPLATE" ] && grep -qE '^dns=' "$TEMPLATE"; then
  report "template puts no tunnel resolver in resolv.conf" no "$(grep -E '^dns=' "$TEMPLATE" | tr '\n' ' ')"
else
  report "template puts no tunnel resolver in resolv.conf" ok
fi

if [ -f "$TEMPLATE" ] && grep -q '^dns-search=ko.ag' "$TEMPLATE"; then
  report "template sets the ko.ag search domain" ok
else
  report "template sets the ko.ag search domain" no "dns-search is not ko.ag"
fi


FAKE_ENDPOINT="wg.example.test:51820"
echo "$FAKE_ENDPOINT" > "$D/wg-endpoint"

# ── install_wg_profile: splice the per-device values, never invent them ─────

dest="$D/live.nmconnection"
stage_live "$dest" "$FAKE_PRIV" "$FAKE_PEER"
out=$(install_wg_profile "$TEMPLATE" "$dest" "$D/wg-endpoint" 2>&1); rc=$?

if [ "$rc" -eq 0 ]; then
  report "install over a live profile succeeds" ok
else
  report "install over a live profile succeeds" no "exit $rc: $out"
fi

if grep -qF "private-key=$FAKE_PRIV" "$dest"; then
  report "the live private key survives the install" ok
else
  report "the live private key survives the install" no "key was dropped or replaced"
fi

if grep -qF "[wireguard-peer.$FAKE_PEER]" "$dest"; then
  report "the live peer public key survives the install" ok
else
  report "the live peer public key survives the install" no "peer section lost its key"
fi

if grep -q 'persistent-keepalive=25' "$dest"; then
  report "the installed profile gains the keepalive" ok
else
  report "the installed profile gains the keepalive" no "keepalive did not reach the destination"
fi

if grep -q "^endpoint=$FAKE_ENDPOINT" "$dest"; then
  report "the installed profile takes the endpoint file's value" ok
else
  report "the installed profile takes the endpoint file's value" no "endpoint was not spliced from the file"
fi

if grep -q '@WG_ENDPOINT@\|@PEER_PUBKEY@' "$dest"; then
  report "no placeholder survives the install" no "a @...@ placeholder reached the destination"
else
  report "no placeholder survives the install" ok
fi

mode=$(stat -c '%a' "$dest")
if [ "$mode" = 600 ]; then
  report "the installed profile is mode 600" ok
else
  report "the installed profile is mode 600" no "mode is $mode; NM ignores a world-readable keyfile"
fi

if grep -q 'o root' "$D/ownflags" 2>/dev/null && grep -q 'g root' "$D/ownflags" 2>/dev/null; then
  report "the installed profile is owned root:root" ok
else
  report "the installed profile is owned root:root" no "install was not given -o root -g root"
fi

# Nothing the installer prints may carry key material, or a deploy log becomes a
# place secrets live.
if printf '%s' "$out" | grep -qF "$FAKE_PRIV"; then
  report "the installer never prints the private key" no "the key appeared on stdout/stderr"
else
  report "the installer never prints the private key" ok
fi

# ── the destination must survive a failure, not be truncated by one ─────────

dest2="$D/nokey.nmconnection"
cat > "$dest2" <<'NOKEY'
[connection]
id=Kon-shiori
type=wireguard
NOKEY
before=$(cat "$dest2")
install_wg_profile "$TEMPLATE" "$dest2" "$D/wg-endpoint" >/dev/null 2>&1; rc2=$?

if [ "$rc2" -ne 0 ]; then
  report "a destination with no private key is refused" ok
else
  report "a destination with no private key is refused" no "exit 0; a keyless profile was written"
fi

if [ "$(cat "$dest2")" = "$before" ]; then
  report "the refused destination is left untouched" ok
else
  report "the refused destination is left untouched" no "the existing profile was modified or truncated"
fi

dest3="$D/absent.nmconnection"
out3=$(install_wg_profile "$TEMPLATE" "$dest3" "$D/wg-endpoint" 2>&1); rc3=$?
if [ "$rc3" -ne 0 ] && [ ! -e "$dest3" ]; then
  report "an absent destination is refused without creating a stub" ok
else
  report "an absent destination is refused without creating a stub" no "exit $rc3, exists=$([ -e "$dest3" ] && echo yes || echo no)"
fi

if printf '%s' "$out3" | grep -q 'wg genkey\|private key'; then
  report "the refusal says how to seed the key" ok
else
  report "the refusal says how to seed the key" no "the error does not tell the operator what to do"
fi

# ── the endpoint file is load-bearing: absent or empty is refused ────────────

dest4="$D/noendpoint.nmconnection"
stage_live "$dest4" "$FAKE_PRIV" "$FAKE_PEER"
before=$(cat "$dest4")
install_wg_profile "$TEMPLATE" "$dest4" "$D/absent-endpoint" >/dev/null 2>&1; rc4=$?

if [ "$rc4" -ne 0 ]; then
  report "a missing endpoint file is refused" ok
else
  report "a missing endpoint file is refused" no "exit 0; endpoint spliced from nothing"
fi

if [ "$(cat "$dest4")" = "$before" ]; then
  report "the refused endpoint destination is untouched" ok
else
  report "the refused endpoint destination is untouched" no "the existing profile was modified or truncated"
fi

: > "$D/empty-endpoint"
install_wg_profile "$TEMPLATE" "$dest4" "$D/empty-endpoint" >/dev/null 2>&1; rc5=$?
if [ "$rc5" -ne 0 ]; then
  report "an empty endpoint file is refused" ok
else
  report "an empty endpoint file is refused" no "exit 0; an empty endpoint was spliced"
fi

# ── the dispatcher hook must not bounce the tunnel it is watching ───────────

HOOK=./system/etc/NetworkManager/dispatcher.d/90-kon-wg-rebind

if [ -x "$HOOK" ]; then
  report "the dispatcher hook is executable" ok
else
  report "the dispatcher hook is executable" no "$HOOK is missing or not +x (NM skips it)"
fi

# Answers the state query with `activated`, so the "only rebind a tunnel that
# is already up" guard is exercised rather than short-circuited.
cat > "$D/nmcli" <<STUB
#!/bin/sh
echo "\$*" >> "$D/nmcli-calls"
echo activated
STUB
chmod +x "$D/nmcli"

run_hook() { # run_hook <iface> <action>
  : > "$D/nmcli-calls"
  NMCLI="$D/nmcli" CONNECTION_ID="${3-Hacker Dojo Free}" \
    "$HOOK" "$1" "$2" >/dev/null 2>&1
  cat "$D/nmcli-calls" 2>/dev/null
}

if [ -x "$HOOK" ]; then
  # The loop this guards against: the hook reactivates Kon-shiori, which fires
  # another `up` -- this time for Kon-shiori itself -- and if the hook acted on
  # that too it would reactivate forever.
  if [ -z "$(run_hook Kon-shiori up)" ]; then
    report "an event on the WG device itself is ignored" ok
  else
    report "an event on the WG device itself is ignored" no "the hook would reactivate itself in a loop"
  fi

  if printf '%s' "$(run_hook wlp0s20f3 up)" | grep -q '^con up Kon-shiori$'; then
    report "a physical-link up reactivates the tunnel" ok
  else
    report "a physical-link up reactivates the tunnel" no "the hook did nothing on the event that matters"
  fi

  if [ -z "$(run_hook wlp0s20f3 pre-down)" ]; then
    report "an unrelated action is ignored" ok
  else
    report "an unrelated action is ignored" no "the hook acts on actions it should not"
  fi
fi

printf '\n%d case(s), %d failure(s)\n' "$n" "$fails"
[ "$fails" -eq 0 ]
