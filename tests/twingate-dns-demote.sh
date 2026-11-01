#!/usr/bin/env bash
# Cases for system/etc/NetworkManager/dispatcher.d/89-twingate-dns-demote, the
# hook that keeps `twingate start` from taking system DNS down with it.
#
# The 2026-10-30 outage mechanism, pinned by the outage journal rather than
# assumed: twingated activates a NetworkManager connection carrying a NEGATIVE
# dns-priority, which NM treats as exclusive ("in presence of at least one
# negative priority, only DNS servers from the connection with the lowest
# value will be used"), registers sdwan0 as resolved's default-route link with
# `~.` search domains, and NM rewrites /etc/resolv.conf to Twingate's relays
# (100.95.0.251-254). tailscaled forwards every non-tailnet query to whatever
# resolv.conf lists, adopts the relays, and every lookup dies while the
# Twingate tunnel is dead -- the exact session that motivates the hook.
#
# The hook demotes the connection to a large positive priority (ordering, not
# excluding), clears the `~.` search domains, reapplies so NM commits the DNS
# change at once, and pins resolved's default-route off the link. Everything
# it does must survive a partial failure -- a hook that cannot fix the state
# must not become a new way to break the activation it rides.
#
# Seams, following ../tests/kon-wireguard-profile.sh: NMCLI and RESOLVECTL are
# stubs that record their arguments. The hook runs as root in production but
# nothing here needs the privilege.
#
#   ./tests/twingate-dns-demote.sh
set -u

cd "$(dirname "$0")/.." || exit 1

HOOK=./system/etc/NetworkManager/dispatcher.d/89-twingate-dns-demote
D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

report() { # report <name> <ok?> <detail>
  n=$((n + 1))
  if [ "$2" = ok ]; then
    printf 'ok %d - %s\n' "$n" "$1"
  else
    printf 'FAIL %d - %s\n      %s\n' "$n" "$1" "$3"
    fails=$((fails + 1))
  fi
}

# Records every invocation (one line per argument) and answers nothing; each
# case greps the log for the exact argv it expects.
cat > "$D/nmcli" <<STUB
#!/bin/sh
echo "+ \$*" >> "$D/nmcli-calls"
STUB
chmod +x "$D/nmcli"

cat > "$D/resolvectl" <<STUB
#!/bin/sh
echo "+ \$*" >> "$D/ctl-calls"
STUB
chmod +x "$D/resolvectl"

# The dispatcher's -g GENERAL.CONNECTION query has no CONNECTION_ID to lean
# on; answer it so the fallback path is exercised too. A case that wants the
# stub to answer must seed the query answer file first.
cat > "$D/nmcli-answering" <<STUB
#!/bin/sh
echo "+ \$*" >> "$D/nmcli-calls"
[ "\$1\$2" = "-gGENERAL.CONNECTION" ] && echo "Twingate-generated"
exit 0
STUB
chmod +x "$D/nmcli-answering"

run_hook() { # run_hook <iface> <action> [env VAR=VAL...] -- runs the hook with the
  #              stub binaries (plus any extra env), prints both call logs
  iface=$1 action=$2; shift 2
  : > "$D/nmcli-calls"; : > "$D/ctl-calls"
  env NMCLI="$D/nmcli" RESOLVECTL="$D/resolvectl" "$@" \
    "$HOOK" "$iface" "$action" >/dev/null 2>&1
  cat "$D/nmcli-calls" "$D/ctl-calls" 2>/dev/null
}

if [ -x "$HOOK" ]; then
  report "the dispatcher hook is executable" ok
else
  report "the dispatcher hook is executable" no "$HOOK is missing or not +x (NM skips it)"
fi

# ── only the event that matters acts ─────────────────────────────────────

if [ -z "$(run_hook sdwan0 down)" ] && [ -z "$(run_hook sdwan0 connectivity-change)" ]; then
  report "teardown and non-activation events are ignored" ok
else
  report "teardown and non-activation events are ignored" no "the hook acts outside its window"
fi

if [ -z "$(run_hook wlan0 up)" ]; then
  report "a non-sdwan0 activation is ignored" ok
else
  report "a non-sdwan0 activation is ignored" no "the hook reaches into other connections"
fi

# ── the demotion itself, on the event that matters ───────────────────────

out=$(run_hook sdwan0 up)
case "$out" in
*"+ connection modify sdwan0 ipv4.dns-priority 10000 ipv6.dns-priority 10000 ipv4.dns-search  ipv6.dns-search "*)
  report "activation demotes dns-priority to 10000 on both families" ok ;;
*)
  report "activation demotes dns-priority to 10000 on both families" no "got: $out" ;;
esac

if printf '%s' "$out" | grep -q "+ device reapply sdwan0"; then
  report "the demotion is reapplied immediately" ok
else
  report "the demotion is reapplied immediately" no "no reapply; NM commits DNS at its leisure"
fi

if printf '%s' "$out" | grep -q "+ default-route sdwan0 false"; then
  report "resolved default-route is pinned off the link" ok
else
  report "resolved default-route is pinned off the link" no "sdwan0 stays resolved's default link"
fi

# ── the connection name is looked up, not assumed ────────────────────────

: > "$D/nmcli-calls"; : > "$D/ctl-calls"
NMCLI="$D/nmcli-answering" RESOLVECTL="$D/resolvectl" \
  "$HOOK" sdwan0 up >/dev/null 2>&1
if grep -q "^+ connection modify Twingate-generated " "$D/nmcli-calls"; then
  report "the live connection name is used, not the interface name" ok
else
  report "the live connection name is used, not the interface name" no \
    "modify did not target the D-Bus connection name"
fi

# ── failure containment: every step warns, none aborts ────────────────────

cat > "$D/nmcli-fail" <<STUB
#!/bin/sh
exit 1
STUB
chmod +x "$D/nmcli-fail"

: > "$D/ctl-calls"
out=$(env NMCLI="$D/nmcli-fail" RESOLVECTL="$D/resolvectl" \
  "$HOOK" sdwan0 up 2>&1)
if printf '%s' "$out" | grep -q "could not demote"; then
  report "a failed modify warns with the connection name" ok
else
  report "a failed modify warns with the connection name" no "silent failure: $out"
fi

# The resolvectl pin must still fire even after nmcli fails -- it is the only
# step that does not depend on NM's connection state.
if grep -q "+ default-route sdwan0 false" "$D/ctl-calls"; then
  report "the resolved pin still fires when nmcli fails" ok
else
  report "the resolved pin still fires when nmcli fails" no "one dead binary short-circuits the hook"
fi

printf '\n%d case(s), %d failure(s)\n' "$n" "$fails"
[ "$fails" -eq 0 ]
