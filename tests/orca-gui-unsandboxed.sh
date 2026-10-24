#!/usr/bin/env bash
# Cases for the claim that orca.nix runs the Orca GUI unsandboxed: the
# AppImage's tree patched with autoPatchelf and wrapped with makeWrapper, not
# appimageTools' bwrap FHS env.
#
# Why this needs a test. Inside the FHS env, everything Orca spawns -- its
# terminals and the agents in them -- saw the env's /usr, not the host's:
# /etc/hostname was missing, `date` printed UTC (/etc/localtime points into
# /usr/share/zoneinfo), every font fell to Montserrat Thin. And bwrap stood
# between every caller and Electron, forwarding no signals. None of that shows
# from the host, only from a process the app spawns, so these cases probe from
# one.
#
# The probes run on the host. When this script itself runs inside a sandbox
# (an Orca terminal on the old build has no /etc/hostname), each probe goes
# through `systemd-run --user`, whose units the user manager starts outside it.
#
# Two trees are deliberately left unpatched and must stay byte-identical:
# resources/node-runtime, whose node is copied into orcad slots and checked
# against a sha256 pin in app.asar (a patched node fails the check, and Orca
# falls back to downloading stock node), and resources/orcad-template, whose
# per-target builds (arm64, musl) are shipped to remote hosts.
#
# The GUI case (6) opens a real, visible window on the desktop, so it runs
# only with ORCA_TEST_GUI=1 -- ask before setting it on someone's session.
# It starts a second instance with its own --user-data-dir and a throwaway
# HOME, so it never touches the running Orca's profile, its
# ~/.orca/agent-hooks or ~/.claude/settings.json.
#
#   tests/orca-gui-unsandboxed.sh               build this host's orca and test it
#   ORCA_TEST_GUI=1 tests/orca-gui-unsandboxed.sh   ... and launch it once
#   tests/orca-gui-unsandboxed.sh <orca-path>   test a given build, e.g. the live one:
#                                               $(dirname $(dirname $(readlink -f ~/.nix-profile/bin/orca-ide)))
set -u
set -o pipefail

fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }

FLAKE=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

if [ -e /etc/hostname ]; then
  host() { "$@"; }
else
  sr=$(nix build --no-link --print-out-paths --inputs-from "$FLAKE" 'nixpkgs#systemd^out') \
    || { echo "inside a sandbox and could not build systemd-run" >&2; exit 1; }
  # Through /bin/sh, so the command is looked up on the host's PATH, not here.
  host() { "$sr/bin/systemd-run" --user --pipe --wait --quiet /bin/sh -c 'exec "$@"' sh "$@"; }
fi
HOSTNAME_=$(host cat /etc/hostname) || { echo "cannot reach the host" >&2; exit 1; }

ORCA="${1:-}"
if [ -z "$ORCA" ]; then
  drv=$(nix eval --raw "$FLAKE#homeConfigurations.$HOSTNAME_.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: builtins.match "orca-[0-9.]+" (p.name or "") != null) ps)).drvPath') \
    || { echo "orca is not in home.packages here" >&2; exit 1; }
  ORCA=$(nix-store --realise "$drv" | tail -1) || { echo "could not build orca" >&2; exit 1; }
fi
ORCA=$(readlink -f "$ORCA")

# The desktop file's Exec names the GUI bin, whatever it is called this week.
exe=$(sed -n 's/^Exec=\([^ ]*\).*/\1/p' "$ORCA"/share/applications/orca-ide.desktop | head -1)
GUI="$ORCA/bin/$exe"
[ -n "$exe" ] && [ -x "$GUI" ] || { echo "no GUI bin from $ORCA's desktop Exec ('$exe')" >&2; exit 1; }
echo "testing $GUI"

T=$(mktemp -d "$HOME/.cache/orca-gui-test.XXXXXX"); trap 'rm -rf "$T"' EXIT

# 1. No bwrap anywhere in the launch chain.
if nix-store -qR "$ORCA" | grep -q -- '-bubblewrap-'; then
  fail "no sandbox" "bubblewrap is in the closure of $ORCA"
else
  pass "no bubblewrap in the closure"
fi

# The unwrapped tree: where resources/ lives.
APP=$(nix-store -qR "$ORCA" | while read -r p; do
  [ -f "$p/lib/orca/resources/app.asar" ] && echo "$p/lib/orca"; done | head -1)
[ -n "$APP" ] || { fail "layout" "no lib/orca/resources/app.asar in the closure"; APP=/nonexistent; }

# 2. Node mode: the wrapper must not inject --ozone-platform, which node
# rejects ("bad option", exit 9). orca's CLI probes the GUI this way.
got=$(host env ELECTRON_RUN_AS_NODE=1 "$GUI" -p 1 2>&1)
[ "$got" = 1 ] && pass "ELECTRON_RUN_AS_NODE=1 runs node" \
  || fail "node mode" "printed '$got', want 1"

# 3. What a spawned process sees is the host: hostname, zone, fonts.
node_run() { # node_run <shell command>: run it from a child of the GUI in node mode
  host env ELECTRON_RUN_AS_NODE=1 "$GUI" -e \
    'process.stdout.write(require("child_process").execFileSync("/bin/sh", ["-c", process.argv[1]]))' \
    "$1" 2>&1
}
for c in 'cat /etc/hostname' 'date +%Z' 'readlink /etc/localtime'; do
  want=$(host /bin/sh -c "$c"); got=$(node_run "$c")
  [ -n "$want" ] && [ "$got" = "$want" ] && pass "child sees host's '$c' ($want)" \
    || fail "$c" "child got '$got', host has '$want'"
done
# Orca's Linux terminalFontFamily default is exactly "DejaVu Sans Mono", one
# family with no fallback, and Chromium refuses fontconfig's alias to another
# mono font; the host has no DejaVu, so orca.nix's fontsConf must apply.
got=$(node_run "fc-list -f '%{family[0]}\n' 'DejaVu Sans Mono' | sort -u | head -1")
[ "$got" = "DejaVu Sans Mono" ] && pass "'DejaVu Sans Mono' exists for the app (terminal default)" \
  || fail "DejaVu" "app's fontconfig has '${got:-nothing}'"
for c in 'sans\-serif:Noto Sans' 'monospace:Noto Sans Mono'; do
  pat=${c%%:*}; want=${c#*:}
  got=$(node_run "fc-match -f '%{family[0]}' '$pat'")
  [ "$got" = "$want" ] && pass "$pat -> $want for the app" \
    || fail "$pat" "resolves to '${got:-nothing}', want '$want'"
done

# Orca's terminals: the patched node-pty, loaded by this Electron, opens a pty.
got=$(host env ELECTRON_RUN_AS_NODE=1 "$GUI" -e '
  const p = require(process.argv[1]).spawn("/bin/sh", ["-c", "echo pty-ok"], {});
  let o = ""; p.onData(d => o += d); p.onExit(e => console.log(o.trim(), e.exitCode));' \
  "$APP/resources/node_modules/node-pty" 2>&1)
[ "$got" = "pty-ok 0" ] && pass "node-pty opens a pty" || fail "node-pty" "got '$got'"

# 4. Every native ELF of this arch resolves all its libraries on the host.
mapfile -t elfs < <(find "$APP" -type f \( -name '*.so*' -o -name '*.node' -o -perm -u+x \) \
  -not -path '*/orcad-template/*' -print0 \
  | host xargs -0 file | grep -E 'ELF 64-bit LSB .*(executable|shared object), x86-64' | cut -d: -f1)
if [ "${#elfs[@]}" -lt 10 ]; then
  fail "ELFs" "only ${#elfs[@]} found under $APP"
else
  missing=$(host /bin/sh -c 'for f; do ldd "$f" 2>&1 | grep "not found" | sed "s|^|$f: |"; done' sh "${elfs[@]}")
  [ -z "$missing" ] && pass "all ${#elfs[@]} ELFs resolve their libraries" \
    || fail "ldd" "$(printf '\n%s' "$missing")"
fi

# 5. The pinned trees are the AppImage's, byte for byte -- not patched, not
# stripped, no shebang rewritten. The derivation's src is the extracted
# AppImage, so it is on this machine whenever the build is.
src=$(nix derivation show "$(nix-store -qd "$APP/../..")" 2>/dev/null \
  | grep -oE '"src": *"/nix/store/[^"]*-extracted"' | grep -oE '/nix/store/[^"]*')
if [ -z "$src" ]; then
  fail "pinned trees" "cannot find the derivation's src for $APP"
else
  for d in resources/node-runtime resources/orcad-template; do
    if out=$(diff -r "$src/$d" "$APP/$d" 2>&1); then
      pass "$d is the AppImage's, unchanged"
    else
      fail "$d" "differs from $src: $(echo "$out" | head -3)"
    fi
  done
fi
node="$APP/resources/node-runtime/bin/node"
# The slot's runtime reference: what orcad checks the copied node against.
pin=$(tr -d '[:space:]' < "$APP/resources/orcad-template/targets/linux-x64-glibc/.runtime-node" 2>/dev/null)
got=$(sha256sum "$node" 2>/dev/null | cut -d' ' -f1)
[ -n "$pin" ] && [ "$got" = "$pin" ] && pass "bundled node matches orcad's runtime pin" \
  || fail "node pin" "sha256 '$got', pin '${pin:-missing}'"

# 6. A real second instance renders, sandboxes its renderers, and stops on
# SIGTERM. Its own --user-data-dir and HOME keep it off the live Orca's state.
if [ "${ORCA_TEST_GUI:-}" != 1 ]; then
  echo "skip GUI launch: it opens a visible window; set ORCA_TEST_GUI=1"
elif [ -z "$(host printenv WAYLAND_DISPLAY)" ]; then
  echo "skip GUI launch: no Wayland session"
else
  unit="orca-gui-test-$$"
  mkdir -p "$T/home"
  since=$(date +%s)
  "${sr:+$sr/bin/}systemd-run" --user --quiet --unit="$unit" --property=KillSignal=SIGTERM \
    --setenv=HOME="$T/home" "$GUI" --user-data-dir="$T/profile"
  pids=""
  for _ in $(seq 40); do
    sleep 1
    pids=$(host systemctl --user show -p MainPID --value "$unit" 2>/dev/null)
    [ -n "$pids" ] && [ "$pids" != 0 ] \
      && host hyprctl clients -j 2>/dev/null | grep -q "\"pid\": $pids," && break
  done
  if host hyprctl clients -j 2>/dev/null | grep -q "\"pid\": $pids,"; then
    pass "a window maps (pid $pids)"
  else
    fail "window" "no Hyprland client for pid '$pids' after 40s"
    host journalctl --user -u "$unit" --no-pager -n 20
  fi
  # Renderers in their own user namespace = Chromium's namespace sandbox,
  # with chrome-sandbox left unprivileged.
  ns=$(host /bin/sh -c '
    main=$(readlink /proc/'"$pids"'/ns/user)
    for p in $(systemctl --user show -p ControlGroup --value '"$unit"' | xargs -I{} cat /sys/fs/cgroup{}/cgroup.procs); do
      tr "\0" " " < /proc/$p/cmdline | grep -q -- "--type=renderer" || continue
      [ "$(readlink /proc/$p/ns/user)" != "$main" ] && echo sandboxed || echo unsandboxed
    done' | sort -u | tr '\n' ' ')
  [ "$ns" = "sandboxed " ] && pass "renderers run in their own user namespace" \
    || fail "chromium sandbox" "renderers: '${ns:-none}'"
  # A transient unit that stopped cleanly is unloaded (show prints nothing);
  # one whose main process exited non-zero stays loaded as failed, with why.
  if host systemctl --user stop "$unit"; then
    res=$(host systemctl --user show -p Result --value "$unit" 2>/dev/null)
    res=${res:-success}
  else
    res="stop failed"
  fi
  host systemctl --user reset-failed "$unit" 2>/dev/null
  [ "$res" = success ] && pass "stops cleanly on SIGTERM" \
    || fail "SIGTERM" "unit result '$res'"
  cores=$(host coredumpctl list --no-legend --since="@$since" 2>/dev/null | grep -E "$ORCA|orca-ide" || true)
  [ -z "$cores" ] && pass "no coredumps" || fail "coredumps" "$cores"
fi

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
