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
# The GUI case (6) needs no desktop and never shows on one: it starts its own
# headless sway (wlroots' headless backend, a private XDG_RUNTIME_DIR) and
# runs Orca in that, with its own --user-data-dir and a throwaway HOME, so it
# never touches a running Orca's profile, ~/.orca/agent-hooks or
# ~/.claude/settings.json. ORCA_TEST_SHOT=<file.png> keeps its screenshot.
#
#   tests/orca-gui-unsandboxed.sh               build this host's orca and test it
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

# 6. A real instance renders, sandboxes its renderers, and exits 0 on
# SIGTERM -- in a headless sway of its own, so it needs no desktop and never
# appears on one.
tools=$(nix build --no-link --print-out-paths --inputs-from "$FLAKE" nixpkgs#sway nixpkgs#grim) \
  || { echo "could not build sway/grim" >&2; exit 1; }
sway=$(echo "$tools" | grep -- '-sway-'); grim=$(echo "$tools" | grep -- '-grim-')
run="$T/run"; mkdir -p -m700 "$run"; mkdir -p "$T/home"
cat > "$T/sway.conf" <<EOF
output HEADLESS-1 resolution 1280x800
exec /bin/sh -c '"$GUI" --user-data-dir="$T/profile"; echo \$? > "$T/orca.exit"'
EOF
in_session() { host env -u DISPLAY XDG_RUNTIME_DIR="$run" WAYLAND_DISPLAY=wayland-1 "$@"; }
unit="orca-gui-test-$$"
since=$(date +%s)
"${sr:+$sr/bin/}systemd-run" --user --quiet --unit="$unit" --setenv=HOME="$T/home" \
  --setenv=XDG_RUNTIME_DIR="$run" --setenv=WLR_BACKENDS=headless --setenv=WLR_RENDERER=pixman \
  --setenv=WLR_LIBINPUT_NO_DEVICES=1 --setenv=WAYLAND_DISPLAY= --setenv=DISPLAY= \
  "$sway/bin/sway" -c "$T/sway.conf"
pid=""
for _ in $(seq 60); do
  sleep 1
  sock=$(ls "$run"/sway-ipc.*.sock 2>/dev/null | head -1)
  [ -n "$sock" ] || continue
  in_session "$sway/bin/swaymsg" -s "$sock" -t get_tree > "$T/tree.json" 2>/dev/null || continue
  pid=$(host env ELECTRON_RUN_AS_NODE=1 "$GUI" -e '
    const walk = n => n.app_id === "orca" ? n.pid : [...n.nodes, ...n.floating_nodes].map(walk).find(Boolean);
    console.log(walk(JSON.parse(require("fs").readFileSync(process.argv[1]))) || "");' "$T/tree.json")
  [ -n "$pid" ] && break
done
if [ -z "$pid" ]; then
  fail "window" "no orca window in the headless session after 60s"
  host journalctl --user -u "$unit" --no-pager -n 20
else
  pass "a window maps (pid $pid)"
  # Give the first frame time, then look at it: a blank or solid surface has
  # a handful of colours, a rendered UI with antialiased text has hundreds.
  sleep 10
  in_session "$grim/bin/grim" -t ppm "$T/shot.ppm"
  colours=$(host env ELECTRON_RUN_AS_NODE=1 "$GUI" -e '
    const b = require("fs").readFileSync(process.argv[1]);
    let i = 0, f = 0; const tok = () => { while (/\s/.test(String.fromCharCode(b[i]))) i++;
      const s = i; while (!/\s/.test(String.fromCharCode(b[i]))) i++; return b.slice(s, i).toString(); };
    tok(); tok(); tok(); tok(); i++;
    const seen = new Set(); for (; i + 2 < b.length; i += 3) seen.add((b[i] << 16) | (b[i+1] << 8) | b[i+2]);
    console.log(seen.size);' "$T/shot.ppm" 2>&1)
  [ "${colours:-0}" -gt 100 ] 2>/dev/null && pass "it renders a UI ($colours colours on screen)" \
    || fail "render" "screen has '$colours' colours; blank or broken"
  if [ -n "${ORCA_TEST_SHOT:-}" ]; then in_session "$grim/bin/grim" "$ORCA_TEST_SHOT"; fi
  # Renderers in their own user namespace = Chromium's namespace sandbox,
  # with chrome-sandbox left unprivileged.
  ns=$(host /bin/sh -c '
    main=$(readlink /proc/'"$pid"'/ns/user)
    for p in $(cat /sys/fs/cgroup$(systemctl --user show -p ControlGroup --value '"$unit"')/cgroup.procs); do
      tr "\0" " " < /proc/$p/cmdline 2>/dev/null | grep -q -- "--type=renderer" || continue
      [ "$(readlink /proc/$p/ns/user)" != "$main" ] && echo sandboxed || echo unsandboxed
    done' | sort -u | tr '\n' ' ')
  [ "$ns" = "sandboxed " ] && pass "renderers run in their own user namespace" \
    || fail "chromium sandbox" "renderers: '${ns:-none}'"
  # SIGTERM to the window's own pid, which this case started: the wrappers
  # all exec, so it is Electron itself, with nothing in between.
  host kill -TERM "$pid"
  for _ in $(seq 20); do [ -s "$T/orca.exit" ] && break; sleep 1; done
  code=$(cat "$T/orca.exit" 2>/dev/null)
  [ "$code" = 0 ] && pass "exits 0 on SIGTERM" || fail "SIGTERM" "exit status '${code:-none after 20s}'"
fi
host systemctl --user stop "$unit" 2>/dev/null
host systemctl --user reset-failed "$unit" 2>/dev/null
cores=$(host coredumpctl list --no-legend --since="@$since" 2>/dev/null | grep -E "$ORCA|orca-ide" || true)
[ -z "$cores" ] && pass "no coredumps" || fail "coredumps" "$cores"

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
