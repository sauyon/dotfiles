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
# Once no bwrap-era Orca is left running anywhere, that path can go.
#
# That the pinned trees (resources/node-runtime, orcad-template, ripgrep) stay
# the AppImage's, every pin in orcad's manifest holds, and the desktop file
# launches the wrapper and nothing else, is the derivation's
# installCheckPhase, not a case here: it gates the build.
#
# The GUI case (5) needs no desktop and never shows on one: it starts its own
# headless sway (wlroots' headless backend, a private XDG_RUNTIME_DIR and
# D-Bus, a near-empty environment) and runs Orca in that, with its own
# --user-data-dir and a throwaway HOME, so it never touches a running Orca's
# profile, ~/.orca, ~/.config/orca or ~/.claude, nor the live desktop, keyring
# or agents. ORCA_TEST_SHOT=<file.png> keeps its screenshot.
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
# Its arguments, minus field codes (%U), are what case 5 launches with.
read -ra exec_line < <(sed -n 's/^Exec=//p' "$ORCA"/share/applications/orca-ide.desktop | head -1)
exe=${exec_line[0]:-}
exec_args=(); for a in "${exec_line[@]:1}"; do case $a in %*) ;; *) exec_args+=("$a") ;; esac; done
GUI="$ORCA/bin/$exe"
[ -n "$exe" ] && [ -x "$GUI" ] || { echo "no GUI bin from $ORCA's desktop Exec ('$exe')" >&2; exit 1; }
echo "testing $GUI"

# Under ~/.cache, not /tmp: the bwrap-era sandbox has its own /tmp, which the
# host-side probes would not see.
unit="orca-gui-test-$$"
mkdir -p "$HOME/.cache"
T=$(mktemp -d "$HOME/.cache/orca-gui-test.XXXXXX") || { echo "mktemp failed" >&2; exit 1; }
trap 'host systemctl --user stop "$unit" "$unit-serve" 2>/dev/null; rm -rf "$T"' EXIT

# Every node-mode run of the GUI, with no way to a display: were node mode
# ever lost, this would be a GUI launch, and it must fail rather than open a
# window on the live desktop -- no WAYLAND_DISPLAY/DISPLAY, and a runtime dir
# with no socket in it (unset, Wayland falls back to wayland-0 there).
mkdir -p -m700 "$T/nodisplay"
appnode() { host timeout 120 env -u WAYLAND_DISPLAY -u WAYLAND_SOCKET -u DISPLAY \
  XDG_RUNTIME_DIR="$T/nodisplay" ELECTRON_RUN_AS_NODE=1 "$GUI" "$@"; }

# 1. No bwrap anywhere in the launch chain.
closure=$(nix-store -qR "$ORCA") || { echo "nix-store -qR $ORCA failed" >&2; exit 1; }
if grep -q -- '-bubblewrap-' <<< "$closure"; then
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
got=$(appnode -p 1 2>&1)
[ "$got" = 1 ] && pass "ELECTRON_RUN_AS_NODE=1 runs node" \
  || fail "node mode" "printed '$got', want 1"

# 3. What a spawned process sees is the host: hostname, zone, fonts. The
# app's FONTCONFIG_FILE reaches every child, so it must be the host's
# fontconfig plus DejaVu: a child resolves families as the host does and
# sees every font the host has.
node_run() { # node_run <shell command>: run it from a child of the GUI in node mode
  appnode -e \
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
# mono font; the host has no DejaVu, so orca.nix's fontsConf must add it.
got=$(node_run "fc-list -f '%{family[0]}\n' 'DejaVu Sans Mono' | sort -u | head -1")
[ "$got" = "DejaVu Sans Mono" ] && pass "'DejaVu Sans Mono' exists for the app (terminal default)" \
  || fail "DejaVu" "app's fontconfig has '${got:-nothing}'"
# Patterns escaped: in fontconfig's syntax a bare hyphen starts a size.
for pat in 'sans\-serif' 'sans' 'monospace' 'system\-ui' 'Arial'; do
  want=$(host fc-match -f '%{family[0]}' "$pat"); got=$(node_run "fc-match -f '%{family[0]}' '$pat'")
  [ -n "$want" ] && [ "$got" = "$want" ] && pass "'${pat//\\/}' -> $want for children, as on the host" \
    || fail "${pat//\\/}" "children get '${got:-nothing}', the host '$want'"
done
want=$(host fc-list -f '%{file}\n' | sort -u); got=$(node_run "fc-list -f '%{file}\n'" | sort -u)
hidden=$(comm -23 <(echo "$want") <(echo "$got") | wc -l)
[ -n "$want" ] && [ "$hidden" = 0 ] && pass "children see all $(echo "$want" | wc -l) of the host's font files" \
  || fail "font files" "$hidden of the host's are hidden from children"

# home.nix's symbol gap font does not yield to DejaVu (that would cut glyphs
# that are tofu outside Orca); it relies on fontconfig ranking DejaVu ahead
# wherever both have a glyph, under the app's own fontconfig. Every such
# glyph, for a mono and a sans request.
cat > "$T/gapcheck.sh" <<'EOF'
# gapcheck.sh <scratch-prefix>: "<family>|<moved>|<shared>" per request, or "nogap"
gap=$(fc-list -f '%{file}\n' 'Noto Sans Symbols 2' | grep -m1 Gaps) || { echo nogap; exit; }
cps() { fc-query -f '%{charset}\n' "$1" | tr ' ' '\n' \
  | awk -F- 'NF { a = strtonum("0x" $1); b = $2 == "" ? a : strtonum("0x" $2); for (i = a; i <= b; i++) printf "%x\n", i }' \
  | sort -u; }
cps "$gap" > "$1.gap"
for pair in 'monospace:DejaVu Sans Mono' 'sans\-serif:DejaVu Sans'; do
  req=${pair%%:*}; fam=${pair#*:}
  cps "$(fc-list -f '%{file}\n' "$fam:style=Book" | head -1)" | comm -12 - "$1.gap" > "$1.both"
  moved=0
  while read -r cp; do
    [ "$(fc-match -f '%{family[0]}' "$req:charset=$cp")" = 'Noto Sans Symbols 2' ] && moved=$((moved+1))
  done < "$1.both"
  echo "$fam|$moved|$(wc -l < "$1.both")"
done
EOF
got=$(node_run "/bin/sh $T/gapcheck.sh $T/gapcheck")
if [ "$got" = nogap ]; then
  echo "skip gap-font ranking: no Noto Sans Symbols 2 gap font on this host"
else
  while IFS='|' read -r fam moved total; do
    [ "${total:-0}" -gt 0 ] 2>/dev/null && [ "$moved" = 0 ] \
      && pass "$fam keeps all $total glyphs it shares with the symbol gap font" \
      || fail "gap font" "$fam: $moved of ${total:-?} shared glyphs go to the gap font"
  done <<< "$got"
fi

# Orca's terminals: the patched node-pty, loaded by this Electron, opens a pty.
got=$(appnode -e '
  const p = require(process.argv[1]).spawn("/bin/sh", ["-c", "echo pty-ok"], {});
  let o = ""; p.onData(d => o += d);
  p.onExit(e => setTimeout(() => console.log(o.trim(), e.exitCode), 200));' \
  "$APP/resources/node_modules/node-pty" 2>&1)
[ "$got" = "pty-ok 0" ] && pass "node-pty opens a pty" || fail "node-pty" "got '$got'"

# 4. Every native ELF of this arch resolves all its libraries, through its
# own loader -- `ld.so --list`, not ldd, whose host loader would find a
# library in Arch's /usr/lib that the patched RUNPATH lost.
mapfile -t elfs < <(find "$APP" -type f \( -name '*.so*' -o -name '*.node' -o -perm -u+x \) \
  -not -path '*/orcad-template/*' -print0 \
  | host xargs -0 file | grep -E 'ELF 64-bit LSB .*(executable|shared object), x86-64' | cut -d: -f1)
ldso=$(readelf -l "$APP/orca-ide" | grep -oE '/nix/store/[^]]*/ld-linux-x86-64.so.2')
if [ "${#elfs[@]}" -lt 10 ] || [ -z "$ldso" ]; then
  fail "ELFs" "only ${#elfs[@]} found under $APP (is file(1) on the host?), loader '$ldso'"
else
  # A shared object has no interpreter of its own; it loads into one whose
  # loader is the patched executables'. Unpatched ones (the pinned node)
  # name the host's, which is what they get.
  missing=$(host env -u LD_LIBRARY_PATH /bin/sh -c 'ldso=$1; shift; for f; do
      i=$(readelf -l "$f" 2>/dev/null | sed -n "s/.*interpreter: \([^]]*\)]/\1/p")
      "${i:-$ldso}" --list "$f" 2>&1 | grep "not found" | sed "s|^|$f: |"; done' sh "$ldso" "${elfs[@]}")
  [ -z "$missing" ] && pass "all ${#elfs[@]} ELFs resolve their libraries, each with its own loader" \
    || fail "ld.so --list" "$(printf '\n%s' "$missing")"
fi

# What they dlopen, which no loader listing shows: each must be on the
# opener's RUNPATH, or it falls through to nothing (nix's loader has no
# /usr/lib). The bwrap env's /usr used to supply all of these.
dlopens() { # dlopens <elf> <soname>...: report the sonames not on its RUNPATH
  local f=$1; shift
  local rp; rp=$(readelf -d "$f" | sed -n 's/.*R.*PATH.*\[\(.*\)\]/\1/p' | sed "s|\$ORIGIN|$(dirname "$f")|g")
  for so; do
    local found=""
    IFS=: read -ra dirs <<< "$rp"
    for d in "${dirs[@]}"; do [ -e "$d/$so" ] && { found=1; break; }; done
    [ -n "$found" ] || echo "${f#$APP/}: $so"
  done
}
# Electron: audio (pulse; pipewire for screen capture), VA-API, safeStorage,
# notifications, GL; ANGLE (libGLESv2.so) opens the system GL itself. The
# clipboard helper: wayland-client, and gtk3 for X11.
lost=$(dlopens "$APP/orca-ide" libpulse.so.0 libpipewire-0.3.so.0 libva.so.2 libva-drm.so.2 \
         libsecret-1.so.0 libnotify.so.4 libGL.so.1 libEGL.so.1 libudev.so.1
       for gl in libGLESv2.so libEGL.so libvk_swiftshader.so; do dlopens "$APP/$gl" libEGL.so.1 libGL.so.1; done
       dlopens "$APP/resources/bin/orca-wayland-clipboard" libwayland-client.so.0 libgtk-3.so.0)
[ -z "$lost" ] && pass "everything they dlopen is on their RUNPATH" \
  || fail "dlopen" "$(printf '\n%s' "$lost")"

# 5. A real instance, launched as the desktop file launches it, renders,
# sandboxes its renderers, and exits 0 on SIGTERM -- in a headless sway of its
# own, cut off from the live session: a private runtime dir and D-Bus (no
# tray, notifications, portals or keyring on the real desktop), no ssh-agent,
# no Hyprland socket. It needs no desktop and never appears on one.
tools=$(nix build --no-link --print-out-paths --inputs-from "$FLAKE" \
    nixpkgs#sway nixpkgs#grim nixpkgs#dbus nixpkgs#xvfb nixpkgs#nodejs_24) \
  || { echo "could not build sway/grim/dbus/xvfb/node" >&2; exit 1; }
sway=$(echo "$tools" | grep -- '-sway-'); grim=$(echo "$tools" | grep -- '-grim-')
dbus=$(echo "$tools" | grep -E -- '-dbus-[0-9.]+$')
xvfb=$(echo "$tools" | grep -- '-xvfb-'); node24=$(echo "$tools" | grep -- '-nodejs-')
run="$T/run"; mkdir -p -m700 "$run"; mkdir -p "$T/home"
cat > "$T/sway.conf" <<EOF
output HEADLESS-1 resolution 1280x800
exec /bin/sh -c '"$GUI" ${exec_args[*]} --user-data-dir="$T/profile" & echo \$! > "$T/orca.pid"; wait \$!; echo \$? > "$T/orca.exit"'
EOF
in_session() { host env -u DISPLAY XDG_RUNTIME_DIR="$run" WAYLAND_DISPLAY=wayland-1 "$@"; }
since=$(date +%s)
# env -i, then an allowlist: the user manager's environment carries the live
# session's sockets and real-home paths (GNUPGHOME, SSH_TPM_AUTH_SOCK, ...).
"${sr:+$sr/bin/}systemd-run" --user --quiet --collect --unit="$unit" --property=RuntimeMaxSec=600 \
  --working-directory="$T/home" \
  /bin/sh -c 'exec env -i PATH="$PATH" LANG="${LANG:-C.UTF-8}" TZ="${TZ:-}" "$@"' sh \
    HOME="$T/home" XDG_RUNTIME_DIR="$run" XDG_CURRENT_DESKTOP=sway \
    WLR_BACKENDS=headless WLR_RENDERER=pixman WLR_LIBINPUT_NO_DEVICES=1 \
    "$dbus/bin/dbus-run-session" -- "$sway/bin/sway" -c "$T/sway.conf" \
  || { fail "session" "systemd-run could not start $unit"; }
pid=""
for _ in $(seq 60); do
  sleep 1
  sock=$(ls "$run"/sway-ipc.*.sock 2>/dev/null | head -1)
  [ -n "$sock" ] || continue
  in_session "$sway/bin/swaymsg" -s "$sock" -t get_tree > "$T/tree.json" 2>/dev/null || continue
  pid=$(appnode -e '
    const walk = n => n.app_id === "orca" ? n.pid : [...n.nodes, ...n.floating_nodes].map(walk).find(Boolean);
    console.log(walk(JSON.parse(require("fs").readFileSync(process.argv[1]))) || "");' "$T/tree.json")
  [ -n "$pid" ] && break
done
if [ -z "$pid" ]; then
  fail "window" "no orca window in the headless session after 60s"
  host journalctl --user -u "$unit" --no-pager -n 20
else
  pass "a window maps (pid $pid)"
  # The pid the session launched is the window's: every wrapper execs, so
  # signals and exit codes reach Electron with nothing in between.
  launched=$(cat "$T/orca.pid" 2>/dev/null)
  [ "$launched" = "$pid" ] && pass "the launched pid is Electron's (wrappers exec)" \
    || fail "exec chain" "launched pid '$launched', window pid '$pid'"
  # Give the first frame time, then look at it: a blank or solid surface has
  # a handful of colours, a rendered UI with antialiased text has hundreds.
  sleep 10
  in_session "$grim/bin/grim" -t ppm "$T/shot.ppm"
  colours=$(appnode -e '
    const b = require("fs").readFileSync(process.argv[1]);
    let i = 0, f = 0; const tok = () => { while (/\s/.test(String.fromCharCode(b[i]))) i++;
      const s = i; while (!/\s/.test(String.fromCharCode(b[i]))) i++; return b.slice(s, i).toString(); };
    tok(); tok(); tok(); tok(); i++;
    const seen = new Set(); for (; i + 2 < b.length; i += 3) seen.add((b[i] << 16) | (b[i+1] << 8) | b[i+2]);
    console.log(seen.size);' "$T/shot.ppm" 2>&1)
  [ "${colours:-0}" -gt 100 ] 2>/dev/null && pass "it renders a UI ($colours colours on screen)" \
    || fail "render" "screen has '$colours' colours; blank or broken"
  if [ -n "${ORCA_TEST_SHOT:-}" ]; then in_session "$grim/bin/grim" "$ORCA_TEST_SHOT"; fi
  # The clipboard helper Orca uses on Wayland, against a live compositor.
  got=$(in_session "$APP/resources/bin/orca-wayland-clipboard" --probe 2>&1); rc=$?
  [ "$rc" = 0 ] && pass "orca-wayland-clipboard --probe works in the session" \
    || fail "clipboard" "exit $rc: $got"
  # Chromium's sandbox, with chrome-sandbox left unprivileged: renderers in
  # their own user namespace (layer 1; --no-sandbox renderers share the
  # browser's) and under a seccomp-bpf filter (layer 2, Seccomp: 2).
  ns=$(host /bin/sh -c '
    main=$(readlink /proc/'"$pid"'/ns/user)
    for p in $(cat /sys/fs/cgroup$(systemctl --user show -p ControlGroup --value '"$unit"')/cgroup.procs); do
      tr "\0" " " < /proc/$p/cmdline 2>/dev/null | grep -q -- "--type=renderer" || continue
      [ "$(readlink /proc/$p/ns/user)" != "$main" ] && echo -n userns || echo -n shared-ns
      grep -q "^Seccomp:[[:space:]]*2" /proc/$p/status && echo +seccomp || echo +no-seccomp
    done' | sort -u | tr '\n' ' ')
  [ "$ns" = "userns+seccomp " ] && pass "renderers are sandboxed (own user namespace, seccomp)" \
    || fail "chromium sandbox" "renderers: '${ns:-none}'"
  # SIGTERM to the pid this case started.
  host kill -TERM "$pid"
  for _ in $(seq 20); do [ -s "$T/orca.exit" ] && break; sleep 1; done
  code=$(cat "$T/orca.exit" 2>/dev/null)
  [ "$code" = 0 ] && pass "exits 0 on SIGTERM" || fail "SIGTERM" "exit status '${code:-none after 20s}'"
fi
host systemctl --user stop "$unit" 2>/dev/null
host systemctl --user reset-failed "$unit" 2>/dev/null

# 6. `orca-ide serve` runs this GUI as its Electron server; a signal to the
# CLI (kill, an agent's timeout) must take the server down with it, not
# orphan it with the profile and port held. Under bwrap it did orphan it:
# bwrap forwards no signals, and with --die-with-parent off nothing else
# reached Electron. Forced onto the Electron runtime (it prefers orcad, a
# node daemon), on Xvfb -- serve wants X11 -- in the same kind of isolated
# session as case 5. The CLI is the profile's own when it has one, else the
# same command run by hand: nixpkgs' node 24 on the AppImage's CLI script.
cli=${ORCA_CLI:-}
if [ -z "$cli" ]; then
  # Realised by drv, as the GUI is above: a bare output path cannot be built.
  clidrv=$(nix eval --raw "$FLAKE#homeConfigurations.$HOSTNAME_.config.home.packages" \
    --apply 'ps: let c = builtins.filter (p: builtins.match "orca-cli-[0-9.]+" (p.name or "") != null) ps;
                 in if c == [] then "" else (builtins.head c).drvPath') \
    || { echo "could not evaluate home.packages for orca-cli" >&2; exit 1; }
  if [ -n "$clidrv" ]; then
    cli=$(nix-store --realise "$clidrv" | tail -1)/bin/orca-ide
    [ -x "$cli" ] || { echo "could not build the profile's orca-cli ($clidrv)" >&2; exit 1; }
  else
    cli="$node24/bin/node $APP/resources/app.asar.unpacked/out/cli/index.js"
  fi
fi
echo "serve via: $cli"
cat > "$T/serve.sh" <<EOF
cd "$T/home"
ORCA_SERVE_RUNTIME=electron ORCA_APP_EXECUTABLE="$GUI" $cli serve --no-pairing > "$T/serve.out" 2>&1 &
cli=\$!
for _ in \$(seq 90); do grep -q 'Terminal daemon' "$T/serve.out" && break; sleep 1; done
servers() { for p in \$(cat /sys/fs/cgroup\$(cut -d: -f3 /proc/self/cgroup)/cgroup.procs); do
  [ "\$(readlink /proc/\$p/exe 2>/dev/null)" = "$APP/orca-ide" ] && echo \$p; done; }
before=\$(servers | wc -l)
kill -TERM \$cli
for _ in \$(seq 15); do [ -z "\$(servers)" ] && break; sleep 1; done
echo "before=\$before after=\$(servers | wc -l)"
EOF
res=$("${sr:+$sr/bin/}systemd-run" --user --quiet --collect --pipe --wait --unit="$unit-serve" \
  --property=RuntimeMaxSec=300 --working-directory="$T/home" \
  /bin/sh -c 'x=$1; shift; exec env -i PATH="$x:$PATH" LANG="${LANG:-C.UTF-8}" "$@"' sh "$xvfb/bin" \
    HOME="$T/home" XDG_RUNTIME_DIR="$run" "$dbus/bin/dbus-run-session" -- /bin/sh "$T/serve.sh" 2>/dev/null \
  | grep -o 'before=[0-9]* after=[0-9]*')
case $res in
  before=0*|"") fail "serve" "no Electron server started ($res): $(tail -3 "$T/serve.out" 2>/dev/null)" ;;
  *after=0) pass "SIGTERM to orca-ide serve takes its Electron server down (${res%% *} processes)" ;;
  *) fail "serve" "Electron server orphaned after the CLI was killed: $res" ;;
esac
# This build's only: a crash of some other Orca meanwhile is not this one's.
cores=$(host coredumpctl list --no-legend --since="@$since" 2>/dev/null | grep -F "$APP" || true)
[ -z "$cores" ] && pass "no coredumps" || fail "coredumps" "$cores"

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
