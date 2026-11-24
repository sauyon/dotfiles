#!/usr/bin/env bash
# Cases for orca.nix's app-menu accelerators: every chord the macOS menu has
# on Cmd, on Super here (nix/orca-menu-accelerators.js), and none on Ctrl.
#
# Why this needs a test. On Linux, upstream gives Edit > Copy and Select All
# no accelerator at all, and Chromium's own text-field chords are Ctrl-only,
# so in any field outside the terminal (Settings, search boxes) Super+C/V/A
# did nothing but type the letter. The keybinding table
# (nix/orca-mac-keybindings.js) cannot reach those fields: its clipboard
# actions are terminal.* only. The page still gets every key before the
# menu, as on macOS: the terminal's Super+V must paste once, a page's own
# binding of a menu chord must win over the menu (case 12), and a page
# forging the report of an unclaimed chord must click nothing (case 14).
#
# Every case presses real keys into a real Orca and reads the result back from
# the page (over Chromium's remote debugging port), the session's clipboard,
# or the shell. Nothing short of that exercises the main-process menu. The
# keys go through wayvnc, whose virtual keyboard carries a real keymap: wtype
# invents one per run, so its first key arrives with Escape's physical code
# (a "Ctrl+Q" that is really Ctrl+Escape), the next with Digit1's, and so on.
#
# The session is cut off from the live one as in tests/orca-gui-unsandboxed.sh
# case 5: a private runtime dir, D-Bus and /tmp, a throwaway HOME and profile;
# it never shows on the desktop. The HOME gets the activation's keybinding
# table merged in, as a real one does. Sway runs with no binds of its own, so
# every chord reaches the window; which chords Hyprland owns is
# nix/orca-mac-keybindings.js's concern.
#
# Not covered: a browser-pane page (its own webContents; case 14's forgery
# by the page itself stands in for one), and the dispatch's focus and blur
# checks. Fields in iframes are left out by design
# (nix/orca-menu-accelerators.js).
#
#   tests/orca-menu-accelerators.sh               build this host's orca and test it
#   tests/orca-menu-accelerators.sh <orca-path> [<keybindings-merge-path>]
#                                                 test a given build
set -u
set -o pipefail

fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }

FLAKE=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
host=$(cat /etc/hostname 2>/dev/null); host=${host:-$(uname -n)}

ORCA="${1:-}"
if [ -z "$ORCA" ]; then
  drv=$(nix eval --raw "$FLAKE#homeConfigurations.$host.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: builtins.match "orca-[0-9.]+" (p.name or "") != null) ps)).drvPath') \
    || { echo "orca is not in home.packages here" >&2; exit 1; }
  ORCA=$(nix-store --realise "$drv" | tail -1) || { echo "could not build orca" >&2; exit 1; }
fi
ORCA=$(readlink -f "$ORCA")
GUI="$ORCA/bin/orca-ide-app"
[ -x "$GUI" ] || { echo "no $GUI" >&2; exit 1; }
# Without the table a fresh HOME runs Orca's Linux defaults, Ctrl chords and all.
KB="${2:-}"
if [ -z "$KB" ]; then
  kbdrv=$(nix eval --raw "$FLAKE#homeConfigurations.$host.config.home.activation.orcaMacKeybindings.data" \
    --apply 'd: builtins.head (builtins.attrNames (builtins.getContext d))') \
    || { echo "could not evaluate home.activation.orcaMacKeybindings on this host" >&2; exit 1; }
  KB=$(nix-store --realise "$kbdrv" | tail -1) || { echo "could not build $kbdrv" >&2; exit 1; }
fi
echo "testing $GUI"

tools=$(nix build --no-link --print-out-paths --inputs-from "$FLAKE" \
    nixpkgs#sway nixpkgs#dbus nixpkgs#nodejs_24 nixpkgs#wayvnc 'nixpkgs#python3Packages.vncdotool' \
    nixpkgs#wl-clipboard) \
  || { echo "could not build sway/dbus/node/wayvnc/vncdotool/wl-clipboard" >&2; exit 1; }
sway=$(echo "$tools" | grep -- '-sway-'); dbus=$(echo "$tools" | grep -E -- '-dbus-[0-9.]+$')
node=$(echo "$tools" | grep -- '-nodejs-')/bin/node
wayvnc=$(echo "$tools" | grep -- '-wayvnc-')/bin/wayvnc
vncdo=$(echo "$tools" | grep -- '-vncdotool-')/bin/vncdo
wl=$(echo "$tools" | grep -- '-wl-clipboard-')/bin

mkdir -p "$HOME/.cache"
T=$(mktemp -d "$HOME/.cache/orca-accel-test.XXXXXX") || { echo "mktemp failed" >&2; exit 1; }
# Named after $T, unique on the box, not $$: a run in another PID namespace
# can share a PID, and the trap stops this unit by name.
unit="orca-accel-test-${T##*.}"
vncpid=""; holdpid=""
# User units holding nothing but processes of this test's Orca: each has the
# test HOME in its environment, which the env -i allowlist below gives every
# descendant and nothing else on the box has. Orca's terminal daemon scope
# (orca-daemon-*) is the one that escaped to the live user manager; a unit
# that also holds anything else -- a terminal tailing $T's log, the shell this
# runs in -- is never ours to stop.
our_daemons() {
  for u in $(systemctl --user list-units --all --plain --no-legend --type=service,scope | awk '{print $1}'); do
    [ "$u" = "$unit.service" ] && continue
    cg=$(systemctl --user show -p ControlGroup --value "$u")
    procs=$(cat "/sys/fs/cgroup$cg/cgroup.procs" 2>/dev/null) || continue
    [ -n "$procs" ] || continue
    all=1
    for p in $procs; do
      tr '\0' '\n' < "/proc/$p/environ" 2>/dev/null | grep -qxF "HOME=$T/home" || { all=""; break; }
    done
    [ -n "$all" ] && echo "$u"
  done
}
trap '[ -n "$holdpid" ] && kill "$holdpid" 2>/dev/null; [ -n "$vncpid" ] && kill "$vncpid" 2>/dev/null; systemctl --user stop "$unit" 2>/dev/null
      leaked=$(our_daemons); [ -n "$leaked" ] && systemctl --user stop $leaked 2>/dev/null
      chmod -R u+w "$T" 2>/dev/null; rm -rf "$T"' EXIT
run="$T/run"; mkdir -p -m700 "$run"; mkdir -p "$T/home"
# Two free loopback ports: a taken one would hand the keys (Super+Q among
# them) or the page queries to whatever listens there.
taken() { (exec 3<>"/dev/tcp/127.0.0.1/$1") 2>/dev/null; }
for _ in $(seq 20); do
  port=$(( 20000 + RANDOM % 20000 )); vport=$(( port + 1 ))
  taken "$port" || taken "$vport" || { free=1; break; }
done
[ -n "${free:-}" ] || { echo "no two free loopback ports in 20 tries" >&2; exit 1; }
HOME="$T/home" "$KB/bin/orca-mac-keybindings" 2> /dev/null \
  || { echo "could not merge the keybinding table into the test HOME" >&2; exit 1; }
printf 'hello\n' > "$T/home/a.js"  # for the editor case

# SHELL=/bin/sh: the terminal cases type shell commands, and a throwaway HOME
# would greet zsh with its new-user menu. The empty /run/user/<uid>: Orca
# starts its terminal daemon as a scope of the user manager it finds at the
# canonical runtime dir's bus, ahead of XDG_RUNTIME_DIR -- the live session's
# -- where it outlives this unit and the quit.
cat > "$T/sway.conf" <<EOF
output HEADLESS-1 resolution 1280x800
exec /bin/sh -c '"$GUI" --remote-debugging-port=$port --user-data-dir="$T/profile" > "$T/orca.log" 2>&1; echo \$? > "$T/orca.exit"'
EOF
systemd-run --user --quiet --collect --unit="$unit" --property=RuntimeMaxSec=600 \
  --property=PrivateTmp=yes --property=TemporaryFileSystem="/run/user/$(id -u)" \
  --working-directory="$T/home" \
  /bin/sh -c 'exec env -i PATH="$PATH" LANG="${LANG:-C.UTF-8}" ${TZ+TZ="$TZ"} "$@"' sh \
    HOME="$T/home" SHELL=/bin/sh XDG_RUNTIME_DIR="$run" XDG_CURRENT_DESKTOP=sway \
    WLR_BACKENDS=headless WLR_RENDERER=pixman WLR_LIBINPUT_NO_DEVICES=1 \
    "$dbus/bin/dbus-run-session" -- "$sway/bin/sway" -c "$T/sway.conf" \
  || { echo "systemd-run could not start $unit" >&2; exit 1; }

in_session() { env -u DISPLAY -u DBUS_SESSION_BUS_ADDRESS XDG_RUNTIME_DIR="$run" WAYLAND_DISPLAY=wayland-1 "$@"; }
# keys <vncdo commands>: e.g. keys key super-c; keys type 'some text'. Each
# vncdo is a new VNC client, and wayvnc drops the first key a client sends,
# so a bare Shift goes first.
keys() { timeout 30 "$vncdo" -s "127.0.0.1::$vport" key shift "$@" \
  || fail "keys" "vncdo $* failed; wayvnc: $(tail -3 "$T/wayvnc.log")"; sleep 0.7; }
clip() { in_session timeout 3 "$wl/wl-paste" -n 2>/dev/null; }
setclip() { in_session "$wl/wl-copy" "$1"; }
swaytree() { in_session "$sway/bin/swaymsg" -s "$(ls "$run"/sway-ipc.*.sock | head -1)" -t get_tree; }

# page <js function body>: run it in Orca's main window, print the JSON result.
cat > "$T/cdp.js" <<'EOF'
const [port, body] = process.argv.slice(2);
(async () => {
  const list = await (await fetch(`http://127.0.0.1:${port}/json/list`)).json();
  const t = list.find((t) => t.type === 'page' && /\/out\/renderer\/index\.html/.test(t.url));
  if (!t) throw new Error(`no main window among ${JSON.stringify(list.map((t) => t.url))}`);
  const ws = new WebSocket(t.webSocketDebuggerUrl);
  await new Promise((r, j) => { ws.onopen = r; ws.onerror = j; });
  ws.onmessage = (m) => {
    const d = JSON.parse(m.data);
    if (d.id !== 1) return;
    if (d.result.exceptionDetails) { console.error(d.result.exceptionDetails.exception?.description); process.exit(2); }
    console.log(JSON.stringify(d.result.result.value ?? null));
    process.exit(0);
  };
  ws.send(JSON.stringify({ id: 1, method: 'Runtime.evaluate',
    params: { expression: `(async () => {${body}})()`, awaitPromise: true, returnByValue: true } }));
})().catch((e) => { console.error(String(e)); process.exit(1); });
EOF
page() { "$node" "$T/cdp.js" "$port" "$1"; }
# cdpkey <modifiers> <key> <code> <vk>: a key down and up through the debugging
# protocol, for the one chord wayvnc cannot send (below).
cdpkey() {
  "$node" -e '
    const [port, modifiers, key, code, vk] = process.argv.slice(1);
    (async () => {
      const list = await (await fetch(`http://127.0.0.1:${port}/json/list`)).json();
      const t = list.find((t) => t.type === "page" && /\/out\/renderer\/index\.html/.test(t.url));
      const ws = new WebSocket(t.webSocketDebuggerUrl);
      await new Promise((r) => (ws.onopen = r));
      let id = 0;
      const send = (params) => new Promise((r) => {
        const i = ++id;
        ws.addEventListener("message", (m) => JSON.parse(m.data).id === i && r());
        ws.send(JSON.stringify({ id: i, method: "Input.dispatchKeyEvent", params }));
      });
      const base = { modifiers: +modifiers, key, code, windowsVirtualKeyCode: +vk, nativeVirtualKeyCode: +vk };
      await send({ type: "rawKeyDown", ...base });
      await send({ type: "keyUp", ...base });
      process.exit(0);
    })();' "$port" "$@"
  sleep 0.7
}
targets() { curl -s "http://127.0.0.1:$port/json/list"; }
str() { "$node" -e 'process.stdout.write(String(JSON.parse(require("fs").readFileSync(0, "utf8"))))'; }

ready=""
for _ in $(seq 60); do
  sleep 1
  page 'return !!document.querySelector("input[aria-label=\"Find files\"]")' 2>/dev/null | grep -q true \
    && { ready=1; break; }
done
[ -n "$ready" ] || { echo "Orca's main window did not come up in 60s; $(tail -5 "$T/orca.log")" >&2; exit 1; }
# On loopback, in the session; killed by pid at exit -- env execs it, so $!
# is wayvnc's own.
env -u DISPLAY -u DBUS_SESSION_BUS_ADDRESS XDG_RUNTIME_DIR="$run" WAYLAND_DISPLAY=wayland-1 \
  "$wayvnc" 127.0.0.1 "$vport" > "$T/wayvnc.log" 2>&1 &
vncpid=$!
# One client held open for the whole run. The session's only keyboard is
# wayvnc's, which exists only while a client is connected: between keys()
# calls the window would lose the keyboard and with it its focus, and a menu
# click that lands then finds no focused window -- as no real session does.
for _ in $(seq 20); do "$vncdo" -s "127.0.0.1::$vport" key shift > /dev/null 2>&1 && break; sleep 0.5; done
timeout 590 "$vncdo" -s "127.0.0.1::$vport" key shift pause 580 > /dev/null 2>&1 &
holdpid=$!
sleep 2
# A fresh profile opens on the onboarding dialog, which holds the focus. It
# mounts after the rest of the window; its dismissal opens a chat tab, whose
# composer takes the focus a moment later.
for _ in $(seq 30); do
  page 'const b = [...document.querySelectorAll("button")].find((b) => b.innerText.trim() === "Set up later");
        if (b) b.click(); return !!b' 2>/dev/null | grep -q true && break
  sleep 1
done
sleep 4

# --- A text field outside the terminal: the file finder's input. ---
field='document.querySelector("input[aria-label=\"Find files\"]")'
value() { page "return $field.value" | str; }
focus_field() { # focus_field <value>: focus it, holding <value> as typed, caret at the end
  # The first keys of a session can land before the window has the keyboard,
  # so the typing is retried until it shows.
  for _ in $(seq 10); do
    page "const e = $field; e.focus(); e.value = ''; return document.activeElement === e" | grep -q true \
      || { fail "focus" "could not focus the Find files input"; return 1; }
    keys type "$1"
    [ "$(value)" = "$1" ] && return 0
    sleep 1
  done
  fail "typing" "typed '$1' into the field ten times, it holds '$(value)'"; return 1
}

# 1-2. Super+A selects all of it, Super+C copies it.
focus_field 'alpha beta'
setclip before
keys key super-a
got=$(page "const e = $field; return [e.selectionStart, e.selectionEnd, e.value]" | tr -d '"')
[ "$got" = "[0,10,alpha beta]" ] && pass "Super+A selects all in a text field" \
  || fail "Super+A" "selection/value $got, want [0,10,alpha beta]"
keys key super-c
got=$(clip)
[ "$got" = "alpha beta" ] && pass "Super+C copies from a text field" \
  || fail "Super+C" "clipboard '$got', want 'alpha beta'"

# 3. Super+V pastes, once, and types no "v".
focus_field 'x'
setclip pasted
keys key super-v
got=$(value)
[ "$got" = "xpasted" ] && pass "Super+V pastes into a text field, once" \
  || fail "Super+V" "field '$got', want 'xpasted'"

# 4. Super+X cuts.
focus_field 'gamma'
setclip before
keys key super-a
keys key super-x
got="$(value)|$(clip)"
[ "$got" = "|gamma" ] && pass "Super+X cuts from a text field" \
  || fail "Super+X" "field|clipboard '$got', want '|gamma'"

# 5. Super+Z undoes, Super+Shift+Z redoes. Chromium may undo the two
# backspaces as one step or two. Super+Shift+Z goes through cdpkey: wayvnc
# cannot send Shift with a letter (it strips the modifiers from a lowercase
# keysym and drops an uppercase one). Linux Chromium has no Meta+Shift+Z of
# its own, so a redo here is the menu's.
focus_field 'delta'
keys key bsp key bsp
before=$(value)
keys key super-z
undone=$(value)
cdpkey 12 Z KeyZ 90  # 12 = Meta | Shift
redone=$(value)
case "$before|$undone|$redone" in
  "del|delt|del"|"del|delta|del") pass "Super+Z undoes and Super+Shift+Z redoes in a text field" ;;
  *) fail "undo/redo" "after backspaces|undo|redo: '$before|$undone|$redone'" ;;
esac

# 6. Ctrl editing still works in the field: Chromium's own Ctrl+A/Ctrl+C.
# (A menu grab of them would look the same; cases 9-10 test Ctrl grabs.)
focus_field 'epsilon'
setclip before
keys key ctrl-a
keys key ctrl-c
got=$(clip)
[ "$got" = "epsilon" ] && pass "Ctrl+A, Ctrl+C still edit natively in a text field" \
  || fail "Ctrl in field" "clipboard '$got', want 'epsilon'"

# --- The terminal: Super chords are the table's, Ctrl chords the shell's. ---
# The first tab is the terminal a fresh profile opens with.
focus_term() { page 'const t = document.querySelector("[data-testid=sortable-tab]");
    for (const ty of ["pointerdown", "mousedown", "pointerup", "mouseup", "click"])
      t?.dispatchEvent(new MouseEvent(ty, { bubbles: true, button: 0 }));
    await new Promise((r) => setTimeout(r, 1000));
    const e = [...document.querySelectorAll("textarea[aria-label=\"Terminal input\"]")].find((e) => e.offsetParent);
    e?.focus(); return !!e && document.activeElement === e' | grep -q true; }
if ! focus_term; then
  fail "terminal" "could not focus a terminal"
else
  # 7. Super+V pastes once into the shell.
  setclip 'pasted-once'
  keys type "cat > $T/paste.out" key enter
  keys key super-v
  keys key enter
  keys key ctrl-d
  sleep 1
  got=$(cat "$T/paste.out" 2>/dev/null)
  [ "$got" = "pasted-once" ] && pass "Super+V in the terminal pastes once" \
    || fail "terminal Super+V" "shell read '$got', want 'pasted-once'"

  # 8. Super+A, Super+C copy the terminal's contents. The quotes keep the
  # typed line from matching: only the output does.
  keys type 'echo mark""er42' key enter
  setclip before
  keys key super-a
  keys key super-c
  clip | grep -qx 'marker42' && pass "Super+A, Super+C copy from the terminal" \
    || fail "terminal Super+C" "clipboard has no marker42 line: '$(clip | tail -c 200)'"

  # 9. Ctrl+Q and Ctrl+X reach the shell as bytes: no menu grabs them (quit
  # and cut were CmdOrCtrl upstream). Raw mode, so the tty does not eat ^Q
  # (IXON).
  keys type "stty raw -echo; head -c 2 > $T/ctrl.out; stty sane" key enter
  keys key ctrl-q
  keys key ctrl-x
  sleep 1
  got=$(od -An -tx1 "$T/ctrl.out" 2>/dev/null | tr -d ' \n')
  [ "$got" = "1118" ] && pass "Ctrl+Q, Ctrl+X reach the shell" \
    || fail "terminal Ctrl" "shell read bytes '$got', want 1118"
fi

# --- The window-level chords, from the field. ---
fullscreen() { swaytree | "$node" -e '
  const walk = (n) => n.app_id === "orca" ? n.fullscreen_mode : [...(n.nodes || []), ...(n.floating_nodes || [])].map(walk).find((x) => x !== undefined);
  console.log(walk(JSON.parse(require("fs").readFileSync(0, "utf8"))));'; }
focus_field 'zeta'

# 10. Ctrl+Q does not quit: Orca is still answering after as long as case 16
# gives a clean quit -- and the page did get the key, so it was not lost.
page "window.__ctrlq = 0; window.__countq = (e) => { if (e.ctrlKey && e.key === 'q') window.__ctrlq++ };
  addEventListener('keydown', window.__countq, true); return 1" > /dev/null
keys key ctrl-q
for _ in $(seq 20); do [ -s "$T/orca.exit" ] && break; sleep 1; done
got=$(page 'removeEventListener("keydown", window.__countq, true); return window.__ctrlq' 2>/dev/null)
[ ! -s "$T/orca.exit" ] && [ "$got" = 1 ] && pass "Ctrl+Q does not quit" \
  || fail "Ctrl+Q" "page saw Ctrl+Q ${got:-?} times; Orca exit '$(cat "$T/orca.exit" 2>/dev/null)'"

# 11. Super+Ctrl+F toggles full screen, and back.
keys key super-ctrl-f
on=$(fullscreen)
keys key super-ctrl-f
off=$(fullscreen)
[ "$on|$off" = "1|0" ] && pass "Super+Ctrl+F toggles full screen" \
  || fail "fullscreen" "fullscreen_mode after each press: '$on|$off', want '1|0'"

# 12. A page that binds a menu chord itself gets it, and the menu does not:
# Super+A taken by a keydown listener -- as the shortcut recorder, the file
# explorer's Super+Z, the terminal's Super+C/V/A take theirs -- selects
# nothing in the field.
caret_end="const e = $field; e.focus(); e.setSelectionRange(e.value.length, e.value.length)"
selection="const e = $field; return [e.selectionStart, e.selectionEnd, e.value]"
page "$caret_end;
  window.__mine = (e) => { if (e.metaKey && e.key === 'a') { e.preventDefault(); window.__got = (window.__got || 0) + 1 } };
  addEventListener('keydown', window.__mine, true); return 1" > /dev/null
keys key super-a
sleep 1
got=$(page "removeEventListener('keydown', window.__mine, true); return window.__got || 0" 2>/dev/null)
sel=$(page "$selection" | tr -d '"')
[ "$got|$sel" = "1|[4,4,zeta]" ] && pass "a page's own Super+A wins over the menu's Select All" \
  || fail "page first" "page saw Super+A ${got:-?} times; field selection/value $sel, want [4,4,zeta]"

# 13. Super+Alt+I opens the developer tools.
# Targets whose own page is DevTools -- every target's devtoolsFrontendUrl
# names devtools:// too, so a raw grep would count any new target.
devtools_count() { targets | "$node" -e '
  console.log(JSON.parse(require("fs").readFileSync(0, "utf8")).filter((t) => t.url.startsWith("devtools://")).length)'; }
before=$(devtools_count)
keys key super-alt-i
sleep 2
after=$(devtools_count)
[ "$after" -gt "$before" ] && pass "Super+Alt+I opens the developer tools" \
  || fail "devtools" "devtools targets $before -> $after"
# They open docked, holding the keyboard, and DevTools' own page raises no
# before-input-event, so no Super chord reaches the menu from there (as none
# of Orca's own bindings does). Closed through their debugging target.
for id in $(targets | "$node" -e '
    for (const t of JSON.parse(require("fs").readFileSync(0, "utf8")))
      if (t.url.startsWith("devtools://")) console.log(t.id);'); do
  curl -s "http://127.0.0.1:$port/json/close/$id" > /dev/null
done
sleep 2

# 14. A page cannot click a menu item by logging the report itself: a
# report of an unclaimed Super+A, with no key pressed, selects nothing
# (refused for want of a pending chord, before the token is looked at); nor
# can a frame inside it, with a real Super+A pending -- the page claims the
# chord (so its top frame reports nothing) and an iframe logs a report.
# Electron attributes a same-process iframe's console to the top frame, so
# only the per-document token stops it: the iframe logs the report as it
# would read without one, and with a well-formed wrong one. (A build without
# the token fails this case -- main does receive the iframe's lines.)
page "$caret_end; console.debug('orca-unclaimed-super-chord KeyA a 0 0 0'); return 1" > /dev/null
sleep 1
sel=$(page "$selection" | tr -d '"')
page "$caret_end;
  const f = document.createElement('iframe'); f.id = '__forger'; f.style.display = 'none';
  f.srcdoc = '<script>addEventListener(\"message\", (e) => { console.debug(e.data); parent.__heard = (parent.__heard || 0) + 1 })<\/script>';
  const loaded = new Promise((r) => { f.onload = r }); document.body.append(f); await loaded;
  window.__mine = (e) => { if (e.metaKey && e.key === 'a') {
    e.preventDefault()
    for (const m of ['orca-unclaimed-super-chord KeyA a 0 0 0', 'orca-unclaimed-super-chord ' + crypto.randomUUID() + ' KeyA a 0 0 0'])
      f.contentWindow.postMessage(m, '*') } };
  addEventListener('keydown', window.__mine, true); return 1" > /dev/null
keys key super-a
sleep 1
framed=$(page "$selection" | tr -d '"')
heard=$(page "removeEventListener('keydown', window.__mine, true); document.getElementById('__forger').remove();
  $caret_end; return window.__heard || ''" | tr -d '"')
[ "$heard" = 2 ] && [ "$framed" = "[4,4,zeta]" ] \
  && pass "a report forged from an iframe, with the chord pending, clicks nothing" \
  || fail "framed forgery" "iframe logged ${heard:-0} of 2 reports; field selection/value $framed, want [4,4,zeta]"
# The same from the page's own world -- a browser-pane site's position --
# whose console lines main certainly sees: only the token refuses these.
page "$caret_end;
  window.__got = 0;
  window.__mine = (e) => { if (e.metaKey && e.key === 'a') { e.preventDefault(); window.__got++; setTimeout(() => {
    console.debug('orca-unclaimed-super-chord KeyA a 0 0 0');
    console.debug('orca-unclaimed-super-chord ' + crypto.randomUUID() + ' KeyA a 0 0 0') }) } };
  addEventListener('keydown', window.__mine, true); return 1" > /dev/null
keys key super-a
sleep 1
own=$(page "removeEventListener('keydown', window.__mine, true); const e = $field; const r = [window.__got, e.selectionStart, e.selectionEnd, e.value];
  e.setSelectionRange(e.value.length, e.value.length); return r" | tr -d '"')
[ "$own" = "[1,4,4,zeta]" ] && pass "a report forged by the page itself, with the chord pending, clicks nothing" \
  || fail "page forgery" "page saw Super+A/selection/value $own, want [1,4,4,zeta]"
# The control: the same chord, really pressed, does select -- so the forgeries
# were refused, not lost to a window without focus.
keys key super-a
real=$(page "$selection" | tr -d '"')
[ "$sel|$real" = "[4,4,zeta]|[0,4,zeta]" ] && pass "a token-less report with nothing pending clicks nothing (a real Super+A then selects)" \
  || fail "forged report" "field selection/value after the forgery $sel, after a real Super+A $real; want [4,4,zeta], [0,4,zeta]"

# 15. In Orca's Monaco editor, Super+Z undoes, Super+Shift+Z redoes and
# Super+A selects all -- Monaco's own commands, not the menu's native ones,
# which reach only its hidden textarea. Opened from the file explorer; the
# typing goes in after a real click into the text.
lines='return [...document.querySelectorAll(".monaco-editor .view-lines")].map((v) => v.innerText).join("|")'
# The field still filters the explorer by the cases above; empty it first.
page "const e = $field; e.focus(); e.select(); return 1" > /dev/null
keys key bsp
xy=$(page 'const el = [...document.querySelectorAll("*")].find((e) => e.children.length === 0 && e.textContent.trim() === "a.js");
  if (!el) return "";
  for (const ty of ["pointerdown", "mousedown", "pointerup", "mouseup", "click", "dblclick"])
    el.dispatchEvent(new MouseEvent(ty, { bubbles: true, button: 0 }));
  for (let i = 0; i < 50 && !document.querySelector(".monaco-editor .view-lines"); i++) await new Promise((r) => setTimeout(r, 100));
  const v = document.querySelector(".monaco-editor .view-lines"); if (!v) return "";
  const b = v.getBoundingClientRect(); return Math.round(b.x + 150) + " " + Math.round(b.y + 8)' | tr -d '"')
if [ -z "$xy" ]; then
  fail "editor" "could not open a.js in the editor"
else
  # Screen = page coordinates plus where sway put the window's content (below
  # its title bar). The page rect is measured again each try: the editor is
  # still settling when it first shows.
  origin=$(swaytree | "$node" -e '
    const walk = (n) => n.app_id === "orca" ? n : [...(n.nodes || []), ...(n.floating_nodes || [])].map(walk).find(Boolean);
    const o = walk(JSON.parse(require("fs").readFileSync(0, "utf8")));
    console.log((o.rect.x + o.window_rect.x) + " " + (o.rect.y + o.window_rect.y));')
  for _ in 1 2 3 4 5; do
    sleep 1
    xy=$(page "const b = document.querySelector('.monaco-editor .view-lines').getBoundingClientRect();
      return Math.round(b.x + 150 + ${origin% *}) + ' ' + Math.round(b.y + 8 + ${origin#* })" | tr -d '"')
    keys move $xy click 1
    page 'return !!document.activeElement?.closest?.(".monaco-editor")' | grep -q true && break
  done
  keys key end type xyz
  typed=$(page "$lines" | tr -d '"')
  keys key super-z
  undone=$(page "$lines" | tr -d '"')
  cdpkey 12 Z KeyZ 90  # Meta | Shift
  redone=$(page "$lines" | tr -d '"')
  keys key enter type line2
  setclip before
  keys key super-a
  keys key super-c
  got="$typed|$undone|$redone|$(clip | tr '\n' '/')"
  [ "$got" = "helloxyz|hello|helloxyz|helloxyz/line2/" ] \
    && pass "Super+Z, Super+Shift+Z, Super+A run Monaco's own undo, redo, select-all in the editor" \
    || fail "editor" "typed|undo|redo|copied after Super+A: '$got', want 'helloxyz|hello|helloxyz|helloxyz/line2/'"
  # Saved (editor.save, Super+S), so quitting asks nothing.
  keys key super-s
  sleep 1
  saved=$(tr '\n' '/' < "$T/home/a.js")
  # Super+Shift+A is Orca's editor.addReviewNote, not a menu chord: the
  # editor's Ctrl twin of it would toggle a block comment in this .js file.
  cdpkey 12 A KeyA 65  # Meta | Shift
  after=$(page "$lines" | tr -d '"')
  [ "$saved|$after" = 'helloxyz/line2/|helloxyz\nline2' ] \
    && pass "the editor saves on Super+S, and Super+Shift+A is not turned into Ctrl+Shift+A" \
    || fail "editor extras" "saved file|text after Super+Shift+A: '$saved|$after', want 'helloxyz/line2/|helloxyz\nline2'"
fi

# 16. Super+Q quits, cleanly.
alive=$([ ! -s "$T/orca.exit" ] && echo 1)
page "$field.focus(); return 1" > /dev/null
keys key super-q
for _ in $(seq 20); do [ -s "$T/orca.exit" ] && break; sleep 1; done
code=$(cat "$T/orca.exit" 2>/dev/null)
[ -n "$alive" ] && [ "$code" = 0 ] && pass "Super+Q quits, exit 0" \
  || fail "Super+Q" "alive before: '${alive:-no}', exit status '${code:-none after 20s}'; windows: $(swaytree | "$node" -e '
    const all = []; const walk = (n) => { if (n.pid) all.push(n.name + (n.focused ? "*" : "")); [...(n.nodes || []), ...(n.floating_nodes || [])].forEach(walk); };
    walk(JSON.parse(require("fs").readFileSync(0, "utf8"))); console.log(all.join(", "));' 2>/dev/null)"

# 17. Nothing of this Orca outlives it in the live session.
sleep 2
leaked=$(our_daemons)
[ -z "$leaked" ] && pass "no terminal daemon of this Orca left in the user session" \
  || fail "daemon leak" "left running: $leaked"

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
