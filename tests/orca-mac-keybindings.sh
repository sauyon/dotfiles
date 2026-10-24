#!/usr/bin/env bash
# Cases for orca.nix's Mac-style keybindings: Orca's own macOS defaults, with
# Cmd on Super, merged into ~/.orca/keybindings.json at activation.
#
# Four contracts, all silent when broken:
#
#   * **No chord Hyprland owns.** The compositor eats a bound chord before the
#     window sees it, so a Super+F override is not "find", it is a dead key.
#     The generator unbinds those actions; case 2 pins that against the live
#     Hyprland bind list, not a copy of it, and 11-12f the digit family and
#     how Hyprland's bind forms translate.
#
#   * **Ctrl and Meta stay the terminal's.** The point of Cmd-on-Super is that
#     emacs keys reach the shell. No Ctrl- or Alt-only chord is left, from the
#     Mac table or from a Linux default, beyond the few pinned in orca.nix
#     (find on Ctrl+S) and the tab-cycling chords terminals cannot send.
#     Cases 3 and 13.
#
#   * **The merge owns only its keys.** Orca's settings UI writes into the same
#     platforms.linux section, so replacing the section would wipe every
#     shortcut set in the UI at each switch. Cases 4-9 and 14: ownership via
#     the sidecar, stale keys, clashes with UI shortcuts, files left alone.
#
#   * **Activation never fails on it.** Case 10.
#
# The merge runs on a throwaway HOME, never the real ~/.orca.
#
#   tests/orca-mac-keybindings.sh                   evaluate + build, then test
#   tests/orca-mac-keybindings.sh <merge-package>   test a given build
set -u
set -o pipefail

fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }

FLAKE=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
HM="$FLAKE#homeConfigurations.$(hostname).config"

PKG="${1:-}"
if [ -z "$PKG" ]; then
  # The activation entry's string context names the merge package's drv.
  drv=$(nix eval --raw "$HM.home.activation.orcaMacKeybindings.data" \
    --apply 'd: builtins.head (builtins.attrNames (builtins.getContext d))') \
    || { echo "could not evaluate home.activation.orcaMacKeybindings on this host (above)" >&2; exit 1; }
  PKG=$(nix-store --realise "$drv" | tail -1) || { echo "could not build $drv" >&2; exit 1; }
fi
MERGE="$PKG/bin/orca-mac-keybindings"
OWNED="$PKG/share/orca-mac-keybindings.json"
[ -x "$MERGE" ] && [ -f "$OWNED" ] || { echo "$PKG lacks bin/ or share/" >&2; exit 1; }

T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
jqo() { jq -c "$1" "$OWNED"; }
NODE=$(grep -oE '/nix/store/[^ "]+/bin/node' "$MERGE" | head -1)

# 1. The Mac table, on Super: clipboard and tabs are the reason this exists.
for c in 'terminal.copySelection:["Cmd+C"]' 'terminal.paste:["Cmd+V"]' \
         'tab.close:["Cmd+W"]' 'tab.newTerminal:["Cmd+T"]'; do
  id=${c%%:*}; want=${c#*:}
  got=$(jqo ".[\"$id\"]")
  [ "$got" = "$want" ] && pass "$id = $want" || fail "$id" "got $got, want $want"
done
got=$(jqo '[.[][] | select(test("(^|\\+)Mod\\+"))]')
[ "$got" = "[]" ] && pass "no Mod (= Ctrl on Linux) left" || fail "Mod" "still bound: $got"

# 2. Nothing the generator emits is a Hyprland chord.
nix eval --json "$HM.wayland.windowManager.hyprland.settings" --apply \
  's: builtins.concatMap (k: map (b: if builtins.isAttrs b && b ? _args then builtins.head b._args else b) s.${k})
      (builtins.filter (k: builtins.substring 0 4 k == "bind" && builtins.isList s.${k}) (builtins.attrNames s))' \
  > "$T/hypr.json" || { echo "could not evaluate Hyprland binds (above)" >&2; exit 1; }
# Hyprland "SUPER + SHIFT + W" -> Orca "Cmd+Shift+W"; letters upper-cased, the
# keys Orca spells differently renamed. Mouse and switch binds never match.
jq -r '.[] | split(" + ") | map(
    {SUPER:"Cmd",CTRL:"Ctrl",ALT:"Alt",SHIFT:"Shift",
     return:"Enter",left:"ArrowLeft",right:"ArrowRight",up:"ArrowUp",down:"ArrowDown",
     space:"Space",semicolon:"Semicolon"}[.] // (if length == 1 then ascii_upcase else . end))
  | join("+")' "$T/hypr.json" | sort -u > "$T/hypr.txt"
grep -qx 'Cmd+F' "$T/hypr.txt" || { echo "Hyprland bind list lacks Super+F; harness is broken" >&2; exit 1; }
jq -r '.[][]' "$OWNED" | sort -u > "$T/owned.txt"
clash=$(comm -12 "$T/hypr.txt" "$T/owned.txt" | tr '\n' ' ')
[ -z "$clash" ] && pass "no override is a Hyprland chord" || fail "hyprland" "both bind: $clash"

# 3. Find is Ctrl+S outside the terminal; in it, and for go-to-file, nothing
#    -- C-s and C-p are the shell's. (Super+F/P are Hyprland's.)
got=$(jqo '[.["editor.find"], .["browser.find"], .["chat.find"], .["settings.search"], .["terminal.search"], .["worktree.quickOpen"]]')
[ "$got" = '[["Ctrl+S"],["Ctrl+S"],["Ctrl+S"],["Ctrl+S"],[],[]]' ] \
  && pass "find on Ctrl+S, terminal search and go-to-file unbound" || fail "find" "got $got"

# 4. Fresh box: the file is created in the shape Orca writes.
H="$T/fresh"; mkdir -p "$H"
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c --slurpfile o "$OWNED" '[.version, (.platforms.linux == $o[0]), .keybindings, .platforms.darwin]' "$H/.orca/keybindings.json" 2>/dev/null)
[ "$got" = '[1,true,{},{}]' ] && pass "fresh HOME gets a version-1 file" || fail "fresh" "got $got"
cp "$H/.orca/keybindings.json" "$T/first.json"
HOME=$H "$MERGE" >/dev/null 2>&1
cmp -s "$T/first.json" "$H/.orca/keybindings.json" && pass "second switch changes nothing" \
  || fail "idempotent" "re-running the merge rewrote the file"

# 5. First merge into a file Orca already wrote (no sidecar): a value already
#    on a generated action is the UI's, kept; the rest of the table goes in.
#    Common and darwin are untouched. Deleting a bad sidecar (case 14) lands
#    here, so it must not cost the UI's shortcuts.
H="$T/edited"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{"tab.rename":["Alt+R"]},
 "platforms":{"darwin":{"tab.close":["Cmd+Q"]},"linux":{"tab.close":["Ctrl+Q"],"editor.find":["Alt+F"]},"win32":{}}}
EOF
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '[.platforms.linux["tab.close"], .platforms.linux["tab.newTerminal"], .platforms.linux["editor.find"], .keybindings, .platforms.darwin]' "$H/.orca/keybindings.json")
[ "$got" = '[["Ctrl+Q"],["Cmd+T"],["Alt+F"],{"tab.rename":["Alt+R"]},{"tab.close":["Cmd+Q"]}]' ] \
  && pass "first merge keeps the UI's values, adds the rest" || fail "merge" "got $got"

# 6. A file Orca cannot parse is left alone; activation does not fail on it.
H="$T/broken"; mkdir -p "$H/.orca"; printf '{not json' > "$H/.orca/keybindings.json"
if HOME=$H "$MERGE" >/dev/null 2>&1 && [ "$(cat "$H/.orca/keybindings.json")" = '{not json' ] \
   && [ -z "$(ls "$H/.orca" | grep -v "^keybindings.json$")" ]; then
  pass "unparseable file untouched, exit 0"
else
  fail "broken" "file changed, a temp file left behind, or non-zero exit"
fi

# 7. A key an earlier generation wrote, and this one no longer emits (Hyprland
#    took the chord, say), is removed -- unless the UI has changed it since.
#    A generated key the UI changed after it was written stays the UI's.
H="$T/stale"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{},
 "platforms":{"darwin":{},"linux":{"tab.newAgent.claude":["Cmd+F"],"worktree.quickOpen":["Alt+P"],"tab.close":["Ctrl+Q"]},"win32":{}}}
EOF
echo '{"linux":{"tab.newAgent.claude":["Cmd+F"],"worktree.quickOpen":["Cmd+P"],"tab.close":["Cmd+W"]}}' \
  > "$H/.orca/keybindings.nix-owned.json"
# Twice: the second run reads the sidecar the first wrote, which must still
# know the UI's keys as changed rather than as never owned.
HOME=$H "$MERGE" >/dev/null 2>&1; HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '[.platforms.linux["tab.newAgent.claude"], .platforms.linux["worktree.quickOpen"], .platforms.linux["tab.close"]]' "$H/.orca/keybindings.json")
[ "$got" = '[null,["Alt+P"],["Ctrl+Q"]]' ] && pass "stale generated key dropped, UI-changed ones kept" \
  || fail "stale" "got $got"

# 7b. A UI value that happens to equal this generation's is still the UI's:
#     the sidecar keeps the value last written, so a later generation that
#     drops the key does not take the user's shortcut with it.
H="$T/coincide"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{},"platforms":{"darwin":{},"linux":{"tab.close":["Cmd+W"]},"win32":{}}}
EOF
echo '{"linux":{"tab.close":["Cmd+X"]}}' > "$H/.orca/keybindings.nix-owned.json"
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '.linux["tab.close"]' "$H/.orca/keybindings.nix-owned.json")
[ "$got" = '["Cmd+X"]' ] && pass "UI value equal to a generated one stays the UI's" \
  || fail "coincide" "sidecar now records $got"

# 7c. Ownership is per key: an action newly generated since the last switch
#     (Hyprland freed its chord, an Orca bump added it) that the UI already
#     set is the UI's, though the sidecar exists.
H="$T/newly"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{},"platforms":{"darwin":{},"linux":{"tab.close":["Cmd+W"],"tab.newTerminal":["Alt+N"]},"win32":{}}}
EOF
echo '{"linux":{"tab.close":["Cmd+W"]}}' > "$H/.orca/keybindings.nix-owned.json"
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '.platforms.linux["tab.newTerminal"]' "$H/.orca/keybindings.json")
[ "$got" = '["Alt+N"]' ] && pass "UI value on a newly generated action stays the UI's" \
  || fail "newly" "got $got"

# 7d. A generated key reset in the UI (absent, though claimed) comes back:
#     the reset target is this table.
H="$T/reset"; mkdir -p "$H/.orca"
echo '{"version":1,"keybindings":{},"platforms":{"darwin":{},"linux":{},"win32":{}}}' > "$H/.orca/keybindings.json"
echo '{"linux":{"tab.close":["Cmd+W"]}}' > "$H/.orca/keybindings.nix-owned.json"
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '.platforms.linux["tab.close"]' "$H/.orca/keybindings.json")
[ "$got" = '["Cmd+W"]' ] && pass "UI reset of a generated key re-applies it" || fail "reset" "got $got"

# 8. A UI shortcut on a generated chord: Orca would drop *both* actions at load.
#    The UI's choice stands; the generated action falls back to its default.
H="$T/clash"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{},"platforms":{"darwin":{},"linux":{"tab.newBrowser":["Cmd+T"]},"win32":{}}}
EOF
err=$(HOME=$H "$MERGE" 2>&1 >/dev/null)
got=$(jq -c '[.platforms.linux["tab.newBrowser"], .platforms.linux["tab.newTerminal"]]' "$H/.orca/keybindings.json")
# Withdrawn means unbound ([]), not deleted: deleted would hand the action
# back to Orca's Linux default, Ctrl+T, which is the terminal's.
if [ "$got" = '[["Cmd+T"],[]]' ] && [[ $err == *tab.newTerminal* ]]; then
  pass "UI chord wins a clash, and the dropped key is named"
else
  fail "clash" "got $got, stderr '$err'"
fi

# 8b. A withdrawn key puts back whatever the UI had there, and keeps it on
#     the next switch, instead of deleting it.
H="$T/clash-restore"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{},"platforms":{"darwin":{},"linux":{"tab.newBrowser":["Cmd+T"],"tab.newTerminal":["Alt+N"]},"win32":{}}}
EOF
HOME=$H "$MERGE" >/dev/null 2>&1; HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '[.platforms.linux["tab.newBrowser"], .platforms.linux["tab.newTerminal"]]' "$H/.orca/keybindings.json")
[ "$got" = '[["Cmd+T"],["Alt+N"]]' ] && pass "withdrawn key restores the UI's value, across switches" \
  || fail "clash-restore" "got $got"

# 8c. A shortcut set in the UI on an action withdrawn by an earlier switch is
#     the UI's, not a free slot.
H="$T/clash-later"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{},"platforms":{"darwin":{},"linux":{"tab.newBrowser":["Cmd+T"]},"win32":{}}}
EOF
HOME=$H "$MERGE" >/dev/null 2>&1
jq '.platforms.linux["tab.newTerminal"] = ["Cmd+Shift+Y"]' "$H/.orca/keybindings.json" > "$T/k" \
  && mv "$T/k" "$H/.orca/keybindings.json"
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '.platforms.linux["tab.newTerminal"]' "$H/.orca/keybindings.json")
[ "$got" = '["Cmd+Shift+Y"]' ] && pass "UI shortcut on a withdrawn action survives" \
  || fail "clash-later" "got $got"

# 8d. The clash check reads values the way Orca's loader does, shorthand and
#     all: "Cmd+T, Alt+B" is two chords, one of them a generated key's.
H="$T/clash-shorthand"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{},"platforms":{"darwin":{},"linux":{"tab.newBrowser":"Cmd+T, Alt+B"},"win32":{}}}
EOF
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '[.platforms.linux["tab.newBrowser"], .platforms.linux["tab.newTerminal"]]' "$H/.orca/keybindings.json")
[ "$got" = '["Cmd+T, Alt+B",[]]' ] && pass "comma-shorthand UI value still wins the clash" \
  || fail "clash-shorthand" "got $got"

# 8e. Sections parse separately, then layer, as Orca's loader does: a bad
#     linux value is dropped and the common one under it still clashes.
H="$T/layer"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{"tab.newBrowser":["Cmd+T"]},"platforms":{"darwin":{},"linux":{"tab.newBrowser":"bogus+"},"win32":{}}}
EOF
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '.platforms.linux["tab.newTerminal"]' "$H/.orca/keybindings.json")
[ "$got" = '[]' ] && pass "common value under a bad linux one still wins its clash" \
  || fail "layer" "tab.newTerminal got $got"

# 9. An empty file is not JSON either: left alone, not "merged" into nothing.
H="$T/empty"; mkdir -p "$H/.orca"; : > "$H/.orca/keybindings.json"
if HOME=$H "$MERGE" >/dev/null 2>&1 && [ ! -s "$H/.orca/keybindings.json" ]; then
  pass "empty file untouched, exit 0"
else
  fail "empty" "file rewritten or non-zero exit"
fi

# 10. An IO failure is one line and exit 0: activation runs under set -e, and
#     a keybinding file is no reason to abandon the rest of the switch.
H="$T/io"; mkdir -p "$H"; : > "$H/.orca"
err=$(HOME=$H "$MERGE" 2>&1 >/dev/null); rc=$?
if [ "$rc" -eq 0 ] && [[ $err == orca:* ]] && [ "$(printf '%s\n' "$err" | wc -l)" -eq 1 ]; then
  pass "IO failure warns once, exit 0"
else
  fail "io" "exit $rc, stderr '$err'"
fi

# 11-12. The generator on fixture chords: a digit-index action's one binding
#        stands for 1-9 -- one taken digit unbinds the family, rather than
#        leaving it on Ctrl+1..9 -- and a bind form it cannot read fails.
gen() { echo "$1" > "$T/chords.json"; "$NODE" "$FLAKE/nix/orca-mac-keybindings.js" "$PKG/lib/shared" "$T/chords.json" 2>"$T/gen.err"; }
got=$(gen '[]' | jq -c '.["workspace.selectByIndex"]'),$(gen '["SUPER + 5"]' | jq -c '.["workspace.selectByIndex"]')
[ "$got" = '["Cmd+1"],[]' ] && pass "Super+5 taken unbinds the whole Cmd+1..9 family" \
  || fail "digits" "free,taken got $got"
if gen '["HYPER + T"]' >/dev/null; then fail "unknown-bind" "HYPER + T passed as a free chord"
elif grep -q 'unknown modifier in Hyprland bind "HYPER + T"' "$T/gen.err"; then pass "unknown Hyprland modifier fails the generator"
else fail "unknown-bind" "failed for another reason: $(cat "$T/gen.err")"; fi

# 12b. Binds that can never be an Orca chord do not fail the build -- a bare
#      Print screenshot key, Scroll_Lock, a bare F7 -- while Hyprland's
#      aliases still count: MOD4 + T is Super+T, tab.newTerminal's.
if out=$(gen '["Print", "SUPER + Scroll_Lock", "MOD4 + T", "F7"]'); then
  got=$(jq -c '.["tab.newTerminal"]' <<<"$out")
  [ "$got" = '[]' ] && pass "unmappable binds skipped, Hyprland aliases taken" \
    || fail "aliases" "tab.newTerminal under MOD4 + T got $got"
else
  fail "aliases" "generator failed: $(cat "$T/gen.err")"
fi

# 12c. Keysyms Orca spells differently are still Hyprland's, in either case:
#      KP_Add is NumpadAdd (zoom.in's), prior is PageUp.
#      A bare Delete is fileExplorer.delete's bare chord.
out=$(gen '["SUPER + KP_Add", "SUPER + prior", "Delete"]')
got=$(jq -c '[.["zoom.in"], .["tab.previousTerminal"], .["fileExplorer.delete"]]' <<<"$out")
[ "$got" = '[["Cmd+Equal","Cmd+Shift+Plus"],null,["Cmd+Backspace"]]' ] \
  && pass "keysym aliases (KP_Add, lowercase prior) and a bare Delete taken" \
  || fail "keysyms" "zoom.in/tab.previousTerminal/fileExplorer.delete got $got"

# 12e. Hyprland matches the unshifted keysym; Orca sees the shifted key. So
#      SUPER + SHIFT + equal is the press Orca reads as Cmd+Shift+Plus.
#      META is a Super alias, and media keysyms skip in any case.
out=$(gen '["SUPER + SHIFT + equal", "META + T", "xf86audiomute"]')
got=$(jq -c '[.["zoom.in"], .["tab.newTerminal"]]' <<<"$out")
[ "$got" = '[["Cmd+Equal","Cmd+NumpadAdd"],[]]' ] && pass "Shift+equal takes Cmd+Shift+Plus; META, xf86 handled" \
  || fail "shifted" "zoom.in/tab.newTerminal got $got"

# 12f. A keycode bind is layout-dependent, so it is refused by name rather
#      than steered into KEYS or NO_CHORD.
if gen '["SUPER + code:41"]' >/dev/null; then fail "keycode" "code:41 passed"
elif grep -q 'keycode' "$T/gen.err"; then pass "keycode bind fails with its own message"
else fail "keycode" "failed for another reason: $(cat "$T/gen.err")"; fi

# 12d. A key it does not know is not assumed harmless: it may be an Orca
#      chord under a spelling not mapped yet, so the build fails.
if gen '["SUPER + Frobnicate"]' >/dev/null; then fail "unknown-key" "SUPER + Frobnicate passed as harmless"
elif grep -q 'Hyprland bind "SUPER + Frobnicate"' "$T/gen.err"; then pass "unknown Hyprland key fails the generator"
else fail "unknown-key" "failed for another reason: $(cat "$T/gen.err")"; fi

# 13. Across the whole effective table -- generated keys over Orca's Linux
#     defaults -- no chord is Ctrl- or Alt-only, save the pinned finds and the
#     tab cycling terminals cannot send (Ctrl+Tab, Ctrl+PageUp/Down).
got=$("$NODE" -e '
  const kb = require(process.argv[1] + "/keybindings.js"), o = require(process.argv[2]);
  const ok = new Set(["Ctrl+Tab", "Ctrl+PageUp", "Ctrl+PageDown"]);
  const pinned = { "editor.find": "Ctrl+S", "browser.find": "Ctrl+S", "chat.find": "Ctrl+S", "settings.search": "Ctrl+S" };
  const bad = kb.KEYBINDING_DEFINITIONS.flatMap((d) =>
    (o[d.id] ?? kb.getDefaultBindings(d, "linux"))
      .filter((b) => /(^|\+)(Mod|Ctrl|Alt)\+/.test(b) && !/(^|\+)Cmd\+/.test(b) && !ok.has(b) && pinned[d.id] !== b)
      .map((b) => d.id + "=" + b));
  console.log(bad.join(" "));' "$PKG/lib/shared" "$OWNED")
[ -z "$got" ] && pass "no Ctrl/Alt-only chord left for the terminal to lose" || fail "terminal-keys" "still bound: $got"

# 14. A sidecar that cannot be read stops the merge: read as empty, it would
#     let every generated key overwrite the UI's changes to it.
H="$T/bad-sidecar"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{},"platforms":{"darwin":{},"linux":{"tab.close":["Ctrl+Q"]},"win32":{}}}
EOF
printf '{bad' > "$H/.orca/keybindings.nix-owned.json"
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '.platforms.linux["tab.close"]' "$H/.orca/keybindings.json")
[ "$got" = '["Ctrl+Q"]' ] && pass "unreadable sidecar: nothing overwritten" || fail "bad-sidecar" "got $got"

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
