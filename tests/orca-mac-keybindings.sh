#!/usr/bin/env bash
# Cases for orca.nix's Mac-style keybindings: Orca's own macOS defaults, with
# Cmd on Super, merged into ~/.orca/keybindings.json at activation.
#
# Two contracts, both silent when broken:
#
#   * **No chord Hyprland owns.** The compositor eats a bound chord before the
#     window sees it, so a Super+F override is not "find", it is a dead key that
#     also hides Orca's working Ctrl+F default. The generator drops those and
#     leaves the action on its Linux default; cases 2-3 pin that against the
#     live Hyprland bind list, not a copy of it.
#
#   * **The merge owns only its keys.** Orca's settings UI writes into the same
#     platforms.linux section, so replacing the section would wipe every
#     shortcut set in the UI at each switch. Cases 5-6.
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
    --apply 'd: builtins.head (builtins.attrNames (builtins.getContext d))' 2>/dev/null) \
    || { echo "no home.activation.orcaMacKeybindings on this host" >&2; exit 1; }
  PKG=$(nix-store --realise "$drv" | tail -1) || { echo "could not build $drv" >&2; exit 1; }
fi
MERGE="$PKG/bin/orca-mac-keybindings"
OWNED="$PKG/share/orca-mac-keybindings.json"
[ -x "$MERGE" ] && [ -f "$OWNED" ] || { echo "$PKG lacks bin/ or share/" >&2; exit 1; }

T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
jqo() { jq -c "$1" "$OWNED"; }

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
      (builtins.filter (k: builtins.substring 0 4 k == "bind") (builtins.attrNames s))' \
  2>/dev/null > "$T/hypr.json" || { echo "could not evaluate Hyprland binds" >&2; exit 1; }
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

# 3. A chord Hyprland owns leaves its action on Orca's Linux default, not unbound.
got=$(jqo 'has("editor.find"), has("worktree.quickOpen")' | tr '\n' ' ')
[ "$got" = "false false " ] && pass "Super+F/Super+P actions keep their Linux defaults" \
  || fail "fallback" "editor.find/worktree.quickOpen overridden: $got"

# 4. Fresh box: the file is created in the shape Orca writes.
H="$T/fresh"; mkdir -p "$H"
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c --slurpfile o "$OWNED" '[.version, (.platforms.linux == $o[0]), .keybindings, .platforms.darwin]' "$H/.orca/keybindings.json" 2>/dev/null)
[ "$got" = '[1,true,{},{}]' ] && pass "fresh HOME gets a version-1 file" || fail "fresh" "got $got"
cp "$H/.orca/keybindings.json" "$T/first.json"
HOME=$H "$MERGE" >/dev/null 2>&1
cmp -s "$T/first.json" "$H/.orca/keybindings.json" && pass "second switch changes nothing" \
  || fail "idempotent" "re-running the merge rewrote the file"

# 5. Owned keys win; UI edits to other actions, common and darwin survive.
H="$T/edited"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{"tab.rename":["Alt+R"]},
 "platforms":{"darwin":{"tab.close":["Cmd+Q"]},"linux":{"tab.close":["Ctrl+Q"],"editor.find":["Alt+F"]},"win32":{}}}
EOF
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '[.platforms.linux["tab.close"], .platforms.linux["editor.find"], .keybindings, .platforms.darwin]' "$H/.orca/keybindings.json")
[ "$got" = '[["Cmd+W"],["Alt+F"],{"tab.rename":["Alt+R"]},{"tab.close":["Cmd+Q"]}]' ] \
  && pass "merge keeps UI edits outside its keys" || fail "merge" "got $got"

# 6. A file Orca cannot parse is left alone; activation does not fail on it.
H="$T/broken"; mkdir -p "$H/.orca"; printf '{not json' > "$H/.orca/keybindings.json"
if HOME=$H "$MERGE" >/dev/null 2>&1 && [ "$(cat "$H/.orca/keybindings.json")" = '{not json' ] \
   && [ ! -e "$H/.orca/keybindings.json.new" ]; then
  pass "unparseable file untouched, exit 0"
else
  fail "broken" "file changed, .new left behind, or non-zero exit"
fi

# 7. A key an earlier generation wrote, and this one no longer emits (Hyprland
#    took the chord, say), is removed -- unless the UI has changed it since.
#    A generated key the UI changed after it was written stays the UI's.
H="$T/stale"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{},
 "platforms":{"darwin":{},"linux":{"editor.find":["Cmd+F"],"worktree.quickOpen":["Alt+P"],"tab.close":["Ctrl+Q"]},"win32":{}}}
EOF
echo '{"linux":{"editor.find":["Cmd+F"],"worktree.quickOpen":["Cmd+P"],"tab.close":["Cmd+W"]}}' \
  > "$H/.orca/keybindings.nix-owned.json"
HOME=$H "$MERGE" >/dev/null 2>&1
got=$(jq -c '[.platforms.linux["editor.find"], .platforms.linux["worktree.quickOpen"], .platforms.linux["tab.close"]]' "$H/.orca/keybindings.json")
[ "$got" = '[null,["Alt+P"],["Ctrl+Q"]]' ] && pass "stale generated key dropped, UI-changed ones kept" \
  || fail "stale" "got $got"

# 8. A UI shortcut on a generated chord: Orca would drop *both* actions at load.
#    The UI's choice stands; the generated action falls back to its default.
H="$T/clash"; mkdir -p "$H/.orca"
cat > "$H/.orca/keybindings.json" <<'EOF'
{"version":1,"keybindings":{},"platforms":{"darwin":{},"linux":{"tab.newBrowser":["Cmd+T"]},"win32":{}}}
EOF
err=$(HOME=$H "$MERGE" 2>&1 >/dev/null)
got=$(jq -c '[.platforms.linux["tab.newBrowser"], .platforms.linux["tab.newTerminal"]]' "$H/.orca/keybindings.json")
if [ "$got" = '[["Cmd+T"],null]' ] && [[ $err == *tab.newTerminal* ]]; then
  pass "UI chord wins a clash, and the dropped key is named"
else
  fail "clash" "got $got, stderr '$err'"
fi

# 9. An empty file is not JSON either: left alone, not "merged" into nothing.
H="$T/empty"; mkdir -p "$H/.orca"; : > "$H/.orca/keybindings.json"
if HOME=$H "$MERGE" >/dev/null 2>&1 && [ ! -s "$H/.orca/keybindings.json" ]; then
  pass "empty file untouched, exit 0"
else
  fail "empty" "file rewritten or non-zero exit"
fi

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
