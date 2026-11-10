#!/usr/bin/env bash
# Cases for the per-window keyboard layout hook (window.active in hyprland.nix):
# QWERTY while Warframe has focus, Colemak everywhere else.
#
# Like hyprland-zen-popup.sh, these load the *generated* hyprland.lua under a
# stub `hl`, so what is under test is the Lua that ships. The stub records every
# hl.device call as "<name> variant=<v> options=<o>" and lets each case fire
# synthetic window.active events.
#
#   ./tests/hyprland-kb-layout.sh                    # evaluates this host's config
#   ./tests/hyprland-kb-layout.sh /path/hyprland.lua # tests one you already have
set -u

CONFIG="${1:-}"
if [ -z "$CONFIG" ]; then
  host=$(cat /etc/hostname)
  CONFIG=$(nix build --no-link --print-out-paths \
    ".#homeConfigurations.$host.config.xdg.configFile.\"hypr/hyprland.lua\".source") \
    || { echo "could not build hyprland.lua for $host" >&2; exit 1; }
  echo "testing $CONFIG"
fi
[ -r "$CONFIG" ] || { echo "not readable: $CONFIG" >&2; exit 1; }
command -v lua >/dev/null 2>&1 || { echo "need a lua interpreter on PATH" >&2; exit 1; }

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

cat > "$D/harness.lua" <<'LUA'
local configPath, eventsPath = arg[1], arg[2]
local trace, handlers = {}, {}

local function dsp(path)
  return setmetatable({}, {
    __index = function(_, k) return dsp(path .. "." .. k) end,
    __call  = function(_, a) return { path = path, args = a } end,
  })
end
local hl = setmetatable({}, { __index = function(_, k) return dsp(k) end })

function hl.on(event, cb)
  handlers[#handlers + 1] = { event = event, cb = cb }
  return { remove = function() end }
end

-- The static `device` list also goes through hl.device at load time; only
-- calls made after load (from a handler) are the hook's, so start recording
-- once the config has run.
local recording = false
function hl.device(spec)
  if recording then
    trace[#trace + 1] = spec.name .. " variant=" .. spec.kb_variant .. " options=" .. spec.kb_options
  end
end

function _G.emit(event, w)
  for _, h in ipairs(handlers) do
    if h.event == event then h.cb(w) end
  end
end

_G.hl = hl
dofile(configPath)
recording = true
dofile(eventsPath)
for _, line in ipairs(trace) do print(line) end
LUA

check() {
  local name=$1 want=$2 got
  cat > "$D/events.lua"
  got=$(lua "$D/harness.lua" "$CONFIG" "$D/events.lua" 2>&1)
  n=$((n + 1))
  if [ "$got" = "$want" ]; then
    printf 'ok %d - %s\n' "$n" "$name"
  else
    printf 'FAIL %d - %s\n      want: [%s]\n      got:  [%s]\n' "$n" "$name" "$want" "$got"
    fails=$((fails + 1))
  fi
}

OPTS='ctrl:nocaps,ctrl:swapcaps'
QWERTY="framework-laptop-16-keyboard-module---ansi-keyboard variant= options=$OPTS
at-translated-set-2-keyboard variant= options=$OPTS"
COLEMAK="framework-laptop-16-keyboard-module---ansi-keyboard variant=colemak options=$OPTS
at-translated-set-2-keyboard variant=colemak options=$OPTS"

check "focusing warframe switches every keyboard to qwerty" "$QWERTY" <<'LUA'
emit("window.active", { class = "steam_app_230410" })
LUA

# Kept options matter: hl.device replaces the keymap wholesale, so dropping
# kb_options here would silently lose Caps-as-Ctrl inside the game.
check "leaving warframe restores colemak with options intact" "$QWERTY
$COLEMAK" <<'LUA'
emit("window.active", { class = "steam_app_230410" })
emit("window.active", { class = "com.mitchellh.ghostty" })
LUA

# The cache: Colemak-to-Colemak focus changes are the common case and must not
# recompile the keymap each time. The first event still applies once, since a
# fresh Lua state does not know what is live.
check "colemak-to-colemak focus changes apply once" "$COLEMAK" <<'LUA'
emit("window.active", { class = "com.mitchellh.ghostty" })
emit("window.active", { class = "zen-beta" })
emit("window.active", { class = "Vesktop" })
LUA

# Focus can land on nothing (last window closed, empty workspace).
check "nil window is treated as colemak" "$COLEMAK" <<'LUA'
emit("window.active", nil)
LUA

printf '\n%d/%d passed\n' "$((n - fails))" "$n"
[ "$fails" -eq 0 ]
