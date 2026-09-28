#!/usr/bin/env bash
# Cases for the Zen extension-popup float handler (defined in hyprland.nix).
#
# Why a handler and not a window rule: `float` is a *static* effect in Hyprland
# -- evaluated once at map time, against initialTitle/initialClass, never again.
# Gecko maps an extension popup as an ordinary browser window and only then
# renames it to "Extension: (...)", so by the time the title exists the static
# rule pass is long over. Upstream declined to make `float` dynamic
# (hyprwm/Hyprland#3835, closed as not planned; #602 is the same bug), and the
# wiki's own advice is to dispatch from an event listener instead. That listener
# is what these cases drive.
#
# They load the *generated* hyprland.lua under a stub `hl`, so what is under test
# is the Lua that actually ships, not a copy of it. The stub records dispatches
# and lets each case fire synthetic window.open / window.title events.
#
#   ./tests/hyprland-zen-popup.sh                   # evaluates this host's config
#   ./tests/hyprland-zen-popup.sh /path/hyprland.lua # tests one you already have
set -u

CONFIG="${1:-}"
if [ -z "$CONFIG" ]; then
  host=$(cat /etc/hostname)
  # build, not eval: the attribute is a derivation, so evaluating it yields a
  # store path that does not exist yet.
  CONFIG=$(nix build --no-link --print-out-paths \
    ".#homeConfigurations.$host.config.xdg.configFile.\"hypr/hyprland.lua\".source") \
    || { echo "could not build hyprland.lua for $host" >&2; exit 1; }
  echo "testing $CONFIG"
fi
[ -r "$CONFIG" ] || { echo "not readable: $CONFIG" >&2; exit 1; }
command -v lua >/dev/null 2>&1 || { echo "need a lua interpreter on PATH" >&2; exit 1; }

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

# The stub. Every hl.* call is a no-op recorder except hl.on (which registers a
# handler and hands back a :remove()-able subscription) and hl.dispatch (which
# appends to the trace). That is enough surface for the whole generated config to
# load without the compositor.
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
  local entry = { event = event, cb = cb, alive = true }
  handlers[#handlers + 1] = entry
  return { remove = function() entry.alive = false end }
end

-- Render a dispatch as "<path> k=v k=v", keys sorted so the trace is stable.
-- A window argument collapses to <window>: identity is already pinned by the
-- address guard the cases exercise, and printing the table would not be stable.
local function fmt(d)
  if type(d) ~= "table" or not d.path then return tostring(d) end
  local keys = {}
  if type(d.args) == "table" then
    for k in pairs(d.args) do keys[#keys + 1] = k end
    table.sort(keys)
  end
  local parts = {}
  for _, k in ipairs(keys) do
    local v = d.args[k]
    parts[#parts + 1] = k .. "=" .. (type(v) == "table" and "<window>" or tostring(v))
  end
  return d.path .. (#parts > 0 and (" " .. table.concat(parts, " ")) or "")
end

function hl.dispatch(d) trace[#trace + 1] = fmt(d) end

-- Snapshot before dispatching: a handler may subscribe from inside a callback
-- (window.open registering a window.title listener), and that new entry must not
-- run for the event currently being delivered.
function _G.emit(event, w)
  local snapshot = {}
  for i, h in ipairs(handlers) do snapshot[i] = h end
  for _, h in ipairs(snapshot) do
    if h.alive and h.event == event then h.cb(w) end
  end
end

_G.hl = hl
dofile(configPath)
dofile(eventsPath)
for _, line in ipairs(trace) do print(line) end
LUA

# check <name> <expected-trace>; the case's event script arrives on stdin.
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

POPUP='Extension: (Bitwarden - Free Password Manager) - Bitwarden — Zen Browser'

# The case the whole handler exists for: Zen maps a generic window, renames it.
check "zen extension popup floats on rename" 'dsp.window.float action=enable window=<window>' <<LUA
local w = { class = "zen-beta", title = "New Tab — Zen Browser",
            initial_title = "Zen Browser", address = "0x1" }
emit("window.open", w)
w.title = [[$POPUP]]
emit("window.title", w)
LUA

# Ordinary browsing renames the window constantly; none of it is a popup.
check "ordinary zen title change does not float" '' <<'LUA'
local w = { class = "zen-beta", title = "New Tab — Zen Browser",
            initial_title = "Zen Browser", address = "0x1" }
emit("window.open", w)
w.title = "Hyprland Wiki — Zen Browser"
emit("window.title", w)
LUA

# The class guard: another app is free to put "Extension: " in its title.
check "non-zen window with popup title is ignored" '' <<LUA
local w = { class = "com.mitchellh.ghostty", title = "shiori: dotfiles",
            initial_title = "ghostty", address = "0x2" }
emit("window.open", w)
w.title = [[$POPUP]]
emit("window.title", w)
LUA

# The address guard: window.title is global, so a rename on any *other* window
# must not float the one whose open we saw.
check "title event for a different window is ignored" '' <<LUA
local opened = { class = "zen-beta", title = "New Tab — Zen Browser",
                 initial_title = "Zen Browser", address = "0x1" }
emit("window.open", opened)
local other = { class = "zen-beta", title = [[$POPUP]],
                initial_title = "Zen Browser", address = "0x99" }
emit("window.title", other)
LUA

# The subscription is per-window and one-shot. Gecko renames a popup more than
# once (the extension sets its own document title after load); without the
# unsubscribe every later rename would re-dispatch float.
check "handler fires once, then unsubscribes" 'dsp.window.float action=enable window=<window>' <<LUA
local w = { class = "zen-beta", title = "New Tab — Zen Browser",
            initial_title = "Zen Browser", address = "0x1" }
emit("window.open", w)
w.title = [[$POPUP]]
emit("window.title", w)
emit("window.title", w)
LUA

# Two popups at once each get their own subscription.
check "two concurrent zen windows float independently" \
  'dsp.window.float action=enable window=<window>
dsp.window.float action=enable window=<window>' <<LUA
local a = { class = "zen-beta", title = "New Tab — Zen Browser",
            initial_title = "Zen Browser", address = "0xa" }
local b = { class = "zen-beta", title = "New Tab — Zen Browser",
            initial_title = "Zen Browser", address = "0xb" }
emit("window.open", a)
emit("window.open", b)
a.title = [[$POPUP]]
emit("window.title", a)
b.title = [[$POPUP]]
emit("window.title", b)
LUA

# A Zen window that never becomes a popup still holds a window.title
# subscription. Hyprland addresses are heap pointers and do get recycled, so a
# subscription outliving its window is not merely untidy: the next window handed
# the same address inherits it, and the first rename that looks like a popup
# floats the wrong window. Closing must drop it.
check "closing a window drops its subscription" '' <<LUA
local w = { class = "zen-beta", title = "New Tab — Zen Browser",
            initial_title = "Zen Browser", address = "0x1" }
emit("window.open", w)
emit("window.close", w)
local recycled = { class = "zen-beta", title = [[$POPUP]],
                   initial_title = "Zen Browser", address = "0x1" }
emit("window.title", recycled)
LUA

printf '\n%d/%d passed\n' "$((n - fails))" "$n"
[ "$fails" -eq 0 ]
