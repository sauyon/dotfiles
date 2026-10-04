#!/usr/bin/env bash
# Cases for the Vesktop-to-workspace-8 hook (defined in hyprland.nix).
#
# Vesktop is the user's Discord client and lives on workspace 8 regardless of
# where it was launched from. WM_CLASS is "Vesktop" (capital V) -- StartupWMClass
# in the desktop entry. Class, not title, is the match: Vesktop windows change
# title constantly per-server / per-channel, and any other app is free to put
# "Vesktop" in a title.
#
# We pin via a window.open handler rather than a static windowrulev2 to keep
# the per-window behaviour colocated with the rest of this file's hooks (the
# Zen popup handler is the existing pattern). That colocating has a cost: this
# test is what keeps the class string and workspace number honest.
#
# They load the *generated* hyprland.lua under a stub `hl`, so what is under test
# is the Lua that actually ships, not a copy of it.
#
#   ./tests/hyprland-vesktop-workspace.sh                   # evaluates this host's config
#   ./tests/hyprland-vesktop-workspace.sh /path/hyprland.lua # tests one you already have
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

# The stub. Same shape as tests/hyprland-zen-popup.sh: every hl.* call is a
# no-op recorder except hl.on (which registers a handler and hands back a
# :remove()-able subscription) and hl.dispatch (which appends to the trace).
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
-- handler, and printing the table is not stable.
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

# The case the handler exists for: a Vesktop window maps and gets pinned to 8.
check "vesktop window.open moves to workspace 8" \
  'dsp.window.move window=<window> workspace=8' <<'LUA'
local w = { class = "Vesktop", title = "Vesktop",
            initial_title = "Vesktop", address = "0x1" }
emit("window.open", w)
LUA

# The class guard: another app opening a window must not be moved.
check "non-vesktop window is left alone" '' <<'LUA'
local w = { class = "com.mitchellh.ghostty", title = "shiori: dotfiles",
            initial_title = "ghostty", address = "0x2" }
emit("window.open", w)
LUA

# Class nothing alike: the title is not the match.
check "non-vesktop class with vesktop-like title is left alone" '' <<'LUA'
local w = { class = "qutebrowser", title = "Vesktop",
            initial_title = "Vesktop", address = "0x3" }
emit("window.open", w)
LUA

printf '\n%d/%d passed\n' "$((n - fails))" "$n"
[ "$fails" -eq 0 ]
