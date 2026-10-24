// Emits Orca's platforms.linux keybinding overrides: every action's macOS
// default, with Mod (Cmd there, Ctrl here) spelled Cmd, which Orca binds to
// Super on Linux. See orca.nix.
//
//   node orca-mac-keybindings.js <app>/out/shared <hyprland-chords.json>
//
// Uses Orca's own keybinding module (out/shared/keybindings.js, the stable
// facade its comment promises) for the definitions, normalisation and the
// conflict check, so a version bump regenerates the table instead of
// silently drifting from it.
const fs = require('fs');
const path = require('path');

const [shared, chordsFile] = process.argv.slice(2);
const kb = require(path.resolve(shared, 'keybindings.js'));

// Chords Hyprland binds never reach the window. "SUPER + SHIFT + W" ->
// "Cmd+Shift+W". Mouse, switch and media-key binds cannot be an Orca chord;
// anything else that does not translate is an error, not a free chord.
const MODS = { SUPER: 'Cmd', CTRL: 'Ctrl', CONTROL: 'Ctrl', ALT: 'Alt', SHIFT: 'Shift' };
const KEYS = { return: 'Enter', left: 'ArrowLeft', right: 'ArrowRight', up: 'ArrowUp', down: 'ArrowDown' };
const taken = new Set();
for (const chord of JSON.parse(fs.readFileSync(chordsFile, 'utf8'))) {
  const parts = chord.split('+').map((s) => s.trim());
  const key = parts.pop();
  if (/^(mouse[:_]|XF86)/.test(key) || chord.startsWith('switch:')) continue;
  const mods = parts.map((p) => MODS[p.toUpperCase()]);
  const n = mods.every(Boolean) && kb.normalizeKeybinding([...mods, KEYS[key] ?? key].join('+'));
  if (!n || !n.ok) throw new Error(`cannot translate Hyprland bind "${chord}" to an Orca chord`);
  taken.add(n.value);
}
// A digit-index action's one binding stands for its whole 1-9 family.
const isTaken = (id, binding) =>
  kb.isDigitIndexActionId(id)
    ? [1, 2, 3, 4, 5, 6, 7, 8, 9].some((d) => taken.has(binding.replace(/1$/, String(d))))
    : taken.has(binding);

const overrides = {};
for (const def of kb.KEYBINDING_DEFINITIONS) {
  const mac = kb.getDefaultBindings(def, 'darwin').map((binding) => {
    const cmd = binding.split('+').map((t) => (t === 'Mod' ? 'Cmd' : t)).join('+');
    const n = kb.normalizeKeybindingWithOptions(cmd, kb.normalizeOptionsForAction(def.id));
    if (!n.ok) throw new Error(`${def.id}: ${cmd}: ${n.error}`);
    return n.value;
  });
  const free = mac.filter((b) => !isTaken(def.id, b));
  if (free.length < mac.length) {
    console.error(`${def.id}: ${mac.filter((b) => isTaken(def.id, b)).join(', ')} is Hyprland's`);
  }
  // Every chord taken: leave the action on its Linux default rather than
  // unbinding it. No macOS binding at all is different -- unbound there, so
  // unbound here ([]). Same as the Linux default: nothing to say.
  if (mac.length > 0 && free.length === 0) continue;
  if (JSON.stringify(free) === JSON.stringify(kb.getDefaultBindings(def, 'linux'))) continue;
  overrides[def.id] = free;
}

// Orca drops every action in a conflicting set at load, with only a line in
// its diagnostics; fail the build instead.
const conflicts = kb.findKeybindingConflicts('linux', overrides);
if (conflicts.length > 0) {
  throw new Error(`conflicting shortcuts: ${JSON.stringify(conflicts)}`);
}
process.stdout.write(`${JSON.stringify(overrides, null, 2)}\n`);
