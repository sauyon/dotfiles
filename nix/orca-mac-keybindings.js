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
// "Cmd+Shift+W", Hyprland's modifier and keysym aliases included (keysyms
// case-insensitively, as Hyprland reads them). Mouse, switch and media binds,
// and the few keys Orca has no chord for (NO_CHORD), are skipped. Anything
// else that does not translate fails the build: an unknown name may be an
// Orca chord spelled another way, and guessing "harmless" ships a dead key.
const MODS = {
  SUPER: 'Cmd', WIN: 'Cmd', LOGO: 'Cmd', MOD4: 'Cmd', META: 'Cmd',
  CTRL: 'Ctrl', CONTROL: 'Ctrl',
  ALT: 'Alt', MOD1: 'Alt',
  SHIFT: 'Shift',
};
const KEYS = {
  return: 'Enter', left: 'ArrowLeft', right: 'ArrowRight', up: 'ArrowUp', down: 'ArrowDown',
  grave: 'Backquote', apostrophe: 'Quote',
  prior: 'PageUp', next: 'PageDown', page_up: 'PageUp', page_down: 'PageDown',
  kp_add: 'NumpadAdd', kp_subtract: 'NumpadSubtract',
};
const NO_CHORD = new Set(['print', 'scroll_lock', 'pause', 'sys_req', 'menu']);
// Hyprland matches a bind on the unshifted keysym, but Orca matches the key
// the press produced: Shift+= arrives as "+", which Orca names Plus, and
// Shift+- as Underscore. Every other punctuation key Orca matches by its
// unshifted name under Shift (probed against keybindingMatchesInput).
const SHIFTED = { Equal: 'Plus', Minus: 'Underscore' };
const taken = new Set();
for (const chord of JSON.parse(fs.readFileSync(chordsFile, 'utf8'))) {
  if (typeof chord !== 'string') throw new Error(`Hyprland bind with a non-string key: ${JSON.stringify(chord)}`);
  const parts = chord.split('+').map((s) => s.trim());
  const key = parts.pop();
  if (/^(mouse[:_]|xf86)/i.test(key) || chord.startsWith('switch:')) continue;
  if (/^code:/.test(key)) {
    // Which key a keycode is depends on the layout; there is no Orca name to check.
    throw new Error(`Hyprland bind "${chord}" is a keycode bind; bind it by keysym so Orca's chords can be checked`);
  }
  const mods = parts.map((p) => MODS[p.toUpperCase()]);
  if (!mods.every(Boolean)) throw new Error(`unknown modifier in Hyprland bind "${chord}"`);
  // Bare keys too: Orca's few bare chords (F7, Delete) can be Hyprland's.
  if (NO_CHORD.has(key.toLowerCase())) continue;
  const n = kb.normalizeKeybindingWithOptions([...mods, KEYS[key.toLowerCase()] ?? key].join('+'), {
    allowBareKeybindings: true,
    allowShiftOnlyKeybindings: true,
  });
  if (!n.ok) {
    throw new Error(`cannot translate Hyprland bind "${chord}" (${n.error}); map its key in KEYS, or NO_CHORD if Orca has none`);
  }
  taken.add(n.value);
  const k = n.value.split('+').pop();
  if (n.value.includes('Shift+') && SHIFTED[k]) taken.add(n.value.replace(/[^+]+$/, SHIFTED[k]));
}
// A digit-index action's one binding stands for its whole 1-9 family.
const isTaken = (id, binding) =>
  kb.isDigitIndexActionId(id)
    ? [1, 2, 3, 4, 5, 6, 7, 8, 9].some((d) => taken.has(binding.replace(/1$/, String(d))))
    : taken.has(binding);

const overrides = {};
// Actions every one of whose Mac chords Hyprland took: left on Linux defaults.
const fellBack = new Set();
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
  if (mac.length > 0 && free.length === 0) {
    fellBack.add(def.id);
    continue;
  }
  if (JSON.stringify(free) === JSON.stringify(kb.getDefaultBindings(def, 'linux'))) continue;
  overrides[def.id] = free;
}

// A fallback can land on a generated chord the Mac table never had to keep
// apart from it. Orca dispatches some actions across scopes, ahead of the
// conflict check's scope groups -- the workspace digits before the tab
// digits -- so with Super+1..9 Hyprland's, workspace.selectByIndex stays on
// Ctrl+1..9 and the Mac tab.selectByIndex (Ctrl+1..9) would never fire.
// Such an action goes back to its own Linux default too, until none is left.
const identities = (id, bindings) =>
  bindings.flatMap((b) => kb.keybindingConflictIdentities(id, b, 'linux'));
for (;;) {
  const held = new Set(
    [...fellBack].flatMap((id) => identities(id, kb.getDefaultBindings(kb.getKeybindingDefinition(id), 'linux'))),
  );
  const shadowed = Object.keys(overrides).filter((id) => identities(id, overrides[id]).some((i) => held.has(i)));
  if (shadowed.length === 0) break;
  for (const id of shadowed) {
    console.error(`${id}: ${overrides[id].join(', ')} is a fallen-back Linux default's`);
    delete overrides[id];
    fellBack.add(id);
  }
}

// Orca drops every action in a conflicting set at load, with only a line in
// its diagnostics; fail the build instead.
const conflicts = kb.findKeybindingConflicts('linux', overrides);
if (conflicts.length > 0) {
  throw new Error(`conflicting shortcuts: ${JSON.stringify(conflicts)}`);
}
process.stdout.write(`${JSON.stringify(overrides, null, 2)}\n`);
