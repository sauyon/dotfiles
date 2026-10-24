// Emits Orca's platforms.linux keybinding overrides: every action's macOS
// default, with Mod (Cmd there, Ctrl here) spelled Cmd, which Orca binds to
// Super on Linux -- and no Ctrl- or Alt-only chord, so emacs keys reach the
// terminal. See orca.nix.
//
//   node orca-mac-keybindings.js <app>/out/shared <hyprland-chords.json> [pinned.json]
//
// pinned.json maps action ids to bindings that win over all of the above.
//
// Uses Orca's own keybinding module (out/shared/keybindings.js, the stable
// facade its comment promises) for the definitions, normalisation and the
// conflict check, so a version bump regenerates the table instead of
// silently drifting from it.
const fs = require('fs');
const path = require('path');

const [shared, chordsFile, pinnedFile] = process.argv.slice(2);
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
  // Every chord taken leaves the action unbound -- not on its Linux default,
  // which is a Ctrl chord the terminal would lose.
  overrides[def.id] = free;
}

// Ctrl and Meta are the terminal's: emacs, readline. Every action has an
// override by now, so what is left to strip is the Mac table's own Ctrl and
// Alt chords (Ctrl+1..9 tab select, Alt+Z word wrap, Ctrl+PageUp/Down,
// which xterm.js sends as CSI 5;5~ / 6;5~). Only Ctrl+Tab stays: xterm.js
// sends it as a bare Tab, so the terminal has nothing to lose.
const TERMINAL_CANNOT_SEND = new Set(['Ctrl+Tab']);
const takesFromTerminal = (b) => {
  const mods = b.split('+').slice(0, -1);
  return !mods.includes('Cmd') && mods.some((m) => m === 'Ctrl' || m === 'Alt') && !TERMINAL_CANNOT_SEND.has(b);
};
for (const def of kb.KEYBINDING_DEFINITIONS) {
  const effective = overrides[def.id];
  const kept = effective.filter((b) => !takesFromTerminal(b));
  if (kept.length < effective.length) {
    console.error(`${def.id}: ${effective.filter(takesFromTerminal).join(', ')} is the terminal's`);
  }
  overrides[def.id] = kept;
}

// Pinned bindings (orca.nix) win over everything above.
if (pinnedFile) {
  for (const [id, bindings] of Object.entries(JSON.parse(fs.readFileSync(pinnedFile, 'utf8')))) {
    if (!kb.getKeybindingDefinition(id)) throw new Error(`pinned: unknown Orca action "${id}"`);
    if (!Array.isArray(bindings)) throw new Error(`pinned: ${id}: give a list of chords, e.g. [ "Ctrl+S" ]`);
    overrides[id] = bindings.map((b) => {
      const n = kb.normalizeKeybindingWithOptions(b, kb.normalizeOptionsForAction(id));
      if (!n.ok) throw new Error(`pinned: ${id}: ${b}: ${n.error}`);
      if (isTaken(id, n.value)) throw new Error(`pinned: ${id}: ${n.value} is Hyprland's`);
      return n.value;
    });
  }
}

// Nothing to say where the result is Orca's own Linux default.
for (const def of kb.KEYBINDING_DEFINITIONS) {
  if (JSON.stringify(overrides[def.id]) === JSON.stringify(kb.getDefaultBindings(def, 'linux'))) delete overrides[def.id];
}

// Orca drops every action in a conflicting set at load, with only a line in
// its diagnostics; fail the build instead.
const conflicts = kb.findKeybindingConflicts('linux', overrides);
if (conflicts.length > 0) {
  throw new Error(`conflicting shortcuts: ${JSON.stringify(conflicts)}`);
}
process.stdout.write(`${JSON.stringify(overrides, null, 2)}\n`);
