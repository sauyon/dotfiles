// Merges the generated overrides (orca-mac-keybindings.js) into
// ~/.orca/keybindings.json's platforms.linux. See orca.nix.
//
//   node orca-mac-keybindings-merge.js <app>/out/shared <owned.json>
//
// The file is Orca's read-write state, so this owns only what it wrote: a
// sidecar, keybindings.nix-owned.json, records the values of the last write.
//   - a key written last time and no longer generated is removed;
//   - a key whose value the UI has changed since is left as the UI's;
//   - everything else in the file is untouched.
// Then the merged file gets Orca's own conflict check. Orca drops every
// action in a conflicting set at load, so a generated key that clashes with
// a UI shortcut is withdrawn instead, and named, leaving the UI's choice and
// the generated action's Linux default.
const fs = require('fs');
const os = require('os');
const path = require('path');

const [shared, ownedFile] = process.argv.slice(2);
const kb = require(path.resolve(shared, 'keybindings.js'));
const owned = JSON.parse(fs.readFileSync(ownedFile, 'utf8'));

const dir = path.join(os.homedir(), '.orca');
const dest = path.join(dir, 'keybindings.json');
const sidecar = path.join(dir, 'keybindings.nix-owned.json');
const isObject = (v) => v !== null && typeof v === 'object' && !Array.isArray(v);
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);

// Orca's own writer: whole file to a temp name, then rename over.
function writeJson(file, value) {
  const tmp = `${file}.nix-${process.pid}`;
  fs.writeFileSync(tmp, `${JSON.stringify(value, null, 2)}\n`, { flag: 'wx' });
  fs.renameSync(tmp, file);
}

fs.mkdirSync(dir, { recursive: true });
let doc;
if (!fs.existsSync(dest)) {
  doc = { version: 1, keybindings: {}, platforms: { darwin: {}, linux: {}, win32: {} } };
} else {
  let why = null;
  try {
    doc = JSON.parse(fs.readFileSync(dest, 'utf8'));
    if (!isObject(doc)) why = 'not a JSON object';
  } catch (e) {
    why = e.message;
  }
  if (why !== null) {
    // Orca's loader rejects it too and says so; overwriting would lose
    // whatever the user was halfway through editing.
    console.error(`orca: ${dest}: ${why}; leaving it for Orca to report`);
    process.exit(0);
  }
}

let prev = {};
try {
  const s = JSON.parse(fs.readFileSync(sidecar, 'utf8'));
  if (isObject(s.linux)) prev = s.linux;
} catch {
  // No sidecar: first run, nothing previously owned.
}

const platforms = isObject(doc.platforms) ? doc.platforms : {};
const linux = isObject(platforms.linux) ? { ...platforms.linux } : {};
// Changed since the last write: the UI's now. Absent (reset in the UI)
// is not a change -- the reset target is this table, so it comes back.
const theirs = new Set(Object.keys(prev).filter((id) => id in linux && !same(linux[id], prev[id])));
for (const id of Object.keys(prev)) {
  if (!theirs.has(id)) delete linux[id];
}
const written = {};
for (const [id, bindings] of Object.entries(owned)) {
  if (theirs.has(id)) continue;
  linux[id] = bindings;
  written[id] = bindings;
}

// What Orca's loader feeds its conflict pass: common, then the platform
// section over it, with its string shorthand and null/false expanded.
// Common is the root's action keys in the legacy shape with no .keybindings.
const common = doc.keybindings === undefined ? doc : isObject(doc.keybindings) ? doc.keybindings : {};
const effective = {};
for (const [id, v] of Object.entries({ ...common, ...linux })) {
  if (!kb.isKeybindingActionId(id)) continue;
  if (v === null || v === false) effective[id] = [];
  else if (typeof v === 'string') effective[id] = [v];
  else if (Array.isArray(v) && v.every((b) => typeof b === 'string')) effective[id] = v;
}
for (;;) {
  const withdrawn = kb
    .findKeybindingConflicts('linux', effective)
    .flatMap((c) => c.actionIds)
    .filter((id) => id in written);
  // Conflicts among the UI's own shortcuts are Orca's to report, not ours.
  if (withdrawn.length === 0) break;
  for (const id of new Set(withdrawn)) {
    console.error(`orca: ${id} ${JSON.stringify(written[id])} clashes with a shortcut set in Orca; keeping that one`);
    delete linux[id];
    delete written[id];
    delete effective[id];
  }
}

doc.version = 1;
doc.platforms = {
  ...platforms,
  darwin: isObject(platforms.darwin) ? platforms.darwin : {},
  linux,
  win32: isObject(platforms.win32) ? platforms.win32 : {},
};
writeJson(dest, doc);
// The UI's keys keep the value last written, so the next run still sees
// them as changed rather than as never owned.
writeJson(sidecar, {
  linux: { ...Object.fromEntries([...theirs].map((id) => [id, prev[id]])), ...written },
});
