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

// Activation runs under set -e, so a throw here would abandon the rest of
// the switch over a keybinding file. Any failure is one line and exit 0;
// the file is then exactly as it was, since writes only ever rename over it.
process.on('uncaughtException', (e) => {
  console.error(`orca: keybinding merge skipped: ${e.message}`);
  process.exit(0);
});

const [shared, ownedFile] = process.argv.slice(2);
const kb = require(path.resolve(shared, 'keybindings.js'));
const owned = JSON.parse(fs.readFileSync(ownedFile, 'utf8'));

const dir = path.join(os.homedir(), '.orca');
const dest = path.join(dir, 'keybindings.json');
const sidecar = path.join(dir, 'keybindings.nix-owned.json');
const isObject = (v) => v !== null && typeof v === 'object' && !Array.isArray(v);
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);

// Orca's own writer (G$): whole file to a temp name, rename over, and the
// temp removed if either step fails.
function writeJson(file, value) {
  const tmp = `${file}.nix-${process.pid}-${Date.now()}`;
  try {
    fs.writeFileSync(tmp, `${JSON.stringify(value, null, 2)}\n`, { flag: 'wx' });
    fs.renameSync(tmp, file);
  } catch (e) {
    fs.rmSync(tmp, { force: true });
    throw e;
  }
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
  if (!isObject(s?.linux)) throw new Error('no .linux object');
  prev = s.linux;
} catch (e) {
  // No sidecar is a first run. A bad one stops the merge: it decides what
  // may be overwritten, and reading it as empty overwrites every key the UI
  // has changed.
  if (e.code !== 'ENOENT') {
    console.error(`orca: ${sidecar}: ${e.message}; not merging -- delete it to start over`);
    process.exit(0);
  }
}

const platforms = isObject(doc.platforms) ? doc.platforms : {};
const linux = isObject(platforms.linux) ? { ...platforms.linux } : {};
// Changed since the last write: the UI's now. Absent (reset in the UI)
// is not a change -- the reset target is this table, so it comes back.
const theirs = new Set(Object.keys(prev).filter((id) => id in linux && !same(linux[id], prev[id])));
for (const id of Object.keys(prev)) {
  if (!theirs.has(id)) delete linux[id];
}
// What the UI had on each key about to be overwritten, for a withdrawal
// below to put back. Stale keys are gone by now, so anything left is the UI's.
const before = {};
const written = {};
for (const [id, bindings] of Object.entries(owned)) {
  if (theirs.has(id)) continue;
  if (id in linux) before[id] = linux[id];
  linux[id] = bindings;
  written[id] = bindings;
}

// What Orca's loader feeds its conflict pass: common, then the platform
// section over it, with its string shorthand and null/false expanded.
// Common is the root's action keys in the legacy shape with no .keybindings.
const common = doc.keybindings === undefined ? doc : isObject(doc.keybindings) ? doc.keybindings : {};
// Orca's loader (jHn): null/false unbind; a string is a comma list; an array
// of strings may hold comma lists too; one bad chord rejects the whole value,
// leaving the action on its default (undefined here).
const expand = (id, v) => {
  if (v === null || v === false) return [];
  const n =
    typeof v === 'string' ? kb.normalizeKeybindingListForAction(id, v)
      : Array.isArray(v) && v.every((b) => typeof b === 'string') ? kb.normalizeKeybindingArrayForAction(id, v)
        : { ok: false };
  return Array.isArray(n) ? n : n.ok ? [n.value] : undefined;
};
// Each section parsed on its own, then layered (q$): a bad platform value is
// dropped, not allowed to hide a good common one.
const parse = (section) =>
  Object.fromEntries(
    Object.entries(section)
      .filter(([id, v]) => kb.isKeybindingActionId(id) && expand(id, v) !== undefined)
      .map(([id, v]) => [id, expand(id, v)]),
  );
const commonParsed = parse(common);
const effective = { ...commonParsed, ...parse(linux) };
for (;;) {
  const withdrawn = kb
    .findKeybindingConflicts('linux', effective)
    .flatMap((c) => c.actionIds)
    .filter((id) => id in written);
  // Only generated keys are ever withdrawn. A conflict with none left in it
  // -- between UI shortcuts, or a UI shortcut and the Linux default a
  // withdrawal brought back -- is Orca's to report.
  if (withdrawn.length === 0) break;
  for (const id of new Set(withdrawn)) {
    console.error(`orca: ${id} ${JSON.stringify(written[id])} clashes with a shortcut set in Orca; keeping that one`);
    delete written[id];
    if (id in before) {
      linux[id] = before[id];
    } else {
      delete linux[id];
    }
    const restored = id in linux ? expand(id, linux[id]) : undefined;
    if (restored !== undefined) effective[id] = restored;
    else if (id in commonParsed) effective[id] = commonParsed[id];
    else delete effective[id];
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
// Every key this table claims, written or withdrawn, at its generated value;
// the UI's keep the value last written. Either way a later UI value reads as
// changed, the UI's, rather than as never owned and free to overwrite.
writeJson(sidecar, {
  linux: { ...owned, ...Object.fromEntries([...theirs].map((id) => [id, prev[id]])), ...written },
});
