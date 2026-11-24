// Repacks the AppImage's app.asar with the app menu's Ctrl accelerators
// neutralised, so Super is the only modifier Orca itself listens on. The
// renderer's keybinding table is already all-Cmd (nix/orca-mac-keybindings.js,
// merged into ~/.orca/keybindings.json); what still steals Ctrl chords is the
// Electron application menu, whose registered accelerators fire in the main
// process before any terminal sees the key: quit on CmdOrCtrl+Q, paste on
// CmdOrCtrl+V, the cut role (Ctrl+X), devtools (Ctrl+Shift+I), and the zoom
// and togglefullscreen roles. Every one of those has a Super chord in the
// keybinding table already (terminal.paste = Cmd+V, zoom.in = Cmd+Equal, ...)
// except quit and devtools, which the menu keeps reachable by click -- the
// titlebar hamburger pops the same menu.
//
// The menu items stay; only their key grabs go. Click handlers are untouched,
// and undo/redo were already inert (registerAccelerator:!1 upstream).
//
//   node orca-menu-accelerators.js <app.asar> <out.asar>
//
// Fails the build on any miss: a bundle that drifted needs the patterns
// re-checked, not a half-patched menu.
const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFileSync } = require('child_process');

const [src, dest] = process.argv.slice(2);
if (!src || !dest) {
  console.error('usage: orca-menu-accelerators.js <app.asar> <out.asar>');
  process.exit(2);
}

const work = fs.mkdtempSync(path.join(os.tmpdir(), 'orca-accel-'));
const asar = (args) => execFileSync('asar', args, { stdio: ['ignore', 'pipe', 'inherit'] });
asar(['extract', src, work]);

const file = path.join(work, 'out/main/index.js');
let main = fs.readFileSync(file, 'utf8');
const before = main;

// Each entry: [needle, replacement, why]. All must match exactly once.
const PATCHES = [
  // Quit (ise): CmdOrCtrl+Q on every platform but win32, since 1.4.262
  // spread from a platform ternary. Empty accelerator: the label keeps no
  // chord, nothing registers.
  ['...process.platform===`win32`?{}:{accelerator:`CmdOrCtrl+Q`}', '...process.platform===`win32`?{}:{accelerator:``}', 'quit accelerator'],
  // Edit > Paste: CmdOrCtrl+V. The click handler stays; terminal.paste's
  // Cmd+V (Super+V) is the keybinding table's.
  ['accelerator:`CmdOrCtrl+V`', 'accelerator:``', 'paste accelerator'],
  // Edit > Cut role: Electron registers Ctrl+X for it. The terminal keeps
  // C-x (emacs C-x C-s, etc). The item is built by a minified
  // (role, key, label) helper -- Td({role:e,label:p.a(t,n)}) as of 1.4.273,
  // renamed by the minifier on most bumps -- so the call is
  // replaced with the same object plus registerAccelerator:!1.
  ['Td(`cut`,`menu.cut`,`Cut`)', '{role:`cut`,label:p.a(`menu.cut`,`Cut`),registerAccelerator:!1}', 'cut role'],
  // Window > Zoom role: Ctrl+= / Ctrl+- / Ctrl+0. zoom.in/out/reset are
  // Cmd+Equal / Cmd+Minus in the table (zoom.reset unbound: Super+0 is
  // Hyprland's workspace 10). Same Td shape as cut.
  ['Td(`zoom`,`menu.zoom`,`Zoom`)', '{role:`zoom`,label:p.a(`menu.zoom`,`Zoom`),registerAccelerator:!1}', 'zoom role'],
  // View > Toggle Full Screen role: F11. Bare key, not a Ctrl chord, but a
  // terminal F11 (less, etc) should reach the shell too. Same Td shape.
  ['Td(`togglefullscreen`,`menu.toggleFullScreen`,`Toggle Full Screen`)', '{role:`togglefullscreen`,label:p.a(`menu.toggleFullScreen`,`Toggle Full Screen`),registerAccelerator:!1}', 'fullscreen role'],
  // View > Toggle Developer Tools: the role MenuItem's accelerator
  // (Ctrl+Shift+I on Linux) was passed through. Drop it; the click still
  // toggles.
  ['accelerator:t.accelerator??void 0', 'accelerator:void 0', 'devtools accelerator'],
];

for (const [needle, replacement, why] of PATCHES) {
  const n = main.split(needle).length - 1;
  if (n !== 1) {
    console.error(`orca: menu accelerator patch "${why}": ${n} matches for ${JSON.stringify(needle)}, want 1`);
    process.exit(1);
  }
  main = main.replace(needle, replacement);
}
if (main === before) {
  console.error('orca: menu accelerator patches applied nothing');
  process.exit(1);
}
fs.writeFileSync(file, main);

// Repack with the unpack layout the AppImage ships: the same files outside
// the archive (the spawned entries -- daemon-entry.js, computer-sidecar.js,
// ... -- are exec'd from app.asar.unpacked by path) and the same ones inside.
// The glob was derived from the stock header: every unpacked file of 1.4.251
// matches it, and no packed one does. A bump that changes the layout shows up
// as a header diff in the installCheck below, not as a broken spawn.
asar([
  'pack', work, dest,
  '--unpack-dir', '{out/cli,out/main/agent-hooks,out/main/backend,out/main/chunks,out/main/daemon,out/main/environment-doctor,out/main/filesystem,out/main/gitlab,out/main/orca-profiles,out/main/orcad,out/main/persistence,out/main/plugins,out/main/skills,out/main/startup,out/main/worker-threads,out/mobile-web,out/renderer,out/shared,resources}',
  '--unpack', '{**/{agent-identity-worker-entry,bots-db-worker-entry,browser-extension-native-host-supervisor-entry,computer-sidecar,daemon-entry,foreign-sqlite-reader-entry,journal-sql-reader-entry,journal-sql-worker-entry,orchestration-reader-entry,parcel-watcher-process-entry,plugin-host-entry,session-scanner-service-entry,session-search-worker-entry,wsl-transcript-fs-process-entry}.js,**/out/package.json,*.{png,woff2,html,css,woff,ttf,webp,svg,wasm,mjs,bcmap,pfb,sh,TXT,md,txt,py,mp3}}',
]);
fs.rmSync(work, { recursive: true, force: true });
