// Repacks the AppImage's app.asar with the app menu's accelerators moved to
// the macOS menu's chords, Cmd spelled Super, so Super is the only modifier
// Orca itself listens on -- the same scheme as the renderer's keybinding table
// (nix/orca-mac-keybindings.js, merged into ~/.orca/keybindings.json).
//
// Upstream's Linux menu is both too greedy and too thin. Its registered
// accelerators fire in the main process before the page sees the key -- off
// macOS, Electron runs them ahead of the page, not after it -- so they took
// Ctrl chords from the terminal: quit on CommandOrControl+Q, paste on
// CmdOrCtrl+V, the cut role (Ctrl+X), devtools (Ctrl+Shift+I) -- and F11 for
// the togglefullscreen role. And Copy and Select All get no accelerator off
// macOS at all, so in a text field outside the terminal (Settings, search
// boxes) nothing handled Super+C/A -- Chromium's own field chords are
// Ctrl-only, and the keybinding table's clipboard actions are terminal.* only.
// tests/orca-menu-accelerators.sh presses each.
//
// Every item's chord is the Mac one, Cmd spelled Super: undo/redo
// Super+Z/Shift+Z, cut/copy/paste/select-all Super+X/C/V/A, quit Super+Q,
// devtools Super+Alt+I, full screen Super+Ctrl+F. Window > Minimize (Mac
// Cmd+M) stays unbound: Super+M is Hyprland's. Window > Zoom has no Mac
// chord. None is registered -- the dispatch appended below unregisters every
// item of the app menu as it is set, and takes the chords itself, page first.
// Click handlers are untouched.
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

// Each entry: [needle, replacement, why]. All must match exactly once.
const PATCHES = [
  // Quit (as of 1.4.273): with an onQuit handler, CmdOrCtrl+Q on every
  // platform but win32, spread from a platform ternary; without one -- the
  // File menu as built here -- the bare quit role, which Electron gives its
  // own CommandOrControl+Q.
  ['...process.platform===`win32`?{}:{accelerator:`CmdOrCtrl+Q`}', '...process.platform===`win32`?{}:{accelerator:`Super+Q`}', 'quit accelerator'],
  ['{role:`quit`,label:n}', '{role:`quit`,label:n,accelerator:`Super+Q`}', 'quit role'],
  // Edit > Undo / Redo roles: inert off macOS upstream (registerAccelerator:!1
  // spread from a minified k as of 1.4.273), so a field had only Chromium's Ctrl+Z.
  ['{role:`undo`,label:p.a(`menu.undo`,`Undo`),...k}', '{role:`undo`,label:p.a(`menu.undo`,`Undo`),accelerator:`Super+Z`}', 'undo role'],
  ['{role:`redo`,label:p.a(`menu.redo`,`Redo`),...k}', '{role:`redo`,label:p.a(`menu.redo`,`Redo`),accelerator:`Super+Shift+Z`}', 'redo role'],
  // Edit > Cut role: Electron registers Ctrl+X for it, which took C-x from
  // the terminal (emacs C-x C-s, etc). The item is built by a minified
  // (role, key, label) helper -- Td({role:e,label:p.a(t,n)}) as of 1.4.273,
  // renamed by the minifier on most bumps -- so the call is replaced with
  // the same object plus the accelerator.
  ['Td(`cut`,`menu.cut`,`Cut`)', '{role:`cut`,label:p.a(`menu.cut`,`Cut`),accelerator:`Super+X`}', 'cut role'],
  // Edit > Copy and Select All (as of 1.4.273): Command+C / Command+A on
  // macOS, no accelerator anywhere else.
  ['...n?{accelerator:e===`copy`?`Command+C`:`Command+A`}:{}', '...{accelerator:e===`copy`?`Super+C`:`Super+A`}', 'copy and select-all accelerators'],
  // Edit > Paste: CmdOrCtrl+V. The click handler stays.
  ['accelerator:`CmdOrCtrl+V`', 'accelerator:`Super+V`', 'paste accelerator'],
  // View > Toggle Full Screen role: F11 off macOS, which a terminal F11
  // (less, etc) should get; the Mac chord is Ctrl+Cmd+F. Same Td shape as
  // cut. (Window > Zoom has no chord on either platform: no patch.)
  ['Td(`togglefullscreen`,`menu.toggleFullScreen`,`Toggle Full Screen`)', '{role:`togglefullscreen`,label:p.a(`menu.toggleFullScreen`,`Toggle Full Screen`),accelerator:`Super+Ctrl+F`}', 'fullscreen role'],
  // View > Toggle Developer Tools: the role MenuItem's accelerator is passed
  // through -- Ctrl+Shift+I here, Alt+Cmd+I on macOS.
  ['accelerator:t.accelerator??void 0', 'accelerator:`Super+Alt+I`', 'devtools accelerator'],
];

for (const [needle, replacement, why] of PATCHES) {
  const n = main.split(needle).length - 1;
  if (n !== 1) {
    console.error(`orca: menu accelerator patch "${why}": ${n} matches for ${JSON.stringify(needle)}, want 1`);
    process.exit(1);
  }
  main = main.replace(needle, replacement);
}
// The dispatch appended below unregisters every app-menu item by wrapping
// Menu.setApplicationMenu, so that must stay the only way a menu is set,
// anywhere in the app's JS: a window menu (setMenu), or another call site,
// would bring back Electron's default role chords (Ctrl+Q, Ctrl+X...) unseen.
const js = [];
const walk = (d) => {
  for (const e of fs.readdirSync(d, { withFileTypes: true })) {
    const p = path.join(d, e.name);
    if (e.isDirectory()) walk(p);
    else if (/\.(c|m)?js$/.test(e.name)) js.push(p);
  }
};
walk(path.join(work, 'out'));
let setters = 0;
const windowMenus = [];
for (const p of js) {
  const t = p === file ? main : fs.readFileSync(p, 'utf8');
  setters += t.split('setApplicationMenu(').length - 1;
  if (t.includes('.setMenu(')) windowMenus.push(path.relative(work, p));
}
if (setters !== 1 || windowMenus.length) {
  console.error(`orca: ${setters} setApplicationMenu( call sites in out/ (want 1), setMenu( in ${JSON.stringify(windowMenus)}; the runtime unregistering would miss a menu`);
  process.exit(1);
}

// Every literal accelerator the main process sets is a Super chord or none:
// a new item upstream spelled CommandOrControl+W or Ctrl+Shift+N would grab
// nothing (unregistered) and match no Super chord -- a dead shortcut, which
// is a bump to look at, not to ship. Literals only: a new role item relying
// on Electron's default chord, or a computed accelerator, still goes dead
// unseen -- safe (it grabs no Ctrl chord), but only the tests would show it.
const foreign = [];
for (const p of js.filter((p) => p.startsWith(path.join(work, 'out/main') + path.sep))) {
  const t = p === file ? main : fs.readFileSync(p, 'utf8');
  for (const m of t.matchAll(/[{,]accelerator:(`[^`]*`|"[^"]*"|'[^']*')/g)) {
    const v = m[1].slice(1, -1);
    if (v !== '' && !v.startsWith('Super+')) foreign.push(`${path.relative(work, p)}: ${v}`);
  }
}
if (foreign.length) {
  console.error(`orca: menu accelerators not on Super: ${JSON.stringify(foreign)}`);
  process.exit(1);
}

// The dispatch appended below is CommonJS; a bundle that went ESM would
// throw on its require at every start, with every needle still matching.
if (!main.includes('require("electron")')) {
  console.error('orca: out/main/index.js no longer require()s electron; the appended dispatch would not load');
  process.exit(1);
}

// Registered, the accelerators would run ahead of the page (above), so a page
// binding of the same chord could never win -- and Chromium would still type
// the letter of a Super chord, Super being no text modifier to it (Super+V
// pasted *and* typed a "v"). So the macOS flow is rebuilt by hand:
//
//   * No app-menu item registers its chord. Menu.setApplicationMenu is
//     wrapped to set registerAccelerator false on every item, which Electron
//     reads at registration time -- so a role Electron gives a default chord
//     (Ctrl+Q quit, Ctrl+W close, Ctrl+R reload...) grabs nothing either,
//     whatever upstream adds or the minifier renames. That is how a Ctrl+Q
//     outlived 5d8a807a: the bare quit role carried no accelerator in the
//     source for a needle to find.
//   * The page gets every key before the menu does (Orca's own main-process
//     shortcut handlers still come first). Each page's top frame carries, in
//     an isolated world the page cannot reach, a capture listener that
//     cancels a Super chord's keypress for a printable key -- the text it
//     would insert, and nothing else (Super+Enter still submits) -- and a
//     bubbling listener that, once the keydown has been dispatched,
//     reports it if nothing in the page took it (preventDefault, or stopped
//     it before it reached window): a page binding of the same chord (the terminal's
//     Super+C/V/A, the file explorer's Super+Z, the shortcut recorder) wins,
//     as on macOS.
//   * The main process clicks the menu item for an unclaimed chord -- the
//     click Orca's renderer already routes to the terminal or the focused
//     field (ui:appMenuPaste, ui:appMenuSelectionAction). Only for a key that
//     webContents received, and only where it was typed: the report must
//     carry the token main gave the document's isolated world at injection
//     -- out of reach of the page's own script and of its frames, whose
//     console messages Electron attributes to the top frame -- and name a
//     Super keydown that before-input-event saw on that webContents within
//     the last second, which no main-process handler took, with the
//     webContents focused both when the key came and when the report does.
//     So a page -- a browser-pane site, a sandboxed plugin frame -- can
//     neither click menu items by forging the report, nor hold a real one
//     back until the focus has moved to a terminal. (The token is in the
//     report's console line, which DevTools at the verbose level and Orca's
//     own console listeners on a browser pane see; the page cannot.)
//     A key event sent into the webContents -- sendInputEvent, as Orca's
//     agent browser automation does -- counts as received: an agent driving
//     a focused browser pane can press Super+V or Super+Q there like a user.
//
// The one place the menu's action is the wrong one is Orca's Monaco editor:
// undo and select-all there are Monaco's own commands, bound to Ctrl+Z,
// Ctrl+Shift+Z and Ctrl+A on Linux (Cmd on macOS), and the menu's native
// undo/select-all reach only its hidden textarea. So inside a .monaco-editor
// the page script turns exactly Super+Z, Super+Shift+Z and Super+A into
// their Ctrl twins for Monaco, ahead of every listener below window. Copy, cut and paste need no
// such help: the menu's native clipboard actions fire the events Monaco
// handles.
//
// So every chord, Quit's included, now needs the page alive: in a hung or
// crashed window, or before its first dom-ready, no Super chord does
// anything, and the compositor is what closes it.
//
// Not in iframes: a field inside one (an Orca plugin frame, a site's
// embedded editor) still types the letter of a Super chord and gets no menu
// action, as before this patch -- its reports could not be told from a
// sandboxed frame's forgeries.
main += `
/* orca: the app menu's Super chords, page first (nix/orca-menu-accelerators.js) */
;(() => {
  const { app, BrowserWindow, Menu, webContents } = require("electron")
  const { randomUUID } = require("crypto")
  const MARK = "orca-unclaimed-super-chord "
  const MODS = ["Meta", "Shift", "Control", "Alt"]
  const WORLD = 1086
  const unregister = (items) => {
    for (const it of items) {
      it.registerAccelerator = false
      if (it.submenu) unregister(it.submenu.items)
    }
  }
  const setApplicationMenu = Menu.setApplicationMenu
  Menu.setApplicationMenu = function (menu) {
    if (menu) unregister(menu.items)
    return setApplicationMenu.call(this, menu)
  }
  const page = (token) => \`(() => {
    addEventListener("keypress", (e) => { if (e.metaKey && e.key.length === 1) e.preventDefault() }, true)
    addEventListener("keydown", (e) => {
      // Exactly Super+Z, Super+Shift+Z, Super+A, by the layout's letter --
      // not Super+Shift+A, which is Orca's own editor.addReviewNote.
      const k = e.key.toLowerCase()
      if (!e.isTrusted || e.defaultPrevented || e.isComposing || !e.metaKey || e.ctrlKey || e.altKey
        || !(k === "z" || (k === "a" && !e.shiftKey))) return
      const t = e.target
      if (!(t instanceof Element) || !t.closest(".monaco-editor")) return
      e.preventDefault()
      e.stopImmediatePropagation()
      t.dispatchEvent(new KeyboardEvent("keydown", { key: e.key, code: e.code, keyCode: e.keyCode, which: e.which,
        ctrlKey: true, shiftKey: e.shiftKey, bubbles: true, cancelable: true, composed: true }))
    }, true)
    addEventListener("keydown", (e) => {
      if (e.isTrusted && e.metaKey && !\${JSON.stringify(MODS)}.includes(e.key))
        setTimeout(() => e.defaultPrevented || console.debug(
          \${JSON.stringify(MARK + token + " ")} + [e.code, e.key, +e.ctrlKey, +e.altKey, +e.shiftKey].join(" ")))
    })
  })()\`
  const norm = (a) => {
    const p = a.split("+"), k = p.pop()
    return [...p.map((m) => m.toLowerCase()).sort(), k.toUpperCase()].join("+")
  }
  const find = (items, want) => {
    for (const it of items) {
      if (!it.visible || !it.enabled) continue
      const hit = it.submenu ? find(it.submenu.items, want)
        : it.accelerator && norm(it.accelerator) === want && it
      if (hit) return hit
    }
    return null
  }
  app.on("web-contents-created", (_, wc) => {
    // Chords typed and not yet reported, oldest first: a busy renderer can
    // report one after the next has come in.
    let pending = []
    // The current document's: a report without it is a forgery.
    let token = null
    const focused = () => webContents.getFocusedWebContents() === wc
    wc.on("dom-ready", () => {
      token = randomUUID()
      wc.executeJavaScriptInIsolatedWorld(WORLD, [{ code: page(token) }])
        .catch((err) => console.error("orca: menu chord script not injected:", err))
    })
    // Nothing typed carries over to another page, nor to another focus.
    wc.on("blur", () => { pending = [] })
    wc.on("did-start-navigation", (e, _url, isInPlace, isMainFrame) => {
      if ((e.isMainFrame ?? isMainFrame) && !(e.isSameDocument ?? isInPlace)) pending = []
    })
    wc.on("before-input-event", (event, input) => { try {
      if (input.type !== "keyDown" || !input.meta || MODS.includes(input.key)) return
      // The layout's letter where the key yields one -- so on a non-Latin
      // layout (Super+C typing "с") no Latin chord matches.
      const key = input.key.length === 1 ? input.key : /^Key[A-Z]$/.test(input.code) ? input.code.slice(3) : input.key
      const want = norm(["Super", input.control && "Ctrl", input.alt && "Alt", input.shift && "Shift", key].filter(Boolean).join("+"))
      const menu = Menu.getApplicationMenu()
      const item = menu && find(menu.items, want)
      if (!item || !focused()) return
      const report = [input.code, input.key, +input.control, +input.alt, +input.shift].join(" ")
      // After every listener has run: a key one of Orca's own main-process
      // handlers took is theirs.
      queueMicrotask(() => {
        if (event.defaultPrevented) return
        const now = Date.now()
        pending = pending.filter((p) => now - p.at <= 1000)
        pending.push({ item, report, at: now })
      })
    } catch (err) {
      console.error("orca: menu chord:", err)
    } })
    wc.on("console-message", (e) => {
      try {
        const m = e.message
        if (!pending.length || !token || typeof m !== "string" || !m.startsWith(MARK + token + " ")) return
        const now = Date.now()
        pending = pending.filter((p) => now - p.at <= 1000)
        const i = pending.findIndex((p) => MARK + token + " " + p.report === m)
        if (i < 0) return
        const { item } = pending[i]
        pending = pending.slice(i + 1)
        if (!focused()) return
        item.click({ triggeredByAccelerator: true }, BrowserWindow.getFocusedWindow() ?? undefined, wc)
      } catch (err) {
        console.error("orca: menu chord:", err)
      }
    })
  })
})();
`;
fs.writeFileSync(file, main);
execFileSync(process.execPath, ['--check', file], { stdio: 'inherit' });

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
