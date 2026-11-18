// Recreates the `out/shared/keybindings.js` facade Orca shipped through
// 1.4.251 ("Keep the public keybinding import path stable while
// implementations live beside this facade") and dropped in 1.4.262, when the
// keybindings/ directory became the only import path. orca.nix writes this
// beside the extracted keybindings/ tree so nix/orca-mac-keybindings{,-merge}.js
// keep requiring one module and picking up every export the old facade did.
//
// The export list is the old facade's verbatim: types, definitions, parser,
// normalization, input, effective, matching-key, matching, formatting,
// keybinding-diagnostic-copy. A new Orca module the merge needs would still
// fail the build here, the same way a moved import did before.
const fs = require('fs');
const path = require('path');

const dir = path.resolve(__dirname, 'keybindings');
for (const m of fs.readdirSync(dir).filter((f) => f.endsWith('.js'))) {
  Object.assign(module.exports, require(path.join(dir, m)));
}
