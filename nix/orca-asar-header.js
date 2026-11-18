// Prints one line per FILE node of an asar archive:
//   <unpacked|pack> <size> <path>
// Directory nodes are skipped: Electron reads only file nodes, and the
// stock archive marks unpacked files without their parent directories while a
// repack marks both -- the file set is the invariant, not the dir flags.
//
//   node orca-asar-header.js <archive.asar>
const fs = require('fs');

const [archive] = process.argv.slice(2);
if (!archive) {
  console.error('usage: orca-asar-header.js <archive.asar>');
  process.exit(2);
}
const s = fs.readFileSync(archive);
// asar layout: uint32 pickle size, uint32 header size, header JSON, files.
const headerLen = s.readUInt32LE(4);
const header = s.slice(8, 8 + headerLen).toString('utf8');
const h = JSON.parse(header.slice(header.indexOf('{'), header.lastIndexOf('}') + 1));

const walk = (dir, pfx) => {
  for (const [name, entry] of Object.entries(dir.files || {})) {
    if (entry.files) walk(entry, `${pfx}/${name}`);
    else console.log(`${entry.unpacked ? 'unpack' : 'pack'} ${entry.size} ${pfx}/${name}`);
  }
};
walk(h, '');
