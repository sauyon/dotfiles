// Regression guard for mcode's SIGABRT at TUI startup. Run from `mcode.nix`'s
// installCheckPhase with the built better-sqlite3 package directory as argv[2];
// prints a line and exits 0 on a good build, dies on SIGABRT on a bad one.
//
// The abort it guards against:
//
//   node::RemoveEnvironmentCleanupHook(...) at ../../src/api/hooks.cc:142
//   Assertion failed: (env) != nullptr
//     Statement::~Statement()
//     v8::internal::GlobalHandles::InvokeFirstPassWeakCallbacks()
//     v8::internal::Heap::PerformGarbageCollection(...)
//
// Why the loop has this shape: a collection asked for from JS (`global.gc()`)
// runs with a context entered, so `Environment::GetCurrent` finds one and
// nothing aborts. Only a collection that lands on a V8 platform task has no
// entered context. So rather than ask for one, churn garbage `Statement`
// wrappers and allocate hard enough that a collection arrives on its own, and
// let it reap one.
//
// Costs about a second either way: a bad build aborts almost immediately, a good
// one finishes the loop in under two seconds.

const Database = require(process.argv[2]);

const db = new Database(':memory:');
db.exec('CREATE TABLE t (a INTEGER, b TEXT)');

const insert = db.prepare('INSERT INTO t VALUES (?, ?)');
for (let i = 0; i < 500; i++) insert.run(i, 'x'.repeat(64));

// A fresh prepared statement per iteration, each dropped immediately, is what
// leaves dead Statement wrappers behind for the collector to find.
let ballast = [];
for (let i = 0; i < 200000; i++) {
  db.prepare('SELECT a, b FROM t WHERE a > ' + (i % 500)).get();
  ballast.push(Buffer.allocUnsafe(1024));
  if (ballast.length > 2000) ballast = [];
}

console.log('gc-stress: survived 200000 statement reaps');
