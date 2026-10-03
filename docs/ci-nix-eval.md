# Ad-hoc evals and test runs: `hmeval` + `.forgejo/workflows/nix-eval.yml`

`hms` answers "build and switch this host". `hmeval` answers the cheaper
question that comes up far more often — **does this still evaluate?** — and the
one `hms` cannot answer at all, because it refuses a dirty tree: *does the thing
I have not committed yet evaluate?*

```bash
hmeval                      # every host, locally, bounded
hmeval utsuho mari          # just these
hmeval --attr 'homeConfigurations.utsuho.config.programs.zen-browser.package.drvPath'
hmeval --ci                 # same question, on the runner
hmeval --tests              # tests/*.sh on the runner (implies --ci)
hmeval --tests tests/wif-jwks.sh tests/hmeval.sh
```

Progress goes to stderr and results to stdout, so `hmeval | grep` sees the
answer and nothing else.

## Why local is the default

Nix's evaluator is single-threaded. Evaluation on its own cannot saturate a
24-core laptop — what does is a **build that evaluation starts**: an
import-from-derivation, or an input that has to be realised before the
expression referring to it can be read. `nix eval` will happily do that, with
`max-jobs` defaulting to the core count, and the first sign is the fans.

`hmeval` passes `--max-jobs 0 --cores 1`. Nix then substitutes from attic or
stops with `cannot build ... max-jobs = 0`, which is the *useful* failure: it
means this question genuinely needs a builder, and `--ci` is one flag away. The
`systemd-run --user --scope -p CPUQuota=…` wrapper bounds the part `--max-jobs`
does not — parallel substituter downloads — and is best-effort: a missing or
broken user manager falls back to plain `nice`, because an evaluation that
`--max-jobs 0` already bounds must not be blocked on systemd.

Tune with `HMEVAL_CPUQUOTA` (default `200%`, i.e. two cores) and
`HMEVAL_MEMORYMAX` (default `8G`).

## How `--ci` carries uncommitted work

The case worth offloading is the one where you have just edited `home.nix` and
committed nothing. So `hmeval --ci` builds a commit out of the **working tree**,
dirty files and untracked files included, and force-pushes it to `eval/<host>`.

It must not disturb your checkout to do that, and on a machine where several
agent sessions share this repo, "must not" is load-bearing — a stray `git add
-A` against the real index sweeps another session's half-finished work into
whatever gets committed next. So the tree is assembled against a private
`GIT_INDEX_FILE` and committed with `git commit-tree`: your index, HEAD and
working tree are never written. `tests/hmeval.sh` pins that.

It carries whatever is on disk, which includes **other sessions' uncommitted
work** when more than one agent is editing this repo. That is the intended
behaviour — it evaluates your actual tree — but it means a red result may belong
to somebody else's half-finished edit. Read the `error:` line before assuming it
is about your change.

The scratch branch is **not** deleted when the command exits, which is
deliberate and counter-intuitive. A branch deletion is itself a push event on
`refs/heads/eval/<host>`, so it matches this workflow's `branches: ["eval/**"]`
and starts a run of its own. That run joins the same concurrency group, and
`cancel-in-progress: true` then has it kill whichever real run is in flight —
and because the forge processes the deletion slightly behind the push, what it
kills is the *next* invocation's run. Runs 140, 141 and 142 were each cancelled
by the cleanup of the invocation before them, which presents as flaky CI with
nothing in the log pointing at cleanup.

So one `eval/<host>` ref per host stays on the repo, force-pushed over on every
run. Nothing reads it in between.

## The contract between the two halves

`nix-eval.yml` reads a `.hmeval-request` file out of the scratch commit —

```
mode=eval|attr|tests
hosts=utsuho setsuna …
tests=tests/foo.sh …
attr=…
```

— and fences its output between `---8<--- hmeval` and `---8<--- end`. `hmeval`
slices the job log between those markers.

Two things to keep in step if you touch either side:

- **The markers.** Rename one and every run reports "no result block" instead of
  an answer. `hmeval` says so rather than exiting silently green, which is the
  only reason this is survivable.
- **Single-line values.** The workflow parses the request with `while IFS='='
  read -r k v` in pure bash, because the `nixos/nix` image has no awk, sed or
  grep (same constraint as `nix-home.yml` — see `docs/ci-nix-home.md`). A
  newline in a value would silently become a second key, so `hmeval` rejects one
  before writing the file.

`hmeval` measures the log's timestamp prefix off the opening marker rather than
assuming a width. `hms` hardcodes 29 characters, which is fine for finding
`error:` lines and not fine here: off by one, the anchored match fails and a
green run reports nothing.

## What this workflow deliberately does not do

**It does not push to attic.** `nix-home.yml` owns the closure cache. A scratch
eval runs on a commit that may never reach `master`, and letting it populate the
cache would mean paths built from abandoned work outliving the branch they came
from.

It also cancels in-progress runs on a new push, unlike `nix-home.yml`. A
discarded closure build wastes twenty minutes someone is waiting on; a
superseded eval is just a stale answer, and `hmeval` is already watching the
newer run.

## tests/ and the runner's hostname

Several scripts in `tests/` build the thing they test from
`homeConfigurations.$(cat /etc/hostname)`, which is meaningless in a container
whose hostname is its own ID. The workflow writes `TEST_HOST` (utsuho) into
`/etc/hostname` before running them, so every script using that idiom works
without growing a new seam. The container is ephemeral; nothing outlives the
job.

## Known duplication

`home/scripts/forge-api.sh` holds the forge REST helpers — token minting, the
per-request re-mint that `tests/hms-ci-poll.sh` exists to pin, run polling and
job-log fetching. `hmeval` sources it. **`hms` still carries its own copy**;
moving it across is mechanical but changes the one script that deploys every
host, so it is a separate commit, verified by running `tests/hms-ci-poll.sh`
through `hmeval --tests`.
