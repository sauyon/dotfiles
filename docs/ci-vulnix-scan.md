# CI: weekly CVE scan (`.forgejo/workflows/vulnix-scan.yml`)

[vulnix](https://github.com/nix-community/vulnix) runs against the **utsuho**
home-manager closure every Monday at 06:00 UTC and prints the affected-package
count to the job log. Same job shape as `nix-home.yml` (nixos/nix container on
the in-cluster `forgejo-runner`, attic for fast substitution).

The scan **never fails on findings**: vulnix exits non-zero whenever it matches
any CVE by name/version, and this closure always matches some — mostly false
positives (Haskell libs like `vault`/`warp` colliding with unrelated CVE IDs,
build-only `go`/`cargo`/`gcc` bootstraps) or genuine-but-unpatched-upstream
issues a flake bump can't fix. So a red pipeline would be noise. Green means the
scan ran; read the log for the count and the top offenders.

## The schedule is in this repo now

That is the whole reason for the migration. Under Woodpecker the cron lived only
in the server's database — created by hand in the UI, invisible to the repo,
unversioned, and gone with the database. It now lives in the workflow's
`on: schedule` block.

Two things changed in the move:

- **5-field cron, not 6.** Woodpecker used a leading-seconds field
  (`0 0 6 * * 1`); Forgejo uses standard `0 6 * * 1`. Copying the old expression
  over silently means something else.
- **No `cron: weekly` name to match.** The old pipeline filtered on a cron *name*
  that had to agree with the UI entry — a mismatch meant it simply never fired.
  That coupling is gone.

## It needs the attic token even though it only reads

`attic.ko.ag` is not anonymously readable (401), and nix treats a substituter it
cannot authenticate to as absent — with no warning. The Woodpecker version of
this pipeline passed `--extra-substituters` with no credential at all, so it
never substituted anything and rebuilt the closure from source every week. The
workflow now writes a netrc from the repo secret `ATTIC_TOKEN` before the first
nix command. Do not "simplify" that step away as push-only.

## Verify

- Dispatch it from <https://forge.ko.ag/sauyon/dotfiles/actions> rather than
  waiting a week (Forgejo has no re-run API, so `workflow_dispatch` is also the
  retry path).
- The `scan` job goes green and its log ends with a
  `vulnix: N affected packages in utsuho closure` banner plus the top-60 table.
- The realise step should *fetch* the closure. If it compiles, the netrc step is
  broken.

## Notes

- Scans only utsuho — the Linux hosts share almost the entire closure, so the
  other boxes' deltas aren't worth a separate run. Add more
  `.#homeConfigurations.<host>.activationPackage` targets if that changes.
- Want a real notification instead of reading logs? Add a final step gated on the
  count, or flip the scan to `exit 1` when `count > 0` and rely on Forgejo's
  failure notifications — at the cost of a permanently red pipeline.
