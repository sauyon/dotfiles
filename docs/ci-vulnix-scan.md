# CI: weekly CVE scan (`.woodpecker/vulnix-scan.yml`)

A Woodpecker cron runs [vulnix](https://github.com/nix-community/vulnix) against
the **utsuho** home-manager closure once a week and prints the affected-package
count to the pipeline log. It reuses the same in-cluster pod pattern as
`nix-home.yml` (nixos/nix on fujiwara, attic for fast substitution).

The scan **never fails on findings**: vulnix exits non-zero whenever it matches
any CVE by name/version, and this closure always has some — mostly false
positives (Haskell libs like `vault`/`warp` colliding with unrelated CVE IDs,
build-only `go`/`cargo`/`gcc` bootstraps) or genuine-but-unpatched-upstream
issues that a flake bump can't fix. So a red pipeline would be noise. Green
means the scan ran; read the log for the count and the top offenders.

## One-time bootstrap

Requires the cluster's Woodpecker (`woodpecker.ko.ag`) with `sauyon/dotfiles`
already enabled (done for `nix-home.yml`).

1. **Create the cron.** In the Woodpecker UI: repo → Settings → Crons → Add.
   - Name: `weekly` (the pipeline filters on `cron: weekly` — must match).
   - Schedule: e.g. `0 0 6 * * 1` (Mondays 06:00 UTC; Woodpecker uses 6-field
     cron with a leading seconds field).
2. **Secrets.** None. The `kube` cache public key is public and inlined in the
   pipeline; no push token — this pipeline only reads from the cache.
3. **Trigger a first run.** Hit **Run** in the UI (the pipeline also declares
   `event: manual`) to confirm it works before waiting for the cron.

## Verify

- Woodpecker: the `scan` step goes green and its log ends with a
  `vulnix: N affected packages in utsuho closure` banner plus the top-60 table.

## Notes

- Scans only utsuho — the Linux hosts share almost the entire closure, so the
  other boxes' deltas aren't worth a separate run. Add more
  `.#homeConfigurations.<host>.activationPackage` targets if that changes.
- Want a real push notification instead of reading logs? Wire a Woodpecker
  notification plugin (matrix/email) as a final step gated on the count, or flip
  the scan to `exit 1` when `count > 0` and rely on Woodpecker's failure
  notifications — at the cost of a permanently red pipeline.
