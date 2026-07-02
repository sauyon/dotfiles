# CI: build home closures in-cluster (`.woodpecker/nix-home.yml`)

Push to `master` (touching any nix/home source) builds the **Linux** home-manager
closures — `homeConfigurations.{utsuho,setsuna,fujiwara}.activationPackage` — in a
Woodpecker pod on **fujiwara**, and pushes them to the `kube` attic cache
(`attic.ko.ag/kube`). Then `home-manager switch --flake .#<host>` just downloads
the prebuilt closure instead of compiling locally.

This mirrors the kube repo's `.woodpecker/nix-nodes.yml`; see that repo's
`docs/nix-binary-cache.md` for the build-pod isolation rationale (each build runs
in its own ephemeral `/nix`, never the host store). mari (aarch64-darwin) is
omitted — an x86_64-linux pod can't build darwin; darwin still builds on mari
itself.

## Consuming side (already wired)

The boxes pull from `attic.ko.ag/kube` via `system/etc/nix/nix.custom.conf`
(deployed by `system/deploy`). On plain upstream nix (e.g. utsuho) the deploy
adds the `!include` + renders the pull token to `/etc/nix/netrc`; on Determinate
it's automatic. Nothing else to do to *consume* the cache.

## One-time bootstrap (producing side)

Requires the cluster's Woodpecker (`woodpecker.ko.ag`, GitHub OAuth, admin
`sauyon`):

1. **Enable the repo.** In the Woodpecker UI, add `sauyon/dotfiles` (this installs
   the GitHub webhook so pushes trigger builds).
2. **Provide the secrets** the pipeline reads via `from_secret`:
   - `ATTIC_TOKEN` — attic **push** token (write-scoped).
   - `ATTIC_PUBLIC_KEY` — the `kube` cache public key
     (`kube:YLRejBKnIVKqvZRXBvFR4KmosPZPg9phiM+pRlhbQ+c=`).
   If these already exist as Woodpecker **org/global** secrets (the kube repo uses
   the same names), the dotfiles repo inherits them; otherwise add them as
   repo secrets. The push token is write-capable — keep it out of the repo.
3. **Trigger it.** Push a nix/home change, or hit **Run** in the UI (the pipeline
   declares `event: manual`).

## Verify

- Woodpecker: the `build-and-push` step goes green.
- From a box: after the build, `home-manager switch --flake .#utsuho` should show
  the closure being *fetched* from `https://attic.ko.ag/kube` rather than built.
