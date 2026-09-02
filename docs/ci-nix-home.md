# CI: build home closures in-cluster (`.forgejo/workflows/nix-home.yml`)

Push to `master` (touching any nix/home source) builds the **Linux**
home-manager closures — `homeConfigurations.{utsuho,setsuna,fujiwara}.activationPackage`
— on the in-cluster `forgejo-runner` at `forge.ko.ag`, and pushes them to the
`kube` attic cache (`attic.ko.ag/kube`). Then `home-manager switch --flake .#<host>`
just downloads the prebuilt closure instead of compiling locally.

mari (aarch64-darwin) is omitted — an x86_64-linux job can't build darwin; darwin
still builds on mari itself.

Migrated from Woodpecker on 2026-08-03. The cluster-side story — runner sizing,
why the job container is unprivileged, how to read a failed job's log — is in the
kube repo's `docs/forgejo-dotfiles-ci.md`. Read that before changing the runner
or debugging an infrastructure failure.

## Editing this workflow

Two constraints that are not obvious from the file:

- **No `uses:` steps.** The job runs in the `nixos/nix` image, which has `git`
  and `bash` but no `node`, so no JS action can execute — `actions/checkout`
  included. The checkout is a hand-rolled `git clone`. Adding a `uses:` step
  fails at "Set up job".
- **The attic netrc must be written before the first nix command.** attic is not
  anonymously readable, and nix silently ignores a substituter it cannot
  authenticate to. Reorder that step after the build and the job stops
  substituting and rebuilds everything from source, with no error to say so.
- **The job substitutes from the in-cluster Service, not `attic.ko.ag`.** Pulling
  through Cloudflare hairpins out of the cluster and back, and that path caps a
  single response at a hard 600s. Every run that had to fetch the 2.44 GiB
  `google-fonts` NAR died at exactly 601.0s with `HTTP error 200 (curl error:
  Stream error in the HTTP/2 framing layer)` — a failure nix does not retry, so
  the whole job died after ten minutes of transfer. 47 of the first 61 runs
  failed this way. Pull and push now both use
  `http://attic.attic.svc.cluster.local`. `nix build` also passes `--fallback`,
  so a substituter that dies mid-NAR degrades to a local build instead of
  killing the run.

There is also a **nix version floor**: `home.nix` merges `programs.gpg.package`
with a later `programs = { gpg = { … } }` block, which nix 2.24 rejects as a
duplicate attribute. The image is pinned to 2.35.1. When bumping it, confirm the
new image still has git, still lacks node, and still ships `sandbox = false`.

## Consuming side (unchanged)

The boxes pull from `attic.ko.ag/kube` via `system/etc/nix/nix.custom.conf`
(deployed by `system/deploy`). On plain upstream nix (e.g. utsuho) the deploy
adds the `!include` + renders the pull token to `/etc/nix/netrc`; on Determinate
it's automatic. Nothing else to do to *consume* the cache.

## Bootstrap (producing side)

1. **Repo.** `sauyon/dotfiles` on forge.ko.ag. Pushing to it triggers the
   workflow — no webhook to install, unlike the Woodpecker/GitHub setup.
2. **Secret.** Repo secret `ATTIC_TOKEN`, an attic push token minted with
   `atticadm make-token --push kube --pull kube` (exact command in the kube doc).
   Expires **2027-08-03**. The cache **public** key is public
   (`kube:YLRejBKnIVKqvZRXBvFR4KmosPZPg9phiM+pRlhbQ+c=`) and is inlined in the
   workflow — no secret needed for it.
3. **Trigger.** Push a nix/home change, or dispatch the workflow from the Actions
   tab (Forgejo has no re-run API, so `workflow_dispatch` is the retry path).

## Verify

- The `build-and-push` job goes green at
  <https://forge.ko.ag/sauyon/dotfiles/actions>.
- The build log should show paths being *fetched* from `https://attic.ko.ag/kube`,
  not built. If it compiles from scratch, the netrc step is broken — that is the
  substituter silently failing open, not a cache miss.
- From a box: after the build, `home-manager switch --flake .#utsuho` should show
  the closure being *fetched* rather than built.
