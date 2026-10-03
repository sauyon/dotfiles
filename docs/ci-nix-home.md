# CI: build home closures in-cluster (`.forgejo/workflows/nix-home.yml`)

Push to `master` (touching any nix/home source) builds the **Linux**
home-manager closures — `homeConfigurations.{utsuho,setsuna,fujiwara}.activationPackage`
— on the in-cluster `forgejo-runner` at `forge.ko.ag`, and pushes them to the
in-cluster attic Service (`http://attic.attic.svc.cluster.local/kube`, cache
name `kube`). Box-side pulls use that same URL — see `docs/nix-binary-cache.md`.
Then `home-manager switch --flake .#<host>` just downloads the prebuilt closure
instead of compiling locally.

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

## Consuming side

The boxes pull from the in-cluster attic Service over whatever reaches the
cluster network — LAN, or WireGuard (`kon-wireguard`, installed by
`system/deploy:213` for shiori; the other Linux boxes carry the same tunnel
out-of-band). The substituter URL itself was switched from
`https://attic.ko.ag/kube` to `http://attic.attic.svc.cluster.local/kube` in
`system/etc/nix/nix.custom.conf`; `flake.nix` carries the same change for
`mari`. Wires through `system/deploy` are documented in `docs/nix-binary-cache.md`.

The change is the box-side twin of what CI did first: substituting through
Cloudflare was the surface the 601 s response ceiling lived on, and taking it
out of the path removed the 307 / stream-error class that made pulls flaky from
boxes.

`hms`'s `switch_now` (`home.nix`) passes `-- --fallback` to `home-manager
switch`, mirroring the `--fallback` the CI build step passes on
`nix-home.yml:132`. With Cloudflare gone the failure mode is different on each
side, but the knob is the same:

- **CI side.** The substituter is the very Service the runner just pushed to,
  seconds to minutes earlier. A NAR the Service can't hand back is almost
  always a transient hiccup — Service reload, RTT spike, attestation blip —
  and a 25-minute build ending in `no substituter that can build it` is a worse
  outcome than that one path being built locally. `--fallback` degrades that.
- **Box side.** The substituter is the same Service, but the boxes' reach to
  it is the *Kon WireGuard overlay* (shiori via the profile committed in
  `c8c17274`; the other Linux boxes carry the same tunnel out-of-band). A
  switch that lands 30 seconds after you ran `hms` already told you things were
  green; aborting it at minute 10 because the in-cluster Service happened to
  reload during one NAR transfer is the *same* worse outcome. `--fallback`
  degrades that too.

The earlier draft of this paragraph argued --fallback masked a real wire
breakage, which the Cloudflare ceiling very much was; with Cloudflare out of
the path, what `--fallback` masks is a per-NAR transient, not the cause of
the failure class. hms --local goes through the same `switch_now` and gets the
same flag.

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
- The build log should show paths being *fetched* from
  `http://attic.attic.svc.cluster.local/kube`, not built. If it compiles from
  scratch, the netrc step is broken — that is the substituter silently failing
  open, not a cache miss.
- From a box: after the build, `home-manager switch --flake .#utsuho` should
  show the closure being *fetched* (from `http://attic.attic.svc.cluster.local/kube`)
  rather than built.

## The private input

The flake takes `dotfiles-private` (`git+https://forge.ko.ag/sauyon/dotfiles-private.git`),
a non-flake tree holding the values the public repo deliberately does not carry:
the git identity, internal endpoints, the new-tab links and the per-host posture
for Claude Code's auto-mode classifier. It is needed at **eval** time, so no host
and no CI job can evaluate this flake without read access to it.

Locally that access is `git-credential-fj` (declared in `home.nix`), which is why
a fresh host must `fj login` before its first switch. In CI it is the repo secret
`FORGE_TOKEN`, a Forgejo token scoped to `read:repository`, handed to git as a
credential helper that answers from the environment.

**nix fetches `git+https` by shelling out to git**, so the attic `netrc-file`
does nothing for this input -- a netrc line is the wrong fix and looks like it
should work. If a run fails with an auth error against `forge.ko.ag`, check that
`FORGE_TOKEN` is set on the *build* step and not only on the step that configures
the helper; the helper is invoked by git at fetch time, inside `nix build`.
