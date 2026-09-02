## Commit and Push Policy

This is a personal dotfiles repo on `master`. When asked to commit and push, do both without asking for confirmation. The global "ask before public communications" rule does not apply here.

## Applying config: use `hms`, not `home-manager switch`

`hms` (defined in `home.nix`, documented in `README.md`) pushes, waits for the
commit's CI run on `forge.ko.ag`, then switches — so the switch is a download of
the closure the in-cluster runner built, not a local compile. Expect it to block
for ten-odd minutes. `hms --local` builds here instead.

It refuses on a dirty tree and on a checkout behind `origin/master`. **Both
refusals are the feature.** CI can only build a pushed commit, so do not route
around them with a bare `home-manager switch` — commit and push, or pass
`--local` deliberately and say that you did.

If a run goes red, `hms` prints the job's `error:` lines; read those rather than
re-running blind. Background on why CI is worth waiting for — and the 600s attic
transfer ceiling that made it unreliable for its first 61 runs — is in
`docs/ci-nix-home.md`.
