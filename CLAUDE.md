## NO EMAILS

**Never write an email address into anything.** Not into files, commit messages,
docs, notes, handoffs or scratch files — mine, colleagues', or service accounts.
This repo is public, and an address is a durable identifier that spreads: it
leaks the work/personal boundary, it enumerates people, and once written it gets
copied onward.

Refer to an account by its **role** — "the personal Google account", "the work
account", "the project's GCS service agent" — or record the command that
recovers the identity at read time (`gcloud config get-value account`) rather
than pasting its output. Role-based wording is better documentation anyway: "must
be the personal account, not the work one" survives an address change; a pasted
address does not.

If an address seems genuinely load-bearing, ask first.

The exception is the git identity already configured in `home.nix`, which git
requires and which is on every commit regardless.

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
