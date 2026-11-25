---
name: update
description: >-
  Use when the user asks to run the update skill or update dependencies, flake
  inputs, nixpkgs, or pinned packages in this dotfiles repo. Pulls latest
  versions, audits changes for malware, vulnerabilities, and usage regressions,
  verifies builds/tests, pushes to master, and runs hms.
---

# Dotfiles Update

Update all flake inputs (including `nixpkgs`) and repo-pinned packages, audit
every change before accepting it, verify, push to `master`, and apply with
`hms`.

## 1. Sync & Pull Latest Versions

1. Confirm the working tree is clean and based on the latest `origin/master`.
2. Update all flake inputs (including `nixpkgs`):
   ```bash
   nix flake update
   ```
3. Check hand-pinned packages in the repo for newer releases and bump versions,
   revs, and hashes where available (e.g. `orca.nix` + its skills rev and
   generated hooks, `cursor-agent.nix`, `kimi-code.nix`, `delta.nix`,
   `gwscli.nix`, `nix/gascity.nix`, `nix/forgejo-cli.nix`, NUR / `claude-code` /
   `claude-agent-acp` pins in `home.nix`, and `uv` in `mise.toml`).

## 2. Audit Every Change

Do not apply unreviewed bumps. For each flake input or pinned package whose
revision actually moved, inspect the diff, changelog, or unpacked release
against three checks:

1. **Malware & supply-chain**
   - Verify release digests/checksums against upstream manifests (e.g.
     `latest-linux.yml` / GitHub asset sha512 for Orca).
   - Check newly added dependencies, install/postinstall scripts, obfuscated or
     encoded blobs, `eval`/`new Function` additions, and unexpected network
     endpoints or telemetry.
2. **Vulnerabilities**
   - Check for known CVEs or advisories on bumped packages and lockfiles
     (nixpkgs insecure flags, `npm`/`cargo` lockfile advisories, Brotli/Node
     checks).
3. **Regressions vs. how this repo uses it**
   - Cross-check upstream changes against our local config, wrappers, hooks,
     and patches (`patches/`, `nix/orca-*.js|ts`, `home.nix`, `orca.nix`,
     `hyprland.nix`, `zsh.nix`, etc.).
   - Watch for subtle behavioral regressions: changed default or legacy-seeded
     keybindings (e.g. Orca capturing `C-k` via legacy search bindings),
     renamed minified symbols in patched bundles, changed CLI/hook schemas, or
     dropped options.
   - If an upstream fix supersedes a local patch, drop the patch and its test.

Hold back or fix anything that fails audit, and escalate to the user if a
suspicious change or breaking regression requires a decision.

## 3. Evaluate & Test (on CI)

Always offload evaluation and test builds to the CI runner — **do not run local
`hmeval`, local `nix build`, or local `tests/*.sh` that realise derivations**,
since concurrent local builds can saturate and lock up the host:

1. Evaluate all hosts on the runner:
   ```bash
   hmeval --ci
   ```
2. Run relevant suites on the runner via `hmeval --tests` — at minimum:
   ```bash
   hmeval --tests \
     tests/insecure-packages.sh \
     tests/brotli-cve-2026-111497.sh \
     tests/nur-pin.sh \
     tests/hyprlock-pending-race.sh
   ```
   - For Orca bumps, also pass `tests/orca-cli-skills.sh`,
     `tests/orca-agent-hooks.sh`, and `tests/orca-mac-keybindings.sh`
     (`tests/orca-gui-unsandboxed.sh` probes the live desktop host's
     fontconfig/systemd/coredumpctl, so run it on the desktop host after
     `hms` if needed rather than inside `hmeval --tests`).
   - Include any other `tests/*.sh` covering bumped components
     (`tests/agent-clis.sh`, `tests/gascity-packages.sh`,
     `tests/zed-modular.sh`, etc.).

## 4. Commit & Push to `master`

1. Stage and commit the updates with a concise summary of bumped versions and
   audit findings. Follow `CLAUDE.md`: **never include email addresses**.
2. Push to `origin master` and fast-forward the local `master` checkout
   (`AGENTS.md`; stash and restore if other work is staged there).

## 5. Apply with `hms`

Run `hms` from the clean `master` checkout so CI builds and caches the closure
before switching (expect ~10 minutes). Do not substitute bare
`home-manager switch`, do not use `hms --local` unless explicitly requested,
and do not pipe `hms` into `tail`.
