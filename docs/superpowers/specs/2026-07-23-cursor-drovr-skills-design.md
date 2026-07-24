# Cursor drovr skills

## Goal

Make drovr's `using-drovr`, `handoff`, and `pipeline` skills available globally
to Cursor while keeping the drovr flake input as their single source of truth.

## Design

Home Manager will create `~/.cursor/skills/drovr` as a directory symlink to
`${drovr-pkg}/share/drovr/skills`. Cursor recursively discovers `SKILL.md`
files beneath its global skills directory and follows directory symlinks, so
all three packaged skills become available without copying or independently
enumerating them.

The existing Claude plugin configuration remains unchanged. Both agents use
the same pinned drovr package, preventing their skill definitions from
drifting apart.

## Verification

Evaluate the Home Manager configuration and confirm that the generated home
files include `.cursor/skills/drovr` pointing at the drovr package's skills
directory.
