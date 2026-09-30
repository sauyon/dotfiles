{ lib, ... }:

# Even Terminal (binary `even-terminal`) — Even Realities' bridge between a
# terminal coding agent and their G2 glasses: it runs a local HTTP server, spawns
# Claude Code or Codex as a child process, renders the agent's streaming output
# onto the G2's 576x288 canvas, and turns R1 ring gestures back into keystrokes.
# Useless without the hardware and the Even phone app paired to it.
#
# npm-only (`@evenrealities/even-terminal`), not in nixpkgs, and unlike
# `kimi-code.nix` there is no single-file release binary to fetch — so this is a
# real `buildNpmPackage` over the published tarball.
#
# The tarball ships `dist/` already compiled but no lockfile, and `npm ci` needs
# one. `even-terminal-package-lock.json` beside this file is that lockfile,
# generated against the tarball's own `package.json` so its root entry is
# `@evenrealities/even-terminal@<version>` — a lock whose root names some wrapper
# package instead makes `npm ci` abort with "package.json and package-lock.json
# are not in sync".
#
# To bump: change `version`, then regenerate BOTH hashes and the lockfile. The
# lockfile is not optional to regenerate — a stale one pins the old dependency
# set onto the new tarball and `npm ci` fails the sync check.
#
#   V=0.10.5
#   T=https://registry.npmjs.org/@evenrealities/even-terminal/-/even-terminal-$V.tgz
#   nix store prefetch-file --json --hash-type sha256 "$T"   # -> src.hash
#   mkdir -p /tmp/et && curl -fsSL "$T" | tar xz -C /tmp/et --strip-components=1
#   nix shell nixpkgs#nodejs --command npm install --prefix /tmp/et \
#     --package-lock-only --ignore-scripts
#   cp /tmp/et/package-lock.json even-terminal-package-lock.json
#
# Then set `npmDepsHash` to lib.fakeHash, build once, and take the hash the
# mismatch prints.
let
  version = "0.10.5";
in
{
  nixpkgs.overlays = [
    (final: prev: {
      even-terminal = prev.buildNpmPackage {
        pname = "even-terminal";
        inherit version;

        src = prev.fetchurl {
          url =
            "https://registry.npmjs.org/@evenrealities/even-terminal/-/even-terminal-${version}.tgz";
          hash = "sha256-GB+ou++BmNS0+VGxxSgQqUSla84ZEn4mTveYB/Uf0uA=";
        };

        postPatch = ''
          cp ${./even-terminal-package-lock.json} package-lock.json
        '';

        npmDepsHash = "sha256-mRt3kITsBhL0Ds7r3UXyKrRXN3lETU8qfI+cn/cY0fk=";

        # `dist/` is published prebuilt and there is no `build` worth rerunning:
        # the package's own build script is `rm -rf dist && tsc`, so running it
        # would delete the shipped output and then need the TypeScript toolchain
        # to put it back.
        dontNpmBuild = true;

        # Same trap by another route. buildNpmPackage's install phase shells out
        # to `npm pack`, and this package's `prepack` hook is `npm run build` —
        # i.e. that same `rm -rf dist && tsc`. Without --ignore-scripts the pack
        # step empties dist/ and the install fails on a package with no code in
        # it. This flag is load-bearing, not hygiene.
        npmPackFlags = [ "--ignore-scripts" ];

        # node-pty has no linux prebuild in its npm tarball (only darwin and
        # win32), and its install script is `node scripts/prebuild.js ||
        # node-gyp rebuild`. prebuild.js only checks for a prebuilds/<platform>-
        # <arch> directory and exits 1 when it is missing — it never reaches out
        # to the network — so on Linux this always falls through to a local
        # node-gyp build, which needs python3. Harmless on darwin, where the
        # prebuild is present and gyp never runs.
        nativeBuildInputs = [ prev.python3 ];

        # Guards both script traps above: a build that silently packed an empty
        # dist/ still produces a $out with a bin/ in it, and only fails when
        # someone runs it. `--version` is the cheapest path that actually loads
        # the entrypoint.
        doInstallCheck = true;
        installCheckPhase = ''
          runHook preInstallCheck

          $out/bin/even-terminal --version

          # node-pty is loaded lazily -- only when a session spawns an agent --
          # so `--version` passes with the native addon missing entirely, which
          # is the exact failure a source build of it would produce. Assert the
          # compiled module is in the closure. Searched rather than named: on
          # Linux node-gyp writes build/Release/pty.node, on darwin the shipped
          # prebuilds/darwin-*/pty.node is used as-is.
          [ -n "$(find $out/lib/node_modules -name pty.node -print -quit)" ]

          runHook postInstallCheck
        '';

        meta = {
          description =
            "Even Realities' CLI bridging a terminal coding agent to G2 glasses";
          homepage = "https://www.npmjs.com/package/@evenrealities/even-terminal";
          license = lib.licenses.unfree;
          mainProgram = "even-terminal";
          platforms = lib.platforms.unix;
        };
      };
    })
  ];
}
