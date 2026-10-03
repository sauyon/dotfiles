{ config, lib, pkgs, ... }:

# MiniMax Code (binary `mcode`) — MiniMax's open-source terminal coding agent,
# github.com/MiniMax-AI/minimax-code. Used here to drive MiniMax-M3 code
# sessions against Modular's own deployments rather than MiniMax's hosted API;
# the provider wiring lives in `home.nix`, this file only builds the CLI.
#
# npm-only (`@minimax-ai/code`), not in nixpkgs, and like `even-terminal.nix`
# there is no single-file release binary — upstream's own installer downloads a
# private Node runtime and runs `npm install` into ~/.minimax-code. So this is a
# `buildNpmPackage` over the published tarball, and the lockfile beside this
# file (`mcode-package-lock.json`) is generated against the tarball's own
# `package.json` so `npm ci`'s sync check passes.
#
# To bump: change `version`, then regenerate BOTH hashes and the lockfile. A
# stale lock pins the old dependency set onto the new tarball and `npm ci`
# aborts on the sync check.
#
#   V=0.6.2
#   T=https://registry.npmjs.org/@minimax-ai/code/-/code-$V.tgz
#   nix store prefetch-file --json --hash-type sha256 "$T"   # -> src.hash
#   mkdir -p /tmp/mc && curl -fsSL "$T" | tar xz -C /tmp/mc --strip-components=1
#   nix shell nixpkgs#nodejs --command npm install --prefix /tmp/mc \
#     --package-lock-only --ignore-scripts
#   cp /tmp/mc/package-lock.json mcode-package-lock.json
#
# Then set `npmDepsHash` to lib.fakeHash, build once, and take the hash the
# mismatch prints.
let
  version = "0.6.2";
in
{
  nixpkgs.overlays = [
    (final: prev: {
      mcode = prev.buildNpmPackage {
        pname = "mcode";
        inherit version;

        src = prev.fetchurl {
          url =
            "https://registry.npmjs.org/@minimax-ai/code/-/code-${version}.tgz";
          hash = "sha256-3tEBFEfUKuNC0sYvJZXzAkYWa3+rEjFuk6TQr/rbTMc=";
        };

        postPatch = ''
          cp ${./mcode-package-lock.json} package-lock.json

          # Drop the root postinstall (`node ./verify-native-install.mjs`). It
          # is a diagnostic for npm-based installs — open an in-memory database,
          # `SELECT 1`, print a verdict — and it gets the verdict right here:
          # "[MCode] Native SQLite check passed." Then the process SIGABRTs on
          # the way out, inside better-sqlite3's `Database` destructor:
          #
          #   node::RemoveEnvironmentCleanupHook ... Assertion `(env) != nullptr'
          #
          # which is teardown ordering between the addon and this nixpkgs' Node
          # 24.20.0, not a broken build — npm sees only the signal and fails the
          # install. `installCheckPhase` below asserts the same two things the
          # script does, from the built output, so nothing is lost by removing
          # it. Re-check on a Node bump: if the abort is gone upstream, this
          # patch can go.
          ${prev.jq}/bin/jq 'del(.scripts.postinstall)' package.json > package.json.new
          mv package.json.new package.json
        '';

        npmDepsHash = "sha256-9h1OwKKL53S32mQaDr03feC/l9rG7hL6Id2Qn5KKXys=";

        # `chunks/` ships prebuilt (esbuild output) and the package declares no
        # build script at all — only a postinstall. Left to its default,
        # buildNpmPackage would fail looking for one.
        dontNpmBuild = true;

        # better-sqlite3 is declared `optional`, which is a lie as far as this
        # CLI is concerned: the session store is drizzle over a static
        # `import ... from "better-sqlite3"`, and the package's own postinstall
        # (`verify-native-install.mjs`) opens an in-memory database and runs
        # `SELECT 1` against it. Both fail hard without the native addon, so the
        # optional marker must not be allowed to turn a failed build into a
        # silent skip — which is exactly what it would do.
        #
        # Its install script is `prebuild-install || node-gyp rebuild`.
        # prebuild-install fetches a prebuilt .node from GitHub and there is no
        # network in the sandbox, so forcing the source build skips a download
        # that can only fail and goes straight to node-gyp, which needs python3.
        npm_config_build_from_source = "true";

        nativeBuildInputs = [ prev.python3 prev.makeWrapper ]
          ++ lib.optionals prev.stdenv.hostPlatform.isLinux [
            prev.autoPatchelfHook
          ];

        # @vscode/ripgrep resolves `rg` out of a per-platform optional package
        # that ships a prebuilt binary (1.18.0 dropped the download-on-install
        # step). It is linked against a system loader, so on Linux it needs
        # patchelf before it will execute at all.
        buildInputs = lib.optionals prev.stdenv.hostPlatform.isLinux [
          prev.stdenv.cc.cc.lib
        ];

        # buildNpmPackage's install phase shells out to `npm pack`, which would
        # re-run the root postinstall — the native SQLite smoke test — against a
        # staging tree that has no node_modules yet. It fails there for reasons
        # that say nothing about the build.
        npmPackFlags = [ "--ignore-scripts" ];

        postInstall = ''
          # Belt to autoPatchelf's braces: mcode prefers @vscode/ripgrep's
          # bundled binary and falls back to `rg` on PATH ("the bundled
          # @vscode/ripgrep binary is unavailable and `rg` is not on PATH"). The
          # fallback is only a fallback if something is there to find, and
          # leaving it to whatever happens to be on the user's PATH makes file
          # search silently depend on an out-of-closure tool.
          #
          # The --run block is the key handoff. A custom provider stores the
          # NAME of an environment variable (`apiKeyEnv`), never the secret, so
          # the value has to be in the process environment at launch. Reading it
          # here keeps it to this process rather than exporting it into every
          # interactive shell, and leaves an explicit MCODE_PROVIDER_API_KEY
          # ahead of the file so a one-off key still wins.
          #
          # That path is `modularApiKey`'s sops destination, shared with
          # opencode's mcloud provider and pi — one Modular key, one file, as
          # `home.nix` already renders it. Not a second copy of the secret.
          for b in mcode mcode-tools; do
            wrapProgram $out/bin/$b \
              --prefix PATH : ${lib.makeBinPath [ prev.ripgrep ]} \
              --run 'if [ -z "''${MCODE_PROVIDER_API_KEY:-}" ] && [ -r "$HOME/.config/local-auto-mode/api-key" ]; then MCODE_PROVIDER_API_KEY="$(cat "$HOME/.config/local-auto-mode/api-key")"; export MCODE_PROVIDER_API_KEY; fi'
          done
        '';

        # Guards the better-sqlite3 trap above: an `npm ci` that skipped the
        # optional native build still produces a $out with both binaries in it,
        # and only fails when someone opens a session.
        doInstallCheck = true;
        installCheckPhase = ''
          runHook preInstallCheck

          $out/bin/mcode --version

          [ -n "$(find $out/lib/node_modules -name better_sqlite3.node -print -quit)" ]

          runHook postInstallCheck
        '';

        meta = {
          description = "MiniMax Code, MiniMax AI's terminal coding agent";
          homepage = "https://github.com/MiniMax-AI/minimax-code";
          license = lib.licenses.mit;
          mainProgram = "mcode";
          platforms = lib.platforms.unix;
        };
      };
    })
  ];

  # Register the Modular deployment as a custom provider. Written by mcode's own
  # `provider add` rather than by rendering config.yaml here: the custom-provider
  # schema is internal and undocumented, and a hand-built mapping that drifts
  # from it fails as "config.yaml could not be parsed" with the whole file dead,
  # not just the one entry.
  #
  # Both inputs come from sops at runtime, so neither the key nor the internal
  # hostname is in the committed config or in /nix/store — same split as
  # opencode's mcloud provider, which dials this exact gateway.
  #
  # Guarded on the provider id being absent, because `provider add --use` runs a
  # live connection check against the endpoint. Unguarded, every `hms` would
  # depend on the deployment being up, and a scaled-down node would fail an
  # otherwise unrelated switch. `|| true` for the same reason: registration is
  # worth attempting at activation, never worth breaking it.
  home.activation.mcodeProvider = lib.hm.dag.entryAfter [ "writeBoundary" "sops-nix" ] ''
    CONFIG="$HOME/.minimax/config.yaml"
    KEY_FILE="$HOME/.config/local-auto-mode/api-key"
    URL_FILE="$HOME/.config/opencode/mcloud-base-url"

    if [ -r "$KEY_FILE" ] && [ -r "$URL_FILE" ] \
       && ! ${pkgs.gnugrep}/bin/grep -q 'modular-oldprod' "$CONFIG" 2>/dev/null; then
      $DRY_RUN_CMD env \
        MCODE_PROVIDER_API_KEY="$(cat "$KEY_FILE")" \
        ${pkgs.mcode}/bin/mcode provider add \
          --name modular-oldprod \
          --base-url "$(cat "$URL_FILE")" \
          --api-format openai-completions \
          --model MiniMaxAI/MiniMax-M3-OldProd-Load \
          --api-key-env MCODE_PROVIDER_API_KEY \
          --use || true
    fi
  '';
}
