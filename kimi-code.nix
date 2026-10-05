{ lib, pkgs, ... }:

# Kimi Code CLI (binary `kimi`) — Moonshot's TypeScript terminal agent,
# github.com/MoonshotAI/kimi-code. Not in nixpkgs, so we package the upstream
# release binary the same way `cursor-agent.nix` does.
#
# NOT the same thing as `kimi-cli` (github.com/MoonshotAI/kimi-cli), the legacy
# Python agent. Both install a binary called `kimi`; Kimi Code is the one under
# active development, and its own installer goes out of its way to rename any
# Python shim it finds to `kimi-legacy`. Nothing here installs that one, so the
# collision cannot arise on this machine — but it is why searching for "kimi
# cli" turns up uv/pip instructions that do not apply.
#
# Upstream ships a Node SEA (single executable application): one self-contained
# file with the Node runtime baked in, not an npm tree. So there is no lockfile
# to vendor and no `buildNpmPackage` — just fetch, patchelf, wrap, install. The npm
# route (`@moonshot-ai/kimi-code`) exists too, but its `postinstall` mutates
# $PATH looking for legacy shims, which is exactly the kind of thing that does
# not belong in a nix build.
#
# To bump: read the release manifest, which carries the per-platform sha256 in
# hex, and convert each to SRI.
#
#   V=$(curl -fsSL https://code.kimi.ai/kimi-code/latest)
#   curl -fsSL "https://code.kimi.ai/kimi-code/binaries/$V/manifest.json"
#   nix hash convert --hash-algo sha256 --to sri <hex>
#
# `checksum` in the manifest is the bare binary; the `zstd` block alongside it
# is a different artifact with a different hash. Take `checksum`.
let
  version = "2.1.1";

  # code.kimi.ai is the global mirror; code.kimi.com is the mainland-CN
  # channel. Same versions, same bytes, but the installer derives a `region`
  # marker from whichever domain it was fetched from, so keep these consistent.
  baseUrl = "https://code.kimi.ai/kimi-code/binaries/${version}";

  platformSpec = system:
    {
      x86_64-linux = {
        target = "linux-x64";
        hash = "sha256-ZvR1NuQLArsdV3zdMkdo9z05uLlHLgyhQOc9jiJc194=";
      };
      aarch64-linux = {
        target = "linux-arm64";
        hash = "sha256-i8wKImeg7LH/SMrShPfjZFKKAVmPONaW7BuqawaaiMg=";
      };
      x86_64-darwin = {
        target = "darwin-x64";
        hash = "sha256-7TIVr27riXnJnb4HYUhO2PCBmO8PbKF4Vjf2CxRjypU=";
      };
      aarch64-darwin = {
        target = "darwin-arm64";
        hash = "sha256-S6tvlsLCiTaLfwWgT2ZzfFOlnOdA+TLY8UkkBYR1jvs=";
      };
    }.${system} or null;

  # The keys of ~/.kimi-code/config.toml this repo owns, as JSON for the
  # activation merge below. Everything NOT in here — the [models.*] catalogue
  # a `/models` refresh writes, the [services.*] blocks, [thinking], any
  # hand-edit — is kimi's to keep.
  #
  # Pinning [providers."managed:kimi-code"] is not just tidiness: kimi derives
  # the on-disk OAuth credential name from the provider's oauth_host
  # ("kimi-code-env-<hash>", stored as ~/.kimi-code/credentials/<name>.json),
  # NOT from anything per-device. With the same base_url/oauth_host every box
  # resolves the SAME filename — kimi-code-env-0e4f99c69cc27850.json — which
  # is exactly the file the kimiCodeCredentials sops secret in home.nix
  # deploys. Drift here (say upstream moves the default host and a box picks
  # it up via /login) and that box silently mints a new credential name the
  # shared secret no longer applies to.
  kimiOwnedConfig = pkgs.writers.writeJSON "kimi-code-owned-config.json" {
    default_permission_mode = "yolo"; # "Never Ask"; per docs: manual/yolo/auto
    default_model = "kimi-code/k3";
    providers."managed:kimi-code" = {
      type = "kimi";
      base_url = "https://api.kimi.ai/coding/v1";
      api_key = "";
      oauth = {
        storage = "file";
        key = "oauth/kimi-code-env-0e4f99c69cc27850";
        oauth_host = "https://auth.kimi.ai";
      };
    };
  };
in
{
  nixpkgs.overlays = [
    (final: prev:
      let
        inherit (prev.stdenvNoCC.hostPlatform) system;
        spec = platformSpec system;
      in
      {
        kimi-code = prev.stdenv.mkDerivation {
          pname = "kimi-code";
          inherit version;

          src =
            if spec == null then
              throw "kimi-code: unsupported system ${system}"
            else
              prev.fetchurl {
                url = "${baseUrl}/kimi-code-${spec.target}";
                inherit (spec) hash;
              };

          # A single file, not an archive.
          dontUnpack = true;

          nativeBuildInputs = [
            prev.makeWrapper
          ] ++ lib.optionals prev.stdenv.hostPlatform.isLinux [
            prev.autoPatchelfHook
          ];

          # The SEA statically links most of Node, but still wants libstdc++
          # (V8) and libz from the dynamic loader.
          buildInputs = lib.optionals prev.stdenv.hostPlatform.isLinux [
            prev.stdenv.cc.cc.lib
            prev.zlib
          ];

          installPhase = ''
            runHook preInstall

            install -Dm755 $src $out/libexec/kimi-code/kimi

            # `fd` is a runtime dependency, so it belongs in this closure rather
            # than in anyone's profile: CI then builds it, and a rollback takes
            # it with them. --prefix, per nixpkgs convention for a pinned
            # runtime tool — upstream pins fd 10.4.2 and we are already
            # substituting nixpkgs' build, so leaving the version to whatever
            # happens to be on PATH trades one surprise for another.
            makeWrapper $out/libexec/kimi-code/kimi $out/bin/kimi \
              --prefix PATH : ${lib.makeBinPath [ prev.fd ]}

            runHook postInstall
          '';

          # REQUIRED. stdenv's default strip phase destroys this binary: the SEA
          # blob is injected post-link (postject) as a section the program
          # headers do not cover, so `strip` cannot map it back into a segment
          # and silently rewrites a corpse. It says so — "section `.text' can't
          # be allocated in segment 3", then a run of "allocated section `' not
          # in segment" — and the result SIGSEGVs on any invocation. autopatchelf
          # is fine; it only rewrites the interpreter and RPATH in place.
          dontStrip = true;

          # Guards the above: the failure mode is a binary that builds clean and
          # dies on first launch, so prove it runs before it reaches a profile.
          doInstallCheck = true;
          installCheckPhase = ''
            $out/bin/kimi --version

            # kimi shells out to `fd` for file search, and resolves it off PATH
            # (`resolveCommandPath` reads env.PATH; there is no override
            # variable). Miss it and `downloadFd()` fetches a 4 MiB fd tarball
            # from Moonshot's CDN into ~/.kimi-code/bin at first launch — a
            # runtime dependency outside the closure, invisible to CI and to
            # rollback. Assert the wrapper carries fd from the store instead.
            grep -q '${prev.fd}/bin' $out/bin/kimi
          '';

          meta = {
            description = "Kimi Code CLI, Moonshot AI's terminal coding agent";
            homepage = "https://github.com/MoonshotAI/kimi-code";
            license = lib.licenses.mit;
            mainProgram = "kimi";
            platforms = lib.platforms.unix;
            sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
          };
        };
      })
  ];

  # config.toml is MERGED, not replaced — the same shape as pi's settings.json
  # (see pi.nix), for the same reason: kimi treats this file as read-write
  # state. `/login` rewrites the oauth block, a `/models` refresh rewrites
  # every [models.*] table, `/secondary-model` writes defaults. Rendering it
  # from the store (home.file) would either fight the agent at every switch or
  # lose silently the first time kimi's atomic write replaces the symlink with
  # a regular file. So at every activation only the keys in kimiOwnedConfig
  # are set, deep-merged over whatever kimi last wrote.
  #
  # The file is TOML and jq cannot read TOML, so the merge round-trips through
  # remarshal: toml2json | jq deep-merge (`*`; owned keys win, everything else
  # survives) | json2toml, written to $DEST.new and renamed into place, per
  # the pi.nix atomicity pattern. Verified against the live file: the
  # round-trip is data-identical (parsed TOML before == after) and the merge
  # is idempotent.
  #
  # default_model is the one owned key that can dangle: it names a
  # [models.*] table, and those only exist once kimi has fetched its
  # catalogue. Pinned over an empty file (fresh box, or a config that got
  # truncated) it makes every launch die with `config.invalid: Model
  # "kimi-code/k3" is not configured in config.toml` — `/login` does not
  # repair it. So the merge drops default_model while the catalogue lacks
  # it; kimi picks its own default, and the next switch after a `/models`
  # refresh pins ours.
  #
  # The marker file under oauth/ is touched only if absent: kimi's file-backed
  # token storage keeps the actual tokens in credentials/<name>.json and uses
  # the oauth/ entry as a create-if-absent marker, so an empty file is the
  # correct content and re-creating it is all a fresh box needs.
  #
  # The credentials file itself is NOT written here — it arrives via the
  # kimiCodeCredentials sops secret (home.nix), shared across the fleet so one
  # `/login` seeds every box. Caveat: access tokens expire and each box
  # refreshes independently, and a switch re-deploys the sops copy over
  # whatever the box refreshed to locally. If the upstream refresh token rolls
  # on use, the last box to refresh wins and the sops copy goes stale —
  # re-seeding means running `/login` on one box and updating the secret from
  # its freshly-written credentials file.
  home.activation.kimiCodeConfig = lib.hm.dag.entryAfter [ "writeBoundary" "sops-nix" ] ''
    DEST="$HOME/.kimi-code/config.toml"
    $DRY_RUN_CMD mkdir -p "$HOME/.kimi-code/oauth"
    [ -f "$DEST" ] || $DRY_RUN_CMD sh -c ': > "'"$DEST"'"'
    $DRY_RUN_CMD ${pkgs.remarshal}/bin/toml2json "$DEST" "$DEST.json"
    $DRY_RUN_CMD ${pkgs.jq}/bin/jq --slurpfile owned "${kimiOwnedConfig}" \
      '. * $owned[0] | if .models[.default_model] then . else del(.default_model) end' \
      "$DEST.json" > "$DEST.merged.json"
    $DRY_RUN_CMD ${pkgs.remarshal}/bin/json2toml "$DEST.merged.json" "$DEST.new"
    $DRY_RUN_CMD mv "$DEST.new" "$DEST"
    $DRY_RUN_CMD rm -f "$DEST.json" "$DEST.merged.json"
    MARKER="$HOME/.kimi-code/oauth/kimi-code-env-0e4f99c69cc27850"
    [ -e "$MARKER" ] || $DRY_RUN_CMD touch "$MARKER"
  '';
}
