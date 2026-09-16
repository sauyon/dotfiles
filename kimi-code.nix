{ lib, ... }:

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
  version = "0.43.1";

  # code.kimi.ai is the global mirror; code.kimi.com is the mainland-CN
  # channel. Same versions, same bytes, but the installer derives a `region`
  # marker from whichever domain it was fetched from, so keep these consistent.
  baseUrl = "https://code.kimi.ai/kimi-code/binaries/${version}";

  platformSpec = system:
    {
      x86_64-linux = {
        target = "linux-x64";
        hash = "sha256-IBeH3kQVBzL8tfhWF+1csgTlr/Dkut9KC90HdSrMf24=";
      };
      aarch64-linux = {
        target = "linux-arm64";
        hash = "sha256-OB0ylqWJYiH5+ebNQpJTBDQjxtFEBi6PsT3G+BwyYx8=";
      };
      x86_64-darwin = {
        target = "darwin-x64";
        hash = "sha256-puE+YpDPbTWrgFzvx3Sz3jtwqKj57HhrKG50luNk87o=";
      };
      aarch64-darwin = {
        target = "darwin-arm64";
        hash = "sha256-/jUdSHLWs1wn8U7F6JWyHR4UwrCdeOoXS0uOzCiJdMY=";
      };
    }.${system} or null;
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
}
