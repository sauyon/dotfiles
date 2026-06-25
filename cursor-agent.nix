{ lib, ... }:

let
  version = "2026.06.24-00-45-58-9f61de7";

  platformSpec = system:
    {
      x86_64-linux = {
        os = "linux";
        arch = "x64";
        hash = "sha256-0lQoOpOeF6hwaWsZzelB95fVbfC0N4msuCLWCFT7D4Y=";
      };
      aarch64-linux = {
        os = "linux";
        arch = "arm64";
        hash = "sha256-trT/kvRYGd7UdMkKLDZfyzQoCxwSpCcP36bztw4mtEw=";
      };
      x86_64-darwin = {
        os = "darwin";
        arch = "x64";
        hash = "sha256-2OgyPNLkhX1eroiaVbadl57umCU1AVw8KK+Q0QmfgRg=";
      };
      aarch64-darwin = {
        os = "darwin";
        arch = "arm64";
        hash = "sha256-Oa2Rwydf78CSYxOHt30S3vlrqOOUSTVjoL7z1EyBR1w=";
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
        cursor-agent-cli = prev.stdenv.mkDerivation {
          pname = "cursor-agent-cli";
          inherit version;

          src =
            if spec == null then
              throw "cursor-agent-cli: unsupported system ${system}"
            else
              prev.fetchurl {
                url = "https://downloads.cursor.com/lab/${version}/${spec.os}/${spec.arch}/agent-cli-package.tar.gz";
                hash = spec.hash;
              };

          nativeBuildInputs = lib.optionals prev.stdenv.hostPlatform.isLinux [
            prev.autoPatchelfHook
            prev.makeWrapper
          ];

          buildInputs = lib.optionals prev.stdenv.hostPlatform.isLinux [
            prev.stdenv.cc.cc.lib
            prev.zlib
          ];

          installPhase = ''
            runHook preInstall

            mkdir -p $out/libexec/cursor-agent $out/bin
            cp -R . $out/libexec/cursor-agent/
            chmod +x $out/libexec/cursor-agent/{cursor-agent,node,crepectl}

            ${lib.optionalString prev.stdenv.hostPlatform.isLinux ''
              patchShebangs $out/libexec/cursor-agent/cursor-agent
            ''}

            ln -s ../libexec/cursor-agent/cursor-agent $out/bin/agent
            ln -s ../libexec/cursor-agent/cursor-agent $out/bin/cursor-agent

            runHook postInstall
          '';

          meta = {
            description = "Cursor Agent CLI for self-hosted Cloud Agent workers";
            homepage = "https://cursor.com/docs/cloud-agent/self-hosted-pool";
            license = lib.licenses.unfree;
            mainProgram = "cursor-agent";
            platforms = lib.platforms.unix;
            sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
          };
        };
      })
  ];
}
