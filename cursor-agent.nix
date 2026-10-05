{ lib, ... }:

let
  version = "2026.10.01-e373342";

  platformSpec = system:
    {
      x86_64-linux = {
        os = "linux";
        arch = "x64";
        hash = "sha256-p5cmxuZEUg6ZOXC+TEV3WmiJgCtnq+RhpnelMhmuKOg=";
      };
      aarch64-linux = {
        os = "linux";
        arch = "arm64";
        hash = "sha256-eFxfa/KmDrESHiftjBT17gftG1tmkjJPLZqZcjgkXrU=";
      };
      x86_64-darwin = {
        os = "darwin";
        arch = "x64";
        hash = "sha256-qA224VYm4Kp69g+rOpXJiiQ+LBUTwMlIU0SlU9e3sA4=";
      };
      aarch64-darwin = {
        os = "darwin";
        arch = "arm64";
        hash = "sha256-Yp5R3kOgt/s7hvXrx+V59999+UGznynoKUXN51AUWvw=";
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
