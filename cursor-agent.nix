{ lib, ... }:

let
  version = "2026.10.22-27dcf5f";

  platformSpec = system:
    {
      x86_64-linux = {
        os = "linux";
        arch = "x64";
        hash = "sha256-hSlpRORgCoPEt2T1eNFQK/nEFtsgDTOlk+jz7Kv9wpQ=";
      };
      aarch64-linux = {
        os = "linux";
        arch = "arm64";
        hash = "sha256-IqNt+XIIMGaMzU1wflGQIEq5yKDvsvBozJFIlfgE0+w=";
      };
      x86_64-darwin = {
        os = "darwin";
        arch = "x64";
        hash = "sha256-ejz78Pz7l5tiE/61+f0Q+8fNTlGrKMO1cruTXgsRSWE=";
      };
      aarch64-darwin = {
        os = "darwin";
        arch = "arm64";
        hash = "sha256-L1sKPdX7lRzs/6yBOGfYoI2BDlmDfVn9XxLWm5IjmNc=";
      };
    }.${system} or null;
in
{
  nixpkgs.overlays = [
    (final: prev:
      let
        inherit (prev.stdenvNoCC.hostPlatform) system;
        spec = platformSpec system;

        # patchelf >= 0.18 cannot grow this tree's ELF headers without
        # corrupting them. Given `node` (and both SEA executables), 0.19.2 —
        # stdenv's patchelf, i.e. the one autoPatchelfHook used to run here —
        # inserts the grown header LOAD at 0x3ff000 with a filesz reaching
        # 0xeffb88, straight across the original R LOAD at 0x400000. The R
        # mapping wins the overlap, the dynamic section comes up read-only,
        # and ld.so dies writing DT_DEBUG at startup: SIGSEGV/SEGV_ACCERR in
        # dl_main, even on `node --version`. 0.15.2 grows the headers onto a
        # page of their own instead. Same pin and bug class as kimi-code.nix;
        # drop it once a released patchelf produces a node that survives
        # `--version`.
        patchelf_0_15 = prev.patchelf.overrideAttrs (finalAttrs: _: {
          version = "0.15.2";
          src = prev.fetchurl {
            url = "https://github.com/NixOS/patchelf/releases/download/${finalAttrs.version}/patchelf-${finalAttrs.version}.tar.bz2";
            hash = "sha256-F3RfVkFZyOIo/EEtplogSLhGxLa0Igt3y/IkFuAvLXw=";
          };
        });

        # The nix glibc loader satisfies libc/libm/libdl/libpthread/libutil
        # from its own store dir, so the rpath only needs the C++ runtime
        # (V8 wants libstdc++, libgcc_s) and zlib (file_service).
        elfRpath = lib.makeLibraryPath [
          prev.stdenv.cc.cc.lib
          prev.stdenv.cc.cc.libgcc
          prev.zlib
        ];
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

          # No autoPatchelfHook: it runs stdenv's patchelf, which is the
          # version that corrupts node and the SEAs (see patchelf_0_15
          # above). Its two edits are made by hand in installPhase instead;
          # the fully static helpers (rg, crepectl, cursorsandbox) need
          # neither.
          nativeBuildInputs = lib.optionals prev.stdenv.hostPlatform.isLinux [
            patchelf_0_15
          ];

          # REQUIRED. The SEA payloads are injected post-link (postject) as
          # sections the program headers do not cover, so `strip` cannot map
          # them back into segments — "allocated section `.dynstr' not in
          # segment" — and rewrites executables that die with a symbol lookup
          # error on every launch. autoPatchelfHook suppressed the strip phase
          # implicitly; without it the pin is explicit (kimi-code.nix carries
          # the same one for the same reason).
          dontStrip = true;

          installPhase = ''
            runHook preInstall

            mkdir -p $out/libexec/cursor-agent $out/bin
            cp -R . $out/libexec/cursor-agent/
            chmod +x $out/libexec/cursor-agent/{cursor-agent,node,crepectl}

            ${lib.optionalString prev.stdenv.hostPlatform.isLinux ''
              patchShebangs $out/libexec/cursor-agent/cursor-agent

              # Executables: interpreter + rpath.
              for bin in node cursor-agent-sea cursor-agent-worker-sea; do
                ${patchelf_0_15}/bin/patchelf \
                  --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
                  --set-rpath ${elfRpath} \
                  $out/libexec/cursor-agent/$bin
              done

              # Shared objects: rpath only. The --print-rpath guard skips the
              # win32/darwin prebuilds and other non-ELF payloads.
              for so in $(find $out/libexec/cursor-agent -name '*.node' -type f); do
                if ${patchelf_0_15}/bin/patchelf --print-rpath "$so" >/dev/null 2>&1; then
                  ${patchelf_0_15}/bin/patchelf --set-rpath ${elfRpath} "$so"
                fi
              done
            ''}

            ln -s ../libexec/cursor-agent/cursor-agent $out/bin/agent
            ln -s ../libexec/cursor-agent/cursor-agent $out/bin/cursor-agent

            runHook postInstall
          '';

          # The 2026.10.01 build shipped clean and segfaulted on every launch,
          # so prove it runs before it reaches a profile. The build sandbox
          # has no ambient LD_LIBRARY_PATH, so the module loads exercise the
          # rpaths and nothing else.
          doInstallCheck = true;
          installCheckPhase = ''
            runHook preInstallCheck

            $out/libexec/cursor-agent/node --version
            $out/bin/cursor-agent --version

            ${lib.optionalString prev.stdenv.hostPlatform.isLinux ''
              $out/libexec/cursor-agent/cursor-agent-sea --version
              $out/libexec/cursor-agent/cursor-agent-worker-sea --version

              for so in $out/libexec/cursor-agent/*.node; do
                if ${patchelf_0_15}/bin/patchelf --print-rpath "$so" >/dev/null 2>&1; then
                  case "$so" in
                    # Node's own test fixture: dlopens fine but never
                    # self-registers, so require() rejects it.
                    *test_extension.node) ;;
                    *) $out/libexec/cursor-agent/node -e 'require(process.argv[1])' "$so" ;;
                  esac
                fi
              done
            ''}

            runHook postInstallCheck
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
