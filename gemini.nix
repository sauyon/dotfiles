{ config, lib, pkgs, ... }:

let
  gemini-cli-latest = pkgs.gemini-cli-bin.overrideAttrs (old: rec {
    version = "0.40.0";
    src = pkgs.fetchurl {
      url = "https://registry.npmjs.org/@google/gemini-cli/-/gemini-cli-${version}.tgz";
      hash = "sha256-dWMqrnskl/+INM6A8z456JOMMhCIVacKMWXlVYTAFaQ=";
    };
    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
    postInstall = (old.postInstall or "") + ''
      if [ -d $out/lib/gemini/bundle ]; then
        cp -r $out/lib/gemini/bundle/* $out/lib/gemini/
      fi
    '' + lib.optionalString (!pkgs.stdenv.isDarwin) ''
      wrapProgram $out/bin/gemini \
        --prefix LD_PRELOAD : "${pkgs.nss_wrapper}/lib/libnss_wrapper.so" \
        --run '
          export NSS_WRAPPER_PASSWD=$(mktemp)
          export NSS_WRAPPER_GROUP=$(mktemp)
          echo "$USER:x:$(id -u):$(id -g):$USER:$HOME:$SHELL" > "$NSS_WRAPPER_PASSWD"
          echo "$USER:x:$(id -g):" > "$NSS_WRAPPER_GROUP"
        '
    '';
  });
in
{
  programs.antigravity-cli = {
    enable = true;
    package = gemini-cli-latest;
    settings = {
      general = {
        enableAutoUpdate = false;
        enableAutoUpdateNotification = false;
      };
      security = {
        auth = {
          selectedType = "oauth-personal";
        };
        enablePermanentToolApproval = true;
        autoAddToPolicyByDefault = true;
        environmentVariableRedaction = {
          enabled = true;
        };
      };
      ui = {
        footer = {
          items = [ "workspace" "git-branch" "sandbox" "model-name" "quota" ];
        };
        showCitations = true;
      };
      model = {
        name = "auto";
      };
      experimental = {
        worktrees = true;
        memoryManager = true;
        contextManagement = true;
        generalistProfile = true;
        autoMemory = true;
        modelSteering = true;
      };
    };
  };

}
