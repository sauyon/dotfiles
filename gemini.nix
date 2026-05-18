{ config, lib, pkgs, ... }:

let
  gemini-cli-latest = pkgs.gemini-cli-bin.overrideAttrs (old: rec {
    version = "0.40.0";
    src = pkgs.fetchurl {
      url = "https://registry.npmjs.org/@google/gemini-cli/-/gemini-cli-${version}.tgz";
      hash = "sha256-dWMqrnskl/+INM6A8z456JOMMhCIVacKMWXlVYTAFaQ=";
    };
    postInstall = (old.postInstall or "") + ''
      if [ -d $out/lib/gemini/bundle ]; then
        cp -r $out/lib/gemini/bundle/* $out/lib/gemini/
      fi
    '';
  });
in
{
  programs.gemini-cli = {
    enable = true;
    package = gemini-cli-latest;
    settings = {
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

  home.file.".local/bin/gemini-rampart-hook" = lib.mkIf (!pkgs.stdenv.isDarwin) {
    executable = true;
    source = ./gemini-rampart-hook.sh;
  };
}
