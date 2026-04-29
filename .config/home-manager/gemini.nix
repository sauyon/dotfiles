{ config, pkgs, ... }:

let
  gemini-cli-latest = pkgs.gemini-cli-bin.overrideAttrs (old: rec {
    version = "0.40.0";
    src = pkgs.fetchurl {
      url = "https://registry.npmjs.org/@google/gemini-cli/-/gemini-cli-${version}.tgz";
      hash = "sha256-dWMqrnskl/+INM6A8z456JOMMhCIVacKMWXlVYTAFaQ=";
    };
    # Fix for 0.40.0 layout change where everything moved into a 'bundle' directory
    postInstall = (old.postInstall or "") + ''
      if [ -d $out/lib/gemini/bundle ]; then
        cp -r $out/lib/gemini/bundle/* $out/lib/gemini/
        # Keep the bundle dir as a symlink just in case internal paths expect it
        # Actually, moving is cleaner if relative paths are relative to gemini.js
        # But let's just copy and keep for safety, or better, move.
        # The official nixpkgs build does: cp -r . $out/lib/gemini
        # So in our case it copied package/bundle to $out/lib/gemini/bundle
      fi
    '';
  });
in
{
  home.packages = [ gemini-cli-latest ];

  home.file.".local/bin/gemini-rampart-hook" = {
    executable = true;
    source = ./gemini-rampart-hook.sh;
  };
}
