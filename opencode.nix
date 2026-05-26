{ config, lib, pkgs, ... }:

let
  opencodeConfig = {
    "$schema" = "https://opencode.ai/config.json";
    provider = {
      mcloud = {
        npm = "@ai-sdk/openai-compatible";
        name = "Modular MAX Cloud";
        options = {
          baseURL = "https://model.api.modular.com/v1";
        };
        models = {
          "google/gemma-4-31B-it" = { name = "Gemma 4 31B"; };
          "google/gemma-4-26B-A4B-it" = { name = "Gemma 4 26B A4B"; };
          "nvidia/Gemma-4-31B-IT-NVFP4" = { name = "Gemma 4 31B (NVFP4)"; };
          "nvidia/Gemma-4-26B-A4B-NVFP4" = { name = "Gemma 4 26B A4B (NVFP4)"; };
          "nvidia/Kimi-K2.5-NVFP4" = { name = "Kimi K2.5 (NVFP4)"; };
        };
      };
      "ko-ag" = {
        npm = "@ai-sdk/openai-compatible";
        name = "ko.ag";
        options = {
          baseURL = "https://ai.ko.ag/v1";
        };
        models = {
          "gemma4:31b" = { name = "Gemma 4 31B"; };
        };
      };
    };
  };
  configTemplate = pkgs.writeText "opencode.json.tmpl" (builtins.toJSON opencodeConfig);
in
{
  # opencode.json is rendered at activation by injecting the sops-decrypted
  # Modular API key into provider.mcloud.options.apiKey. The file is not
  # symlinked into the nix store so the secret never leaks to world-readable
  # /nix/store paths.
  home.activation.opencodeConfig = lib.hm.dag.entryAfter [ "writeBoundary" "sops-nix" ] ''
    DEST="$HOME/.config/opencode/opencode.json"
    TMPL="${configTemplate}"
    KEY_FILE="$HOME/.config/local-auto-mode/api-key"
    $DRY_RUN_CMD mkdir -p "$HOME/.config/opencode"
    if [ -r "$KEY_FILE" ]; then
      $DRY_RUN_CMD ${pkgs.jq}/bin/jq \
        --rawfile key "$KEY_FILE" \
        '.provider.mcloud.options.apiKey = ($key | sub("\n$"; ""))' \
        "$TMPL" > "$DEST.new"
      $DRY_RUN_CMD mv "$DEST.new" "$DEST"
      $DRY_RUN_CMD chmod 0600 "$DEST"
    else
      $DRY_RUN_CMD cp "$TMPL" "$DEST"
      $DRY_RUN_CMD chmod 0644 "$DEST"
    fi
  '';
}
