{ config, lib, pkgs, ... }:

let
  opencodeConfig = {
    "$schema" = "https://opencode.ai/config.json";
    plugin = [ "opencode-model-stats" ];
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

  # opencode-model-stats adds `session_id` and `message_id` headers alongside
  # the canonical `x-opencode-*` forms. Modular's edge proxy (Envoy) rejects
  # any request carrying an underscore-named header with a 400 before it
  # reaches the model server. Strip those two lines from the cached plugin
  # source — the x- prefixed headers carry the same data.
  patchModelStats = pkgs.writeShellScript "opencode-patch-model-stats" ''
    set -eu
    shopt -s nullglob
    for f in "$HOME"/.cache/opencode/packages/opencode-model-stats@*/node_modules/opencode-model-stats/src/server.ts; do
      if ${pkgs.gnugrep}/bin/grep -q '\["session_id"\]\|\["message_id"\]' "$f"; then
        ${pkgs.gnused}/bin/sed -i \
          -e '/output\.headers\["session_id"\] *= *ctx\.sessionID/d' \
          -e '/output\.headers\["message_id"\] *= *ctx\.messageID/d' \
          "$f"
      fi
    done
  '';
in
{
  nixpkgs.overlays = [
    (final: prev: {
      opencode = prev.symlinkJoin {
        name = "opencode-${prev.opencode.version or "wrapped"}-mcloudpatch";
        paths = [ prev.opencode ];
        nativeBuildInputs = [ prev.makeWrapper ];
        postBuild = ''
          rm -f $out/bin/opencode
          makeWrapper ${prev.opencode}/bin/opencode $out/bin/opencode \
            --run ${patchModelStats}
        '';
      };
    })
  ];

  # opencode.json is rendered at activation by injecting the sops-decrypted
  # Modular API key into provider.mcloud.options.apiKey. The file is not
  # symlinked into the nix store so the secret never leaks to world-readable
  # /nix/store paths.
  home.activation.opencodeConfig = lib.hm.dag.entryAfter [ "writeBoundary" "sops-nix" ] ''
    DEST="$HOME/.config/opencode/opencode.json"
    TMPL="${configTemplate}"
    MCLOUD_KEY_FILE="$HOME/.config/local-auto-mode/api-key"
    KOAG_KEY_FILE="$HOME/.config/opencode/ko-ag-key"
    $DRY_RUN_CMD mkdir -p "$HOME/.config/opencode"
    JQ_ARGS=()
    JQ_FILTER='.'
    if [ -r "$MCLOUD_KEY_FILE" ]; then
      JQ_ARGS+=(--rawfile mcloudKey "$MCLOUD_KEY_FILE")
      JQ_FILTER="$JQ_FILTER | .provider.mcloud.options.apiKey = (\$mcloudKey | sub(\"\\n$\"; \"\"))"
    fi
    if [ -r "$KOAG_KEY_FILE" ]; then
      JQ_ARGS+=(--rawfile koagKey "$KOAG_KEY_FILE")
      JQ_FILTER="$JQ_FILTER | .provider.\"ko-ag\".options.apiKey = (\$koagKey | sub(\"\\n$\"; \"\"))"
    fi
    if [ ''${#JQ_ARGS[@]} -gt 0 ]; then
      $DRY_RUN_CMD ${pkgs.jq}/bin/jq "''${JQ_ARGS[@]}" "$JQ_FILTER" "$TMPL" > "$DEST.new"
      $DRY_RUN_CMD mv "$DEST.new" "$DEST"
      $DRY_RUN_CMD chmod 0600 "$DEST"
    else
      $DRY_RUN_CMD cp "$TMPL" "$DEST"
      $DRY_RUN_CMD chmod 0644 "$DEST"
    fi
  '';
}
