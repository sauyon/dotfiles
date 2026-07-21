{ config, lib, pkgs, ... }:

let
  opencodeConfig = {
    "$schema" = "https://opencode.ai/config.json";
    plugin = [ "opencode-model-stats@0.2.3" ];
    disabled_providers = [ "opencode" "zai" ];
    provider = {
      mcloud = {
        npm = "@ai-sdk/openai-compatible";
        name = "Modular (internal)";
        # baseURL is injected at activation from the sops secret
        # ~/.config/opencode/mcloud-base-url so the private internal hostname
        # never appears in the committed config.
        options = { };
        models = {
          "MiniMaxAI/MiniMax-M3" = { name = "MiniMax M3"; };
        };
      };
      "ko-ag" = {
        npm = "@ai-sdk/openai-compatible";
        name = "ko.ag";
        options = {
          baseURL = "https://ai.ko.ag/v1";
        };
        models = {
          "Huihui-Qwen3.6-35B-A3B-abliterated-Q4_K" = { name = "Qwen3.6 35B A3B (abliterated)"; };
          "gemma-4-26B-A4B-it" = { name = "Gemma 4 26B A4B"; };
        };
      };
      # Built-in models.dev providers; apiKey injected at activation from sops.
      "zai-coding-plan" = {
        npm = "@ai-sdk/openai-compatible";
        name = "Z.AI Coding Plan";
        options = {
          baseURL = "https://api.z.ai/api/coding/paas/v4";
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
    MCLOUD_URL_FILE="$HOME/.config/opencode/mcloud-base-url"
    KOAG_KEY_FILE="$HOME/.config/opencode/ko-ag-key"
    ZAI_KEY_FILE="$HOME/.config/opencode/zai-key"
    $DRY_RUN_CMD mkdir -p "$HOME/.config/opencode"
    JQ_ARGS=()
    JQ_FILTER='.'
    if [ -r "$MCLOUD_KEY_FILE" ]; then
      JQ_ARGS+=(--rawfile mcloudKey "$MCLOUD_KEY_FILE")
      JQ_FILTER="$JQ_FILTER | .provider.mcloud.options.apiKey = (\$mcloudKey | sub(\"\\n$\"; \"\"))"
    fi
    if [ -r "$MCLOUD_URL_FILE" ]; then
      JQ_ARGS+=(--rawfile mcloudUrl "$MCLOUD_URL_FILE")
      JQ_FILTER="$JQ_FILTER | .provider.mcloud.options.baseURL = (\$mcloudUrl | sub(\"\\n$\"; \"\"))"
    fi
    if [ -r "$KOAG_KEY_FILE" ]; then
      JQ_ARGS+=(--rawfile koagKey "$KOAG_KEY_FILE")
      JQ_FILTER="$JQ_FILTER | .provider.\"ko-ag\".options.apiKey = (\$koagKey | sub(\"\\n$\"; \"\"))"
    fi
    if [ -e "$ZAI_KEY_FILE" ]; then
      JQ_ARGS+=(--rawfile zaiKey "$ZAI_KEY_FILE")
      JQ_FILTER="$JQ_FILTER | .provider.\"zai-coding-plan\".options.apiKey = (\$zaiKey | sub(\"\\n$\"; \"\"))"
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

  # C-g is the Emacs universal cancel key; map it to session_interrupt and
  # unbind it from messages_first (which used "ctrl+g,home"). C-f/C-b mirror
  # forward-char/backward-char in confirmation dialogs (select next/prev).
  xdg.configFile."opencode/tui.json".text = builtins.toJSON {
    "$schema" = "https://opencode.ai/tui.json";
    keybinds = {
      session_interrupt = "escape,ctrl+g";
      messages_first = "home";
      "dialog.select.next" = "down,ctrl+n,ctrl+f";
      "dialog.select.prev" = "up,ctrl+p,ctrl+b";
    };
  };

  # herdr integration: file-based opencode plugin that reports lifecycle state
  # and session identity to the local herdr socket while opencode runs inside a
  # herdr pane. Auto-loaded from ~/.config/opencode/plugins/ (no entry needed in
  # opencode.json's `plugin` array, which is for npm plugins). No-op unless
  # HERDR_ENV=1. Vendored verbatim from `herdr integration install opencode`
  # (v8); regenerate if `herdr integration status` reports it outdated.
  xdg.configFile."opencode/plugins/herdr-agent-state.js".source =
    ./home/opencode/plugins/herdr-agent-state.js;

  # Belt-and-suspenders: 0.2.4 reintroduced legacy session_id/message_id headers
  # that Modular's Envoy rejects with 400. Patch any cached copy after activation.
  home.activation.patchOpencodeModelStats = lib.hm.dag.entryAfter [ "opencodeConfig" ] ''
    $DRY_RUN_CMD ${patchModelStats}
  '';
}
