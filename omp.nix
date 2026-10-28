{ config, lib, pkgs, ... }:

let
  # Providers and models for omp (oh-my-pi, https://omp.sh), the interactive
  # coding agent. pi stays installed only as drovr's reviewer; see pi.nix.
  #
  # Modular's own `setup-mcloud.py` is the upstream for this list, with two
  # deliberate differences:
  #   - the provider is `modular`, not `mcloud`, for the reason pi.nix gives:
  #     `mcloud` is Modular's internal deployment, a different endpoint.
  #   - GLM 5.3's window is the served 1048576, not the script's stale 524288.
  #
  # Every `contextWindow` is the server's own, read off the 400 a ~1.3M-token
  # prompt returns ("exceeds the configured maximum context length of N" /
  # "max_seq_len N"), probed 2026-10-21. Re-probe rather than assume.
  #
  # `reasoning` is true on all four, gemma included: measured 2026-10-21, gemma
  # returns a `reasoning_content` thinking block when asked and answers a
  # bat-and-ball prompt wrong when not. It is on/off only — `low` produced more
  # thinking than `high`. `thinkingFormat = "openrouter"` makes omp send
  # `reasoning: {effort}`, which is the shape that toggles it.
  #
  # `input` image flags are taken from setup-mcloud.py unprobed; it notes omp
  # guesses vision wrong and compaction 400s if they are left unset.
  model = id: contextWindow: vision: {
    inherit id contextWindow;
    reasoning = true;
    input = [ "text" ] ++ lib.optional vision "image";
    compat.thinkingFormat = "openrouter";
  };

  ompModels = {
    providers.modular = {
      baseUrl = "https://api.modular.com/v1";
      api = "openai-completions";
      # apiKey injected at activation; see the jq filter below.
      models = [
        (model "zai-org/glm-5.3" 1048576 false)
        (model "minimax/minimax-m3" 1048576 true)
        (model "google/gemma-4-31b-it" 262144 true)
        (model "qwen/qwen3.8-27b" 262144 true)
      ];
    };
  };

  # JSON is valid YAML, so models.yml is rendered with jq like pi's models.json.
  modelsTemplate = pkgs.writeText "omp-models.yml.tmpl" (builtins.toJSON ompModels);
in
{
  home.packages = [ pkgs.omp ];

  # Vendored verbatim from `herdr integration install omp` (v10); inert unless
  # HERDR_ENV=1. Regenerate and bump if `herdr integration status` reports it
  # outdated.
  home.file.".omp/agent/extensions/herdr-omp-agent-state.ts".source =
    ./home/.omp/agent/extensions/herdr-omp-agent-state.ts;

  # Rendered rather than symlinked, for the reason pi.nix gives: the key must
  # not land in a world-readable store path. omp would also accept the NAME of
  # an env var as `apiKey`, but that needs the var in every launch environment;
  # a literal key in a 0600 file works from anywhere.
  home.activation.ompModelsConfig = lib.hm.dag.entryAfter [ "writeBoundary" "sops-nix" ] ''
    DEST="$HOME/.omp/agent/models.yml"
    TMPL="${modelsTemplate}"
    KEY_FILE="$HOME/.config/local-auto-mode/api-key"
    $DRY_RUN_CMD mkdir -p -m 0700 "$HOME/.omp/agent"
    if [ -r "$KEY_FILE" ]; then
      $DRY_RUN_CMD ${pkgs.jq}/bin/jq --rawfile key "$KEY_FILE" \
        '.providers.modular.apiKey = ($key | sub("\n$"; ""))' \
        "$TMPL" > "$DEST.new"
    else
      $DRY_RUN_CMD cp "$TMPL" "$DEST.new"
    fi
    $DRY_RUN_CMD chmod 0600 "$DEST.new"
    $DRY_RUN_CMD mv "$DEST.new" "$DEST"
  '';

  # config.yml is MERGED, not replaced: omp writes its own keys into it (`omp
  # config set`, the first-run wizard). Only the default model is forced.
  # `setupVersion` marks the first-run wizard done — left unset it runs on the
  # first launch and asks for a provider and default model, which can overwrite
  # this. It is only filled in when absent, so a newer omp that bumps it (it is
  # 2 as of 18.11.2) still gets to run its newer setup scenes.
  home.activation.ompSettings = lib.hm.dag.entryAfter [ "ompModelsConfig" ] ''
    DEST="$HOME/.omp/agent/config.yml"
    [ -f "$DEST" ] || $DRY_RUN_CMD sh -c 'echo "{}" > "'"$DEST"'"'
    $DRY_RUN_CMD ${pkgs.yq-go}/bin/yq \
      '.modelRoles.default = "modular/zai-org/glm-5.3" | .setupVersion = (.setupVersion // 2)' \
      "$DEST" > "$DEST.new"
    $DRY_RUN_CMD chmod 0600 "$DEST.new"
    $DRY_RUN_CMD mv "$DEST.new" "$DEST"
  '';
}
