{ config, lib, pkgs, ... }:

let
  # Providers and models for omp (oh-my-pi, https://omp.sh), the interactive
  # coding agent, and drovr's review agent ([agents.omp] in home.nix).
  #
  # Modular's own `setup-mcloud.py` is the upstream for this list, with two
  # deliberate differences:
  #   - the provider is `modular`, not `mcloud`: `mcloud` is Modular's
  #     internal deployment behind a sops-held hostname, a different endpoint
  #     serving a much smaller model set, and sharing the name invites
  #     exactly the confusion it caused once already.
  #   - GLM 5.3's window is the served 1048576, not the script's stale 524288.
  #
  # Every `contextWindow` is the server's own, read off the 400 a ~1.3M-token
  # prompt returns ("exceeds the configured maximum context length of N" /
  # "max_seq_len N"), probed 2026-10-21. Re-probe rather than assume.
  #
  # `reasoning` is true on all four, gemma included: measured 2026-10-21, gemma
  # returns a `reasoning_content` thinking block when asked and answers a
  # bat-and-ball prompt wrong when not. `thinkingFormat = "openrouter"` makes
  # omp send `reasoning: {effort}`, which is the shape that toggles it.
  # Effort levels, re-probed 2026-11-23 (reasoning_tokens on a bat-and-ball
  # prompt, 3 runs each): glm-5.3 has three real tiers -- ~8 at low, ~12 at
  # high/medium (medium maps to high), ~419 at max/xhigh (xhigh maps to
  # max); the 2026-10-21 "on/off only" reading was gemma's, and gemma is
  # still level-inert (~318 at every level, 0 with no field). qwen3.8 is
  # inert-to-inverted on this prompt (~213 at low, ~66 at high/max), and
  # minimax-m3 is not routed on api.modular.com at all.
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
      # A leading `!` makes omp run the rest via /bin/sh and use its trimmed
      # stdout as the key (verified on 18.20.1: `!echo bogus` 401s). It runs
      # once per omp process and caches the result for that process's life, so
      # a rotation reaches every omp started after it; running sessions keep
      # the old key until restarted. Baking the key in at activation instead
      # went stale: the step ran "after sops-nix" yet still read the old
      # secret on the first switch after a rotation.
      apiKey = "!${pkgs.coreutils}/bin/cat ${config.home.homeDirectory}/.config/modular/api-key";
      models = [
        (model "zai-org/glm-5.3" 1048576 false)
        (model "minimax/minimax-m3" 1048576 true)
        (model "google/gemma-4-31b-it" 262144 true)
        (model "qwen/qwen3.8-27b" 262144 true)
      ];
    };
  };

  # JSON is valid YAML.
  modelsTemplate = pkgs.writeText "omp-models.yml" (builtins.toJSON ompModels);
in
{
  home.packages = [ pkgs.omp ];

  # Vendored verbatim from `herdr integration install omp` (v10); inert unless
  # HERDR_ENV=1. Regenerate and bump if `herdr integration status` reports it
  # outdated.
  home.file.".omp/agent/extensions/herdr-omp-agent-state.ts".source =
    ./home/.omp/agent/extensions/herdr-omp-agent-state.ts;

  # Copied rather than symlinked so the file stays writable, as before. It holds
  # no secret now -- the key is read at use time (see apiKey above) -- so the
  # step no longer waits on sops-nix.
  home.activation.ompModelsConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    DEST="$HOME/.omp/agent/models.yml"
    $DRY_RUN_CMD mkdir -p -m 0700 "$HOME/.omp/agent"
    $DRY_RUN_CMD install -m 0600 "${modelsTemplate}" "$DEST.new"
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
