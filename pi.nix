{ config, lib, pkgs, ... }:

let
  # Providers and models for pi (`~/.pi/agent/models.json`). Same endpoint and
  # same key as opencode's `modular` provider — api.modular.com is public, so
  # only the key is a secret and it is injected at activation below.
  #
  # pi is NOT opencode's `mcloud`: that one is the internal deployment behind a
  # sops-held hostname and serves a different, much smaller model set. Keeping
  # the name `mcloud` here would invite exactly the confusion it caused once
  # already, so this provider is called `modular`, matching opencode.nix.
  piModels = {
    providers = {
      modular = {
        name = "Modular (api.modular.com)";
        baseUrl = "https://api.modular.com/v1";
        api = "openai-completions";
        # apiKey injected at activation; see the jq filter below.
        models = [
          # `contextWindow` is REQUIRED on every entry. pi defaults it to 128000
          # when absent, and api.modular.com advertises no window in
          # `/v1/models`, so an omitted value is silently wrong rather than
          # unset — a reviewer on a 262k model auto-compacts at 128k and
          # finishes a review it only half read.
          #
          # Every number below is the server's own, from a 400 that named it:
          # a deliberately oversized prompt returns "exceeds the configured
          # maximum context length of N tokens". Re-probe rather than assume if
          # Modular changes a deployment.
          {
            # No `reasoning`: this model has no thinking support, unlike the four
            # below. Omitted rather than set false so the difference is visible.
            id = "google/gemma-4-31b-it";
            name = "Gemma 4 31B (Modular)";
            contextWindow = 262144;
          }
          {
            # 192000, NOT the 1M the model is documented with elsewhere: this
            # deployment is configured lower, and pi packing to the published
            # figure would take 400s mid-review.
            id = "zai-org/glm-5.3";
            reasoning = true;
            name = "GLM 5.3 (Modular)";
            contextWindow = 192000;
            maxTokens = 65536;
          }
          {
            id = "z-ai/glm-5.2";
            reasoning = true;
            name = "GLM 5.2 (Modular)";
            contextWindow = 1048576;
            maxTokens = 65536;
          }
          {
            id = "moonshotai/kimi-k2.7-code";
            reasoning = true;
            name = "Kimi K2.7 Code (Modular)";
            contextWindow = 262144;
            maxTokens = 65536;
          }
          {
            id = "minimax/minimax-m3";
            reasoning = true;
            name = "MiniMax M3 (Modular)";
            contextWindow = 1048576;
          }
        ];
      };
    };
  };

  modelsTemplate = pkgs.writeText "pi-models.json.tmpl" (builtins.toJSON piModels);

  # Rate limits. pi's DEFAULT gives a 429 three session-level retries at 2s, 4s,
  # 8s and then kills the turn — measured, not read off the docs: an always-429
  # stub endpoint got exactly four requests 14s apart end to end, and pi emitted
  # `auto_retry_start delayMs=2000/4000/8000`. Worse, the `Retry-After` the
  # server sends is ignored on that path; a stub answering `Retry-After: 5` was
  # still hit on the same 2/4/8 schedule.
  #
  # There are TWO retry layers and the shape of each decides how the hour gets
  # spent. Both were measured against the same stub:
  #
  #   retry.*           session level, on by default. delay = baseDelayMs *
  #                     2^(n-1), UNCAPPED, and it never reads Retry-After. Cheap
  #                     in requests, but the tail runs away: maxRetries = 11
  #                     reaches an hour only by ending on one 34-minute sleep,
  #                     so a limit that clears in five minutes still costs 34.
  #
  #   retry.provider.*  per-request, and OFF by default (maxRetries = 0) — which
  #                     is why the default retries ignore Retry-After: the layer
  #                     that honors it never runs. delay = min(0.5*2^n, 8)s with
  #                     jitter, so it polls steadily rather than sleeping through
  #                     the recovery, and it prefers Retry-After when present.
  #
  # So put the hour in the CAPPED layer: 9 session attempts (8 retries = 2,4,
  # ...,256s, 8.5 min of backoff, longest single sleep 4.3 min), each carrying up
  # to 55 provider retries at <=8s (~7 min). Worst case ~70 min and ~500
  # requests — roughly 7/min, each rejected at Modular's edge. Turning that dial
  # down is a tradeoff against how long a limit can last and still be ridden out.
  #
  # maxRetryDelayMs bounds what the SERVER is allowed to ask for, not our own
  # backoff: past it pi throws `Server requested Ns retry delay` instead of
  # waiting that long. The 60s default is shorter than a quota window, so a
  # polite upstream saying "come back in 5 minutes" would fail the turn outright.
  piRetry = {
    maxRetries = 8;
    baseDelayMs = 2000;
    provider = {
      maxRetries = 55;
      maxRetryDelayMs = 300000;
    };
  };

  # Handed to jq as a FILE rather than interpolated into the activation script.
  # `--argjson retry '${...}'` reads identically and is safe for the integers
  # above, but it puts Nix-rendered JSON inside shell single quotes: the day this
  # attrset gains a string value containing a quote, the word ends early and the
  # rest runs as commands at activation time. A store path cannot contain one.
  retryTemplate = pkgs.writeText "pi-retry.json" (builtins.toJSON piRetry);
in
{
  # `~/.pi/agent/models.json` is rendered at activation with the Modular key
  # injected, for the reason opencode.json is: not symlinked from the nix store,
  # so the secret never lands in a world-readable store path.
  home.activation.piModelsConfig = lib.hm.dag.entryAfter [ "writeBoundary" "sops-nix" ] ''
    DEST="$HOME/.pi/agent/models.json"
    TMPL="${modelsTemplate}"
    KEY_FILE="$HOME/.config/local-auto-mode/api-key"
    $DRY_RUN_CMD mkdir -p "$HOME/.pi/agent"
    if [ -r "$KEY_FILE" ]; then
      $DRY_RUN_CMD ${pkgs.jq}/bin/jq --rawfile key "$KEY_FILE" \
        '.providers.modular.apiKey = ($key | sub("\n$"; ""))' \
        "$TMPL" > "$DEST.new"
      $DRY_RUN_CMD mv "$DEST.new" "$DEST"
      $DRY_RUN_CMD chmod 0600 "$DEST"
    else
      # No key: still write the providers so `pi --list-models` shows them and
      # says they are unauthenticated, rather than pi seeing no provider at all.
      $DRY_RUN_CMD cp "$TMPL" "$DEST"
      $DRY_RUN_CMD chmod 0644 "$DEST"
    fi
  '';

  # `settings.json` is MERGED, not replaced. pi writes its own runtime keys into
  # this file (`theme`, `lastChangelogVersion`), so rendering it from a template
  # — or `force`-ing a home.file over it — would reset them at every switch and
  # fight the agent for ownership the way drovr's config.toml did. Only the keys
  # this repo has an opinion about are set — the two defaults, and `retry`, whose
  # subtree IS replaced wholesale (see `piRetry` above) so a stale hand-edit of
  # one knob cannot leave the other three at pi's defaults.
  #
  # The default matters beyond convenience: non-interactive launches (drovr's
  # review panel spawns `pi` with no `--model`) inherit whatever this is, and
  # with no default at all pi picks its built-in provider `google`, which has no
  # key here and fails at the first turn.
  home.activation.piSettings = lib.hm.dag.entryAfter [ "piModelsConfig" ] ''
    DEST="$HOME/.pi/agent/settings.json"
    $DRY_RUN_CMD mkdir -p "$HOME/.pi/agent"
    [ -f "$DEST" ] || $DRY_RUN_CMD sh -c 'echo "{}" > "'"$DEST"'"'
    $DRY_RUN_CMD ${pkgs.jq}/bin/jq --slurpfile retry "${retryTemplate}" \
      '.defaultProvider = "modular" | .defaultModel = "google/gemma-4-31b-it" | .retry = $retry[0]' \
      "$DEST" > "$DEST.new"
    $DRY_RUN_CMD mv "$DEST.new" "$DEST"
  '';
}
