{
  config,
  lib,
  pkgs,
  sops-nix,
  walker,
  nixgl,
  agent-orchestrator,
  ao-mcp,


  system,
  ...
}:

let
  isDarwin = pkgs.stdenv.isDarwin;
  machine = import ./machine.nix;
  hostname = machine.hostname;
  isDesktop = machine.gui or true;
  gpu = machine.gpu or null;

  btopPkg =
    if gpu == "amd" then pkgs.btop-rocm
    else if gpu == "nvidia" then pkgs.btop-cuda
    else pkgs.btop;

  hidpi = let
    scale = if hostname == "setsuna" || hostname == "fujiwara" then 1.25 else 1.0;
    enabled = scale != 1.0;
  in {
    inherit scale enabled;
    qtFontDpi = builtins.floor (96.0 * scale);
    cursorSize = if enabled then 48 else 24;
    waybarFontSize = if enabled then 20 else 17;
    waybarBarHeight = if enabled then 48 else 42;
    ghosttyFontSize = 14;
  };

  edgeGap = if hostname == "fujiwara" then 20 else 0;

  nixGL =
    if isDarwin || !isDesktop then
      null
    else
      pkgs.writeShellScriptBin "nixGL" ''
        exec ${nixgl.packages.${system}.nixGLIntel}/bin/nixGLIntel "$@"
      '';
  agent-orchestrator-pkg = if isDarwin then null else agent-orchestrator.packages.${system}.default;
  ao-mcp-pkg = if isDarwin then null else ao-mcp.packages.${system}.default;

  caffeine = pkgs.writeShellScriptBin "caffeine" ''
    set -eu
    PIDFILE="''${XDG_RUNTIME_DIR:-/tmp}/caffeine.pid"
    is_on() { [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; }
    case "''${1:-toggle}" in
      toggle)
        if is_on; then
          kill "$(cat "$PIDFILE")" 2>/dev/null || true
          rm -f "$PIDFILE"
        else
          systemd-inhibit --what=idle --who=caffeine --why="user toggle" \
            sleep infinity & disown
          echo $! > "$PIDFILE"
        fi
        pkill -RTMIN+10 waybar 2>/dev/null || true
        ;;
      waybar)
        if is_on; then
          echo '{"text":"","class":"on","tooltip":"Idle inhibited (caffeine on)"}'
        else
          echo '{"text":"󰒲","class":"off","tooltip":"Idle enabled"}'
        fi
        ;;
    esac
  '';

  hypr-fullscreen-inhibit = pkgs.writeShellScriptBin "hypr-fullscreen-inhibit" ''
    set -u
    PIDFILE="''${XDG_RUNTIME_DIR:-/tmp}/hypr-fullscreen-inhibit.pid"

    is_on()    { [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; }
    has_full() { ${pkgs.hyprland}/bin/hyprctl clients -j | ${pkgs.jq}/bin/jq -e 'any(.fullscreen != 0)' >/dev/null; }

    start_lock() {
      is_on && return
      systemd-inhibit --what=idle --who=hypr-fullscreen \
        --why="fullscreen window" sleep infinity & disown
      echo $! > "$PIDFILE"
    }
    stop_lock() {
      is_on || { rm -f "$PIDFILE"; return; }
      kill "$(cat "$PIDFILE")" 2>/dev/null || true
      rm -f "$PIDFILE"
    }
    sync() { if has_full; then start_lock; else stop_lock; fi; }

    trap 'stop_lock; exit 0' INT TERM EXIT

    sync
    SOCK="''${XDG_RUNTIME_DIR}/hypr/''${HYPRLAND_INSTANCE_SIGNATURE}/.socket2.sock"
    ${pkgs.socat}/bin/socat -u "UNIX-CONNECT:$SOCK" - | while IFS= read -r ev; do
      case "$ev" in
        fullscreen*|closewindow*|openwindow*|workspace*) sync ;;
      esac
    done
  '';

  ao-run = pkgs.writeShellScriptBin "ao-run" ''
    docker network inspect agent-net >/dev/null 2>&1 || docker network create agent-net

    # Build/rebuild the ao image (dereference nix symlinks for Docker build context)
    AO_BUILD=$(mktemp -d)
    cp -rL ~/.config/agent-orchestrator/* "$AO_BUILD/"
    docker build -t ao \
      --build-arg HOST_UID="$(id -u)" \
      --build-arg HOST_GID="$(id -g)" \
      --build-arg HOST_USER="$(id -un)" \
      "$AO_BUILD"
    rm -rf "$AO_BUILD"

    # Build the hermes plugin (source is on host, node_modules resolved in container)
    PLUGIN_DIR=~/devel/agent-orchestrator/packages/plugins/agent-hermes
    CORE_DIR=~/devel/agent-orchestrator/packages/core
    if [ -d "$PLUGIN_DIR/src" ] && [ ! -f "$PLUGIN_DIR/dist/index.js" ]; then
      docker run --rm -v ~/devel:/repos -w /repos/agent-orchestrator \
        node:24-bookworm sh -c 'npm install -g pnpm && pnpm install --no-frozen-lockfile && pnpm --filter @aoagents/ao-core build && pnpm --filter @aoagents/ao-plugin-agent-hermes build'
    fi

    # Prune stale git worktrees left by previous container runs (happens on unclean exit)
    for REPO in ~/devel/mcloud ~/devel/mammoth ~/devel/modular; do
      if [ -d "$REPO/.git" ]; then
        docker run --rm -v ~/devel:/repos "node:24-bookworm" bash -c \
          "git config --global --add safe.directory /repos/$(basename $REPO) && cd /repos/$(basename $REPO) && git worktree prune" 2>/dev/null || true
      fi
    done

    AO_CONFIG="/tmp/ao-config.yaml"
    cp -f ~/.config/agent-orchestrator/config.yaml "$AO_CONFIG"
    chmod 644 "$AO_CONFIG"
    TTY_FLAG=""
    [ -t 0 ] && TTY_FLAG="-t"

    # Locate hermes binary in the nix store so it's accessible inside the container.
    # The hermes plugin searches /root/.local/bin/hermes and /root/.hermes/hermes-agent/venv/bin/hermes.
    # We mount the nix store read-only and bind the binary to the first search path.
    HERMES_BIN=$(find /nix/store -maxdepth 3 -name 'hermes' -path '*hermes-agent*/bin/hermes' 2>/dev/null | sort | tail -1)
    HERMES_MOUNT=""
    if [ -n "$HERMES_BIN" ]; then
      HERMES_MOUNT="-v /nix/store:/nix/store:ro -v ''${HERMES_BIN}:/home/$(id -un)/.local/bin/hermes:ro"
    fi

    CONTAINER_HOME="/home/$(id -un)"
    exec docker run --rm -i $TTY_FLAG \
      --name ao \
      --network agent-net \
      -v "$AO_CONFIG":/work/agent-orchestrator.yaml \
      -e AO_CONFIG_PATH=/work/agent-orchestrator.yaml \

      -v ~/devel:/repos \
      -v ~/.config/gh:"$CONTAINER_HOME"/.config/gh:ro \
      -v ~/.aws:"$CONTAINER_HOME"/.aws:ro \
      -v ~/.hermes-orchestrator:"$CONTAINER_HOME"/.hermes \
      $HERMES_MOUNT \
      -e HERMES_GATEWAY_TOKEN="$(cat ~/.hermes-gateway-token 2>/dev/null)" \
      -e ANTHROPIC_BASE_URL=http://litellm:4000 \
      -e AO_ANTHROPIC_KEY=sk-ao \
      -e OPENAI_API_BASE=https://deepseek.api.modular.com/v1 \
      -e OPENAI_API_KEY="$(cat ~/.config/hermes/secrets/api-key 2>/dev/null || echo REPLACE_ME)" \
      -e AIDER_MODEL=openai/moonshotai/kimi-k2.5 \
      -p 3000:3000 \
      -p 14801:14801 \
      ao start ''${@:-mcloud}
  '';

  claude-prof = pkgs.writeShellScriptBin "claude-prof" ''
    set -euo pipefail
    CONFIG_HOME="''${XDG_CONFIG_HOME:-$HOME/.config}"

    cmd="''${1:-help}"
    shift || true

    profile_dir() { echo "$CONFIG_HOME/claude-$1"; }

    case "$cmd" in
      list|ls)
        found=0
        for d in "$CONFIG_HOME"/claude-*/; do
          [ -d "$d" ] || continue
          basename "$d" | sed 's/^claude-//'
          found=1
        done
        [ "$found" = 1 ] || echo "No profiles."
        ;;
      rm|delete)
        name="''${1:?usage: claude-prof rm <name>}"
        dir="$(profile_dir "$name")"
        [ -d "$dir" ] || { echo "error: profile '$name' not found"; exit 1; }
        rm -rf "$dir"
        echo "Deleted profile: $name"
        ;;
      run)
        name="''${1:?usage: claude-prof run <name> [claude-args...]}"
        shift
        dir="$(profile_dir "$name")"
        mkdir -p "$dir"

        # Share settings/memory/instructions across profiles; only credentials
        # are per-profile. Log in once per profile via /login on first run.
        for f in settings.json settings.local.json CLAUDE.md; do
          real="$HOME/.claude/$f"
          link="$dir/$f"
          [ -e "$real" ] || continue
          [ -e "$link" ] || ln -sf "$real" "$link"
        done
        for d in projects; do
          real="$HOME/.claude/$d"
          link="$dir/$d"
          [ -e "$real" ] || continue
          [ -e "$link" ] || ln -sf "$real" "$link"
        done

        exec env CLAUDE_CONFIG_DIR="$dir" claude "$@"
        ;;
      help|--help|-h)
        echo "Usage: claude-prof <command> [args]"
        echo ""
        echo "Commands:"
        echo "  list               list profiles"
        echo "  run <name> [args]  run claude with the named profile"
        echo "  rm <name>          delete a profile"
        ;;
      *)
        echo "error: unknown command '$cmd'. Try 'claude-prof help'." >&2
        exit 1
        ;;
    esac
  '';

  args = { inherit config lib pkgs; };

  # Claude Code settings kept as a nix value so they can be merged into the
  # mutable ~/.claude/settings.json on each activation (see home.activation below).
  claudeSettings = {
    hooks = {
      PreToolUse = [
        {
          matcher = ".*";
          hooks = [ {
            type = "command";
            command = "python3 ${config.home.homeDirectory}/.claude/plugins/local-auto-mode/classifier.py";
            timeout = 15;
          } ];
        }
        {
          matcher = "Bash";
          hooks = [ {
            type = "command";
            # Block `gh pr create` outside the quite-app worktree. The hook
            # JSON arrives on stdin; we must inspect tool_input.command
            # ourselves because `matcher` only filters on tool name.
            command = ''
              case "$PWD" in ${config.home.homeDirectory}/devel/quite-app*) exit 0 ;; esac
              input=$(cat)
              case "$input" in *'"command":"gh pr create'*) ;; *) exit 0 ;; esac
              printf '%s' '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"Do not run `gh pr create`. Print a PR creation link instead (e.g. https://github.com/<owner>/<repo>/compare/<base>...<head>?expand=1, or https://github.com/<owner>/<repo>/pull/new/<branch>) and let the user create the PR themselves."}}'
            '';
          } ];
        }
      ];
      PostToolUseFailure = [];
    };
    permissions = {
      allow = [
        "Bash(mise run:*)"
        "Bash(home-manager switch)"
        "mcp__claude_ai_Slack__slack_read_channel"
        "mcp__claude_ai_Slack__slack_read_thread"
        "mcp__claude_ai_Slack__slack_read_canvas"
        "mcp__claude_ai_Slack__slack_read_user_profile"
        "mcp__claude_ai_Notion__notion-fetch"
        "mcp__claude_ai_Notion__notion-get-comments"
        "mcp__claude_ai_Notion__notion-search"
        "mcp__claude_ai_Notion__notion-query-data-sources"
        "mcp__claude_ai_Notion__notion-query-meeting-notes"
        "mcp__claude_ai_Notion__notion-get-teams"
        "mcp__claude_ai_Notion__notion-get-users"
        "mcp__claude_ai_Linear__get_issue"
        "mcp__claude_ai_Linear__get_project"
        "mcp__claude_ai_Linear__get_team"
        "mcp__claude_ai_Linear__get_user"
        "mcp__claude_ai_Linear__list_issues"
        "mcp__claude_ai_Linear__list_projects"
        "mcp__claude_ai_Linear__list_teams"
        "mcp__claude_ai_Linear__list_users"
        "mcp__claude_ai_Linear__list_comments"
        "mcp__claude_ai_Linear__get_document"
        "mcp__claude_ai_Linear__list_documents"
        "mcp__claude_ai_Linear__get_initiative"
        "mcp__claude_ai_Linear__list_initiatives"
        "mcp__claude_ai_Linear__get_milestone"
        "mcp__claude_ai_Linear__list_milestones"
        "mcp__claude_ai_Linear__get_status_updates"
        "mcp__claude_ai_Linear__list_cycles"
        "mcp__claude_ai_Linear__list_issue_labels"
        "mcp__claude_ai_Linear__list_issue_statuses"
        "mcp__claude_ai_Linear__list_project_labels"
        "mcp__claude_ai_Linear__get_authenticated_user"
        "mcp__claude_ai_Linear__get_attachment"
        "mcp__claude_ai_Linear__get_issue_status"
        "mcp__claude_ai_Linear__search_documentation"
        "mcp__github__get_commit"
        "mcp__github__get_copilot_job_status"
        "mcp__github__get_file_contents"
        "mcp__github__get_label"
        "mcp__github__get_latest_release"
        "mcp__github__get_me"
        "mcp__github__get_release_by_tag"
        "mcp__github__get_tag"
        "mcp__github__get_team_members"
        "mcp__github__get_teams"
        "mcp__github__issue_read"
        "mcp__github__list_branches"
        "mcp__github__list_commits"
        "mcp__github__list_issue_types"
        "mcp__github__list_issues"
        "mcp__github__list_pull_requests"
        "mcp__github__list_releases"
        "mcp__github__list_tags"
        "mcp__github__pull_request_read"
        "mcp__github__search_code"
        "mcp__github__search_issues"
        "mcp__github__search_pull_requests"
        "mcp__github__search_repositories"
        "mcp__github__search_users"
        "Skill(evaluate)"
      ];
      deny = [
        "mcp__github__create_pull_request"
      ];
    };
    enabledPlugins = {
      "rust-analyzer-lsp@claude-plugins-official" = true;
    };
    mcpServers = {
      unifi = {
        type = "stdio";
        command = "sh";
        args = [
          "-c"
          "UNIFI_API_KEY=$(cat ${config.home.homeDirectory}/.config/unifi/api-key) exec ${config.home.homeDirectory}/.local/share/mise/shims/uvx unifi-mcp-server"
        ];
        env = {
          UNIFI_API_TYPE = "local";
          UNIFI_LOCAL_HOST = "10.0.0.1";
          UNIFI_LOCAL_VERIFY_SSL = "false";
        };
      };
    };
    autoDreamEnabled = true;
    skipDangerousModePermissionPrompt = true;
    skipAutoPermissionPrompt = true;
  };
  claudeSettingsFile = pkgs.writeText "claude-settings-nix.json" (builtins.toJSON claudeSettings);

  newtabLinks = [
    { group = "Work"; links = [
      { name = "Gmail";       url = "https://mail.google.com"; }
      { name = "Google Docs"; url = "https://docs.google.com"; }
      { name = "GitHub";      url = "https://github.com"; }
      { name = "Notion";          url = "https://www.notion.so"; }
      { name = "Rippling";        url = "https://app.rippling.com"; }
      { name = "Cloud (prod)";    url = "https://console.modular.com"; }
      { name = "Cloud (staging)"; url = "https://mcloud-staging.bentoml.ai"; }
    ];}
    { group = "Infra"; links = [
      { name = "Okta";       url = "https://modular.okta.com"; }
      { name = "AWS";        url = "https://d-906789f3a0.awsapps.com"; }
      { name = "Datadog";    url = "https://app.datadoghq.com"; }
      { name = "ArgoCD";     url = "https://argocd.prod.modular-internal.com"; }
      { name = "BentoML ArgoCD"; url = "https://argocd.tail1beac.ts.net"; }
      { name = "Tailscale";  url = "https://login.tailscale.com"; }
      { name = "Cloudflare"; url = "https://dash.cloudflare.com"; }
      { name = "OpenShift";  url = "https://console.redhat.com"; }
    ];}
    { group = "Other"; links = [
      { name = "Reddit";       url = "https://www.reddit.com"; }
      { name = "YouTube";      url = "https://www.youtube.com"; }
      { name = "YT Music";     url = "https://music.youtube.com"; }
      { name = "Claude";       url = "https://claude.ai"; }
      { name = "Amazon";       url = "https://www.amazon.com"; }
      { name = "Zillow";       url = "https://www.zillow.com"; }
    ];}
  ];

  renderLink = l: ''<a href="${l.url}">${l.name}</a>'';
  renderGroup = g: ''
    <div class="group">
      <h2>${g.group}</h2>
      <div class="links">${lib.concatMapStrings renderLink g.links}</div>
    </div>'';

  newtabHtml = ''
    <!DOCTYPE html>
    <html lang="en">
    <head>
    <meta charset="utf-8">
    <title>New Tab</title>
    <style>
      * { margin: 0; padding: 0; box-sizing: border-box; }
      body {
        background: #1a1a1a;
        color: #e0e0e0;
        font-family: system-ui, -apple-system, sans-serif;
        display: flex;
        justify-content: center;
        padding-top: 15vh;
      }
      .container { max-width: 60vw; width: 100%; }
      h2 {
        font-size: 1.2vh;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #888;
        margin-bottom: 0.8vh;
        text-align: center;
      }
      .group { margin-bottom: 2.5vh; }
      .links { display: flex; flex-wrap: wrap; gap: 0.6vh; justify-content: center; }
      a {
        color: #c0c0c0;
        text-decoration: none;
        font-size: 1.6vh;
        padding: 0.6vh 1.2vh;
        border-radius: 0.5vh;
        background: #252525;
        transition: background 0.1s, color 0.1s;
      }
      a:hover { background: #333; color: #fff; }
    </style>
    </head>
    <body>
    <div class="container">
    ${lib.concatMapStrings renderGroup newtabLinks}
    </div>
    </body>
    </html>
  '';
in
{
  imports = [ sops-nix.homeManagerModules.sops walker.homeManagerModules.default ./gemini.nix ./opencode.nix ];

  home.stateVersion = "26.05";

  # ── sops-nix ────────────────────────────────────────────────────────────────
  sops.defaultSopsFile = ./secrets.yaml;
  # Decryption is GCP KMS (see .sops.yaml + GOOGLE_APPLICATION_CREDENTIALS below).
  # sops-nix asserts that *some* age/gpg key source be configured even when KMS
  # does all the work, and sops-install-secrets opens the configured keyFile at
  # runtime — so we declare an empty managed file just to satisfy both.
  home.file.".config/sops/age-unused.txt".text = "";
  sops.age.keyFile = "${config.home.homeDirectory}/.config/sops/age-unused.txt";
  sops.age.sshKeyPaths = [];
  sops.gnupg.sshKeyPaths = [];
  sops.environment.GOOGLE_APPLICATION_CREDENTIALS = "${config.home.homeDirectory}/.config/sops/gcp-key.json";

  # ── Modular API (local auto-mode classifier) ────────────────────────────────
  sops.secrets.modularApiKey = {
    path = "${config.home.homeDirectory}/.config/local-auto-mode/api-key";
    mode = "0600";
  };

  # ── ko.ag API (opencode provider) ──────────────────────────────────────────
  sops.secrets.koAgApiKey = {
    path = "${config.home.homeDirectory}/.config/opencode/ko-ag-key";
    mode = "0600";
  };

  # ── UniFi API key (unifi-mcp-server) ───────────────────────────────────────
  sops.secrets.unifiApiKey = {
    path = "${config.home.homeDirectory}/.config/unifi/api-key";
    mode = "0600";
  };

  # ── Global Claude preferences (loaded into every conversation) ────────────
  home.file.".claude/CLAUDE.md".source = ./home/.claude/CLAUDE.md;

  # ── Claude skills ──────────────────────────────────────────────────────────
  home.file.".claude/skills/linear-flow/SKILL.md".source =
    ./home/.claude/skills/linear-flow/SKILL.md;
  home.file.".claude/skills/linear-flow/DESIGN.md".source =
    ./home/.claude/skills/linear-flow/DESIGN.md;

  # ── Claude plugins ─────────────────────────────────────────────────────────
  home.file.".claude/plugins/local-auto-mode/hooks.json".source =
    ./home/.claude/plugins/local-auto-mode/hooks.json;
  home.file.".claude/plugins/local-auto-mode/classifier.py".source =
    ./home/.claude/plugins/local-auto-mode/classifier.py;
  home.file.".claude/plugins/local-auto-mode/prompt.py".source =
    ./home/.claude/plugins/local-auto-mode/prompt.py;
  home.file.".claude/plugins/local-auto-mode/config.py".source =
    ./home/.claude/plugins/local-auto-mode/config.py;

  # Merge nix-declared Claude settings into a mutable ~/.claude/settings.json.
  # Using jq's recursive merge (.[0] * .[1]) so nix values win on conflict while
  # any keys Claude wrote at runtime (MCP servers, extra permissions, etc.) are
  # preserved across home-manager switches.
  home.activation.claudeSettings = lib.hm.dag.entryAfter [ "writeBoundary" "sops" ] ''
    DEST="$HOME/.claude/settings.json"
    NIX="${claudeSettingsFile}"
    $DRY_RUN_CMD mkdir -p "$HOME/.claude"
    if [ -e "$DEST" ] && [ ! -L "$DEST" ]; then
      TMP=$(mktemp)
      ${pkgs.jq}/bin/jq -s '.[0] * .[1]' "$DEST" "$NIX" > "$TMP"
      $DRY_RUN_CMD mv "$TMP" "$DEST"
    else
      # First run or was previously a symlink from programs.claude-code.settings
      $DRY_RUN_CMD rm -f "$DEST"
      $DRY_RUN_CMD cp "$NIX" "$DEST"
      $DRY_RUN_CMD chmod 644 "$DEST"
    fi
  '';

  # Auto-install Claude Code plugins via the `claude plugin` CLI. Idempotent:
  # marketplace is added if missing, each plugin is installed if not already
  # tracked in installed_plugins.json. Runs after writeBoundary so the wrapper
  # script `claude` is already on PATH from programs.claude-code or wherever
  # else it's exposed.
  home.activation.claudePlugins = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    PLUGINS=(
      "superpowers@claude-plugins-official"
    )
    MARKETPLACE="claude-plugins-official"
    MARKETPLACE_SOURCE="anthropics/claude-plugins-official"
    INSTALLED="$HOME/.claude/plugins/installed_plugins.json"

    # The activation environment has a stripped PATH that doesn't include
    # ~/.nix-profile/bin where home-manager places claude. Expose it.
    export PATH="${config.home.profileDirectory}/bin:$PATH"

    # If claude still isn't available (fresh machine pre-bootstrap), skip
    # rather than fail the activation. Avoid `exit` here — it terminates the
    # entire home-manager activation script, not just this step.
    if ! command -v claude >/dev/null 2>&1; then
      echo "claudePlugins: claude CLI not on PATH yet — skipping plugin install"
    else
      # Register marketplace if not already known.
      if ! claude plugin marketplace list 2>/dev/null | grep -q "$MARKETPLACE"; then
        $DRY_RUN_CMD claude plugin marketplace add "$MARKETPLACE_SOURCE"
      fi

      # Install each plugin if not already tracked.
      for p in "''${PLUGINS[@]}"; do
        if [ -f "$INSTALLED" ] && grep -q "\"$p\"" "$INSTALLED"; then
          continue
        fi
        $DRY_RUN_CMD claude plugin install "$p" --scope user
      done
    fi
  '';

  # Firefox 67+ keys profile-per-install via [Install<HASH>] sections inside
  # profiles.ini (gated by `Version=2`), which override `Default=1`. Every nix
  # firefox bump produces a new install hash, so Firefox creates a fresh
  # *.default-release profile and pins it, ignoring the home-manager-managed
  # one. Drop Version= to make Firefox honor Default=1 (same posture as
  # Darwin), and rm the legacy installs.ini backup so it can't re-seed the
  # Install section on next launch.
  home.activation.firefoxInstallsIni = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD rm -f "$HOME/${config.programs.firefox.configPath}/installs.ini"
  '';

  xdg.userDirs.setSessionVariables = true;

  home.username = let v = builtins.getEnv "USER"; in if v != "" then v else "sauyon";
  home.homeDirectory = let v = builtins.getEnv "HOME"; in if v != "" then v else (if isDarwin then "/Users/sauyon" else "/home/sauyon");

  home.sessionVariables =
    import ./env.nix (
      args
      // {
        xdg = config.xdg;
        home = config.home.homeDirectory;
      }
    )
    // (lib.optionalAttrs hidpi.enabled {
      QT_FONT_DPI = toString hidpi.qtFontDpi;
    })
    # setsuna scales GTK via dconf (text-scaling-factor); other HiDPI hosts
    # don't have a dconf D-Bus service, so use GDK_DPI_SCALE instead.
    // (lib.optionalAttrs (hidpi.enabled && hostname != "setsuna") {
      GDK_DPI_SCALE = toString hidpi.scale;
    });

  # TERMINFO_DIRS is already set under systemd by home-manager's generic-linux
  # module; exclude it from this propagation to avoid a conflicting definition.
  systemd.user.sessionVariables =
    lib.mkIf (!isDarwin) (removeAttrs config.home.sessionVariables [ "TERMINFO_DIRS" ]);

  # ── Emacs ──────────────────────────────────────────────────────────────────
  home.file.".emacs.d/init.el".source = ./home/emacs/init.el;
  home.file.".emacs.d/lisp/mode-init.el".source = ./home/emacs/lisp/mode-init.el;
  home.file.".emacs.d/lisp/pref-init.el".source = ./home/emacs/lisp/pref-init.el;
  home.file.".emacs.d/lisp/root-find.el".source = ./home/emacs/lisp/root-find.el;

  services.emacs = lib.mkIf (!isDarwin && isDesktop) {
    enable = true;
    package = pkgs.emacs30-pgtk;
    client.enable = true;
  };

  # ── Scripts ────────────────────────────────────────────────────────────────
  # On darwin, .local/bin is a symlink to the dotfiles repo; skip HM management.
  home.file.".local/bin/bootstrap.sh" = lib.mkIf (!isDarwin) { executable = true; source = ./home/scripts/bootstrap.sh; };
  home.file.".local/bin/mprisinfo" = lib.mkIf (!isDarwin) { executable = true; source = ./home/scripts/mprisinfo; };
  home.file.".local/bin/reyubikey" = lib.mkIf (!isDarwin) { executable = true; source = ./home/scripts/reyubikey; };
  home.file.".local/bin/upload" = lib.mkIf (!isDarwin) { executable = true; source = ./home/scripts/upload; };
  home.file.".local/bin/yank" = lib.mkIf (!isDarwin) { executable = true; source = ./home/scripts/yank; };

  # ── Pulse ──────────────────────────────────────────────────────────────────
  xdg.configFile."pulse/client.conf" = lib.mkIf (!isDarwin) { text = "cookie-file = /.cache/pulse/cookie\n"; };

  # ── WirePlumber ────────────────────────────────────────────────────────────
  # Disable the AB13X USB headset adapter on fujiwara — unused, but keeps
  # auto-grabbing default-sink when plugged in.
  xdg.configFile."wireplumber/wireplumber.conf.d/51-disable-ab13x.conf" = lib.mkIf (hostname == "fujiwara") {
    text = ''
      monitor.alsa.rules = [
        {
          matches = [
            { device.name = "alsa_card.usb-Generic_USB_Audio_20210726905926-00" }
          ]
          actions = {
            update-props = {
              device.disabled = true
            }
          }
        }
      ]
    '';
  };

  # ── p10k ───────────────────────────────────────────────────────────────────
  xdg.configFile."zsh/.p10k.zsh".source = ./home/p10k.zsh;

  # ── Warp ───────────────────────────────────────────────────────────────────
  xdg.configFile."warp-terminal/keybindings.yaml" = lib.mkIf (!isDarwin && isDesktop) {
    source = ./home/warp/keybindings.yaml;
  };
  xdg.configFile."warp-terminal/user_preferences.json" = lib.mkIf (!isDarwin && isDesktop) {
    source = ./home/warp/user_preferences.json;
  };
  home.file.".local/share/warp-terminal/tab_configs/startup_config.toml" = lib.mkIf (!isDarwin && isDesktop) {
    source = ./home/warp/tab_configs/startup_config.toml;
  };

  home.file.".local/bin/hyprland-graceful-exit" = lib.mkIf (!isDarwin && isDesktop) {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      # Gracefully close all Hyprland windows, then optionally exit Hyprland.
      set -euo pipefail

      hyprctl clients -j | ${pkgs.jq}/bin/jq -r '.[].address' | while read -r addr; do
        hyprctl dispatch closewindow "address:$addr" || true
      done

      # Wait for windows to close (up to 5s)
      for i in $(seq 1 10); do
        count=$(hyprctl clients -j | ${pkgs.jq}/bin/jq 'length')
        [ "$count" -eq 0 ] && break
        sleep 0.5
      done

      if [ "''${1:-}" != "--no-exit" ]; then
        hyprctl dispatch exit
      fi
    '';
  };


  systemd.user.services.wayvnc = lib.optionalAttrs (!isDarwin && isDesktop) {
    Unit = {
      Description = "WayVNC server bound to the tailscale interface";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = pkgs.writeShellScript "wayvnc-tailscale" ''
        set -eu
        for _ in $(seq 1 60); do
          ip=$(${pkgs.tailscale}/bin/tailscale ip -4 2>/dev/null | head -n1 || true)
          [ -n "''${ip:-}" ] && break
          sleep 1
        done
        if [ -z "''${ip:-}" ]; then
          echo "tailscale IPv4 not available" >&2
          exit 1
        fi
        exec ${pkgs.wayvnc}/bin/wayvnc "$ip"
      '';
      Restart = "on-failure";
      RestartSec = 5;
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  systemd.user.services.hyprland-cleanup = lib.optionalAttrs (!isDarwin && isDesktop) {
    Unit = {
      Description = "Gracefully close all Hyprland windows on session end";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };
    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.coreutils}/bin/true";
      ExecStop = "${config.home.homeDirectory}/.local/bin/hyprland-graceful-exit --no-exit";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  home.packages = [
    claude-prof
  ] ++ (with pkgs; [
    bfs
    btopPkg
    google-fonts
    claude-agent-acp
    coder
    comma
    cosign
    cryptomator-cli
    lnav
    opencode
    bat
    rustup
    nixfmt
    kubectl
    kube-capacity
    kubectx
    unzip
    zip
    (emacsPackages.treesit-grammars.with-grammars (grammars: with grammars; [
      tree-sitter-tsx
      tree-sitter-typescript
    ]))
  ]) ++ lib.optionals (!isDarwin) [
    agent-orchestrator-pkg
    ao-mcp-pkg
    ao-run
  ] ++ lib.optionals (!isDesktop) [
    pkgs.ghostty.terminfo
  ] ++ lib.optionals (!isDarwin && isDesktop) [
    caffeine
    hypr-fullscreen-inhibit
    nixGL

    pkgs.emacs30-pgtk
    pkgs.hyprpicker
    pkgs.slack
    pkgs.vesktop
    (config.lib.nixGL.wrap pkgs.warp-terminal)
    pkgs.wayvnc
    pkgs.xdg-utils
  ];

  nixpkgs.config = {
    allowUnfree = true;
    sandbox = true;
  };

  nixpkgs.overlays = [
    (final: prev: {
      nur = import (builtins.fetchTarball {
        url = "https://github.com/nix-community/NUR/archive/4b22de075887985d445668c4634ae148618c6a41.tar.gz";
        sha256 = "1fkb8bv1qfls4gvvim91pgxms6vidm093ycc3vwnacygjgbv5hqh";
      }) {
        nurpkgs = prev;
        pkgs = prev;
      };
    })
    (self: super: {
      any-nix-shell-s = super.any-nix-shell.overrideAttrs (old: {
        src = super.fetchFromGitHub {
          owner = "sauyon";
          repo = "any-nix-shell";
          rev = "3a99be0b3d76a691c940608c477955d122f37e75";
          sha256 = "1g735n0xr50vgcw30igldhmjvb40jgk65x5qjnnxidvm1i3vykw9";
        };
      });
    })
    (final: prev: {
      astal = prev.astal // {
        network = prev.astal.network.overrideAttrs (old: {
          patches = (old.patches or []) ++ [
            ./patches/astal-network-null-bssid.patch
          ];
        });
      };
    })
    (final: prev: {
      hyprlock = prev.hyprlock.overrideAttrs (old: {
        patches = (old.patches or []) ++ [
          ./patches/hyprlock-skip-dtors-on-early-fail.patch
        ];
      });
    })
    (final: prev: {
      claude-agent-acp = prev.buildNpmPackage rec {
        pname = "claude-agent-acp";
        version = "0.33.1";
        src = prev.fetchFromGitHub {
          owner = "agentclientprotocol";
          repo = "claude-agent-acp";
          rev = "v${version}";
          hash = "sha256-FwcIJf/tfH6prDFKtOo7X1mTocibf4Ne6JHOS9ITG8U=";
        };
        npmDepsHash = "sha256-y795LyNjSJjTpIqtA5bC/AgeFLghM0yU5xQRD3m+Ajs=";
        dontNpmPrune = true;
      };
    })
  ];

  home.pointerCursor = lib.mkIf (!isDarwin && isDesktop) {
    package = pkgs.yaru-theme;
    name = "Yaru";
    size = hidpi.cursorSize;
    gtk.enable = true;
  };

  gtk = lib.optionalAttrs (!isDarwin && isDesktop) {
    enable = true;
    colorScheme = "dark";
    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
    gtk3.extraConfig.gtk-key-theme-name = "Emacs";
    gtk3.extraCss = ''
      @binding-set mac-bindings {
        bind "<Super>x" { "cut-clipboard" () };
        bind "<Super>c" { "copy-clipboard" () };
        bind "<Super>v" { "paste-clipboard" () };
        bind "<Super>a" { "select-all" (true) };
        bind "<Super>z" { "undo" () };
        bind "<Super><Shift>z" { "redo" () };
      }
      * { -gtk-key-bindings: mac-bindings; }
    '';
    gtk4.extraConfig.gtk-key-theme-name = "Emacs";
    gtk4.extraCss = ''
      @binding-set mac-bindings {
        bind "<Super>x" { "cut-clipboard" () };
        bind "<Super>c" { "copy-clipboard" () };
        bind "<Super>v" { "paste-clipboard" () };
        bind "<Super>a" { "select-all" (true) };
        bind "<Super>z" { "undo" () };
        bind "<Super><Shift>z" { "redo" () };
      }
      * { -gtk-key-bindings: mac-bindings; }
    '';
    theme = {
      name = "Plano";
      package = pkgs.plano-theme;
    };
    gtk4.theme = config.gtk.theme;
    iconTheme = {
      name = "Yaru-dark";
      package = pkgs.yaru-theme;
    };
    font = {
      name = "NotoSans Nerd Font";
      package = pkgs.nerd-fonts.noto;
    };
  };

  qt = lib.optionalAttrs (!isDarwin && isDesktop) {
    enable = true;
    platformTheme.name = "gtk2";
  };

  programs.walker = lib.optionalAttrs (!isDarwin && isDesktop) {
    enable = true;
    runAsService = true;
  };

  services = {
    hyprpaper = {
      enable = !isDarwin && isDesktop;
      package = config.lib.nixGL.wrap pkgs.hyprpaper;
      settings = {
        path = "${config.home.homeDirectory}/images/wallpapers/${hostname}.png";
      };
    };

    kanshi = lib.optionalAttrs (!isDarwin && isDesktop) {
      enable = true;
      settings = [
        {
          output = {
            criteria = "BOE NE160QDM-NZ6 Unknown";
            mode = "2560x1600";
            position = "0,0";
            scale = 2.0;
            transform = "normal";
            alias = "UTSUHO";
          };
        }
        {
          output = {
            criteria = "BOE 0x095F Unknown";
            mode = "2256x1504";
            position = "0,0";
            scale = 1.0;
            transform = "normal";
            alias = "SETSUNA";
          };
        }
        {
          profile = {
            name = "setsuna";
            outputs = [
              { criteria = "$SETSUNA"; status = "enable"; scale = 1.0; }
            ];
          };
        }
        {
          profile = {
            name = "utsuho";
            outputs = [
              { criteria = "$UTSUHO"; status = "enable"; scale = 1.0; }
            ];
          };
        }
        {
          profile = {
            name = "home";
            outputs = [
              { criteria = "GIGA-BYTE TECHNOLOGY CO., LTD. AORUS FO48U 21170B001458"; mode = "3840x2160"; position = "0,0"; scale = 2.0; }
              { criteria = "eDP-1"; status = "disable"; }
            ];
          };
        }
        {
          profile = {
            name = "Modular";
            outputs = [
              { criteria = "Dell Inc. DELL P3424WEB F2VTM04"; mode = "3440x1440"; position = "-528,-1440"; transform = "normal"; scale = 1.0; }
              { criteria = "$UTSUHO"; status = "enable"; scale = 1.0; }
            ];
          };
        }
        {
          profile = {
            name = "fujiwara";
            outputs = [
              { criteria = "Samsung Electric Company S90F 0x01000E00"; mode = "3840x2160"; position = "0,0"; scale = 1.0; }
            ];
          };
        }
      ];
    };

    gpg-agent = lib.optionalAttrs (!isDarwin) {
      enable = true;
      # SSH support handled by ssh-tpm-agent (below), which falls back here
      # for non-TPM keys via the fallback socket arg.
      enableSshSupport = false;
      defaultCacheTtl = 600;
      maxCacheTtl = 1200;
      pinentry.package = if isDesktop then pkgs.pinentry-gnome3 else pkgs.pinentry-curses;
    };

    ssh-tpm-agent = lib.optionalAttrs (!isDarwin) {
      enable = true;
    };

    gnome-keyring = lib.optionalAttrs (!isDarwin && isDesktop) {
      enable = true;
      components = [
        "pkcs11"
        "secrets"
      ];
    };

    hypridle = lib.optionalAttrs (!isDarwin && isDesktop) {
      enable = true;
      settings = {
        general = {
          lock_cmd = "pidof hyprlock || ${config.programs.hyprlock.package}/bin/hyprlock";
          before_sleep_cmd = "loginctl lock-session";
          after_sleep_cmd = "hyprctl dispatch dpms on";
        };
        listener = [
          {
            timeout = 300;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
          {
            timeout = 600;
            on-timeout = "loginctl lock-session";
          }
        ];
      };
    };

    mako = lib.optionalAttrs (!isDarwin && isDesktop) {
      enable = true;
      settings = {
        background-color = "#1a1b26e6";
        text-color = "#c0caf5";
        border-color = "#7aa2f7";
        border-size = 2;
        border-radius = 8;
        default-timeout = 5000;
        font = "NotoSans Nerd Font 11";
        padding = "10";
        margin = "8";
        max-visible = 5;
        anchor = "top-right";
        "urgency=high" = {
          border-color = "#f7768e";
          default-timeout = 0;
        };
        "urgency=low" = {
          border-color = "#565f89";
        };
      };
    };
  };

  targets.genericLinux.enable = !isDarwin;
  targets.genericLinux.nixGL.packages = lib.mkIf (!isDarwin && isDesktop) nixgl.packages.${system};

  wayland.windowManager.hyprland = lib.optionalAttrs (!isDarwin && isDesktop) (import ./hyprland.nix { inherit pkgs config edgeGap; });

  dconf = {
    enable = hostname == "setsuna";
    settings = lib.optionalAttrs (hostname == "setsuna") {
      "org/gnome/desktop/interface" = {
        text-scaling-factor = hidpi.scale;
      };
    };
  };

  fonts.fontconfig.enable = true;


  programs = {
    hyprlock = lib.optionalAttrs (!isDarwin && isDesktop) {
      enable = true;
      package = config.lib.nixGL.wrap pkgs.hyprlock;
      settings = {
        general = {
          hide_cursor = true;
        };

        background = [
          {
            monitor = "";
            # path = "screenshot";   # disabled to debug deadlock
            blur_passes = 3;
            blur_size = 8;
          }
        ];

        auth = {
          "fingerprint:enabled" = true;
        };

        input-field = [
          {
            monitor = "";
            size = "300, 50";
            position = "0, -80";
            halign = "center";
            valign = "center";
            outline_thickness = 2;
            dots_size = 0.33;
            dots_spacing = 0.15;
            dots_center = true;
            outer_color = "rgb(151515)";
            inner_color = "rgb(200, 200, 200)";
            font_color = "rgb(10, 10, 10)";
            fade_on_empty = true;
            placeholder_text = "<i>Password...</i>";
            hide_input = false;
            check_color = "rgb(204, 136, 34)";
            fail_color = "rgb(204, 34, 34)";
            fail_text = "<i>$FAIL <b>($ATTEMPTS)</b></i>";
            capslock_color = "rgb(170, 0, 255)";
          }
        ];

        label = [
          {
            monitor = "";
            text = ''cmd[update:1000] echo "$(date +"%H:%M:%S")"'';
            font_size = 64;
            font_family = "NotoSans Nerd Font";
            position = "0, 80";
            halign = "center";
            valign = "center";
            color = "rgba(255, 255, 255, 0.9)";
          }
          {
            monitor = "";
            text = ''cmd[update:60000] echo "$(date +"%A, %B %-d")"'';
            font_size = 24;
            font_family = "NotoSans Nerd Font";
            position = "0, 10";
            halign = "center";
            valign = "center";
            color = "rgba(255, 255, 255, 0.7)";
          }
          {
            monitor = "";
            text = " $FPRINTPROMPT";
            font_size = 14;
            font_family = "NotoSans Nerd Font";
            position = "0, -140";
            halign = "center";
            valign = "center";
            color = "rgba(255, 255, 255, 0.7)";
          }
        ];
      };
    };
    waybar = let
      fontSize = hidpi.waybarFontSize;
      barHeight = hidpi.waybarBarHeight;
      shared = {
        layer = "top";
        position = "top";
        height = barHeight;
        spacing = 0;
        margin-top = edgeGap;
        margin-left = edgeGap;
        margin-right = edgeGap;

        "hyprland/workspaces" = {
          format = "{id}";
          on-click = "activate";
          sort-by-number = true;
        };
        "hyprland/window" = {
          format = "{title}";
          max-length = 60;
          separate-outputs = true;
        };
        mpris = {
          format = "{player_icon} {dynamic}";
          format-paused = "{status_icon} {dynamic}";
          player-icons.default = "";
          status-icons.paused = "";
          dynamic-len = 40;
        };
        wireplumber = {
          format = "{icon} {volume}%";
          format-muted = "󰝟";
          format-icons = [ "" "" "" ];
          on-click = "pavucontrol";
          scroll-step = 5;
        };
        network = {
          format-wifi = "  {essid}";
          format-ethernet = " {ifname}";
          format-disconnected = "󰖪 offline";
          tooltip-format = "{ifname}: {ipaddr}";
          max-length = 30;
          on-click = "ghostty -e nmtui";
        };
        bluetooth = {
          format = " {status}";
          format-disabled = "󰂲";
          format-connected = " {device_alias}";
          format-connected-battery = " {device_alias} {device_battery_percentage}%";
          tooltip-format = "{controller_alias}\n{num_connections} connected";
        };
        tray = {
          spacing = 8;
          icon-size = 18;
        };
        memory = {
          format = "󰍛 {percentage}%";
          interval = 2;
        };
        battery = {
          states = { warning = 30; critical = 15; };
          format = "{icon} {capacity}%";
          format-charging = "󰂄 {capacity}% (+{time})";
          format-discharging = "{icon} {capacity}% (-{time})";
          format-plugged = "󰚥 {capacity}%";
          format-full = "󰁹 {capacity}%";
          format-time = "{H}:{M:02}";
          format-icons = [ "" "" "" "" "" ];
        };
        clock = {
          format = "{:%a %m-%d %H:%M:%S}";
          interval = 1;
          tooltip-format = "<tt>{calendar}</tt>";
        };
        "custom/notifications" = {
          exec = ''makoctl mode | grep -qx do-not-disturb && echo '{"text":"󰂛","class":"dnd"}' || echo '{"text":"󰂚"}' '';
          return-type = "json";
          interval = 2;
          on-click = "makoctl dismiss --all";
          on-click-right = "makoctl mode -t do-not-disturb";
        };
        "custom/caffeine" = {
          exec = "${caffeine}/bin/caffeine waybar";
          return-type = "json";
          interval = 5;
          signal = 10;
          on-click = "${caffeine}/bin/caffeine toggle";
        };
      };
    in {
      enable = !isDarwin && isDesktop;
      systemd.enable = !isDarwin && isDesktop;
      settings = [
        (shared // {
          output = [ "eDP-1" ];
          modules-left = [ "hyprland/workspaces" "hyprland/window" ];
          modules-center = [ "mpris" ];
          modules-right = [ "wireplumber" "network" "battery" "tray" "custom/caffeine" "clock" "custom/notifications" ];
        })
        (shared // {
          output = [ "!eDP-1" "*" ];
          modules-left = [ "hyprland/workspaces" "hyprland/window" ];
          modules-center = [ "mpris" ];
          modules-right = [ "wireplumber" "network" "bluetooth" "tray" "memory" "custom/caffeine" "clock" "custom/notifications" ];
        })
      ];
      style = ''
        @define-color bg          #1a1b26;
        @define-color bg-darker   #16161e;
        @define-color bg-lighter  #24283b;
        @define-color fg          #c0caf5;
        @define-color fg-dim      #a9b1d6;
        @define-color comment     #565f89;
        @define-color border      #292e42;
        @define-color red         #f7768e;
        @define-color orange      #ff9e64;
        @define-color yellow      #e0af68;
        @define-color green       #9ece6a;
        @define-color cyan        #7dcfc2;
        @define-color blue        #7aa2f7;
        @define-color purple      #bb9af7;

        * {
          font-family: "NotoSans Nerd Font", sans-serif;
          font-size: ${toString fontSize}px;
          border: none;
          border-radius: 0;
          min-height: 0;
        }

        window#waybar {
          background: alpha(@bg, 0.95);
          color: @fg;
          border-bottom: 1px solid @border;
        }

        #workspaces button {
          background: transparent;
          color: @fg-dim;
          padding: 0 12px;
          margin: 0;
          border-bottom: 6px solid transparent;
          transition: color 150ms, border-color 150ms;
        }
        #workspaces button:hover {
          background: alpha(#a695d0, 0.12);
          color: @fg;
          box-shadow: none;
        }
        #workspaces button.active {
          color: @fg;
          border-bottom: 6px solid #a695d0;
        }
        #workspaces button.urgent {
          color: @red;
          border-bottom: 6px solid @red;
        }

        #window { padding: 0 12px; color: @fg-dim; }
        window#waybar.empty #window { background: transparent; }

        #mpris { padding: 0 12px; color: @purple; }

        #wireplumber,
        #network,
        #bluetooth,
        #tray,
        #memory,
        #battery,
        #clock,
        #custom-caffeine,
        #custom-notifications { padding: 0 10px; }

        #custom-caffeine.off { color: @comment; }
        #custom-caffeine.on { color: @yellow; }

        #wireplumber { color: @cyan; }
        #wireplumber.muted { color: @comment; }
        #network { color: @green; }
        #network.disconnected { color: @red; }
        #bluetooth { color: @blue; }
        #bluetooth.disabled, #bluetooth.off { color: @comment; }
        #memory { color: @orange; }
        #battery { color: @green; }
        #battery.warning:not(.charging) { color: @yellow; }
        #battery.critical:not(.charging) { color: @red; }
        #battery.charging { color: @cyan; }
        #clock { color: @fg; font-weight: 600; }
        #custom-notifications { color: @yellow; }
        #custom-notifications.dnd { color: @comment; }

        tooltip {
          background: @bg-darker;
          border: 1px solid @border;
        }
        tooltip label { color: @fg; padding: 6px; }
      '';
    };

    thunderbird = lib.optionalAttrs (!isDarwin && isDesktop) {
      enable = true;
      profiles.default = {
        isDefault = true;
        extensions = [
          (pkgs.fetchFirefoxAddon {
            name = "tbkeys";
            url = "https://github.com/wshanks/tbkeys/releases/download/v2.4.3/tbkeys.xpi";
            hash = "sha256-2e+T5Nr5kc2s8EykFzWKaJZ2jPUDHh9Cqn4hCuDCLaM=";
          })
        ];
      };
      settings = {
        "mail.tabs.drawInTitlebar" = false;
        "ui.key.accelKey" = 91;
        "ui.key.textcontrol.prefer_native_key_bindings_over_builtin_shortcut_key_definitions" = true;
        "extensions.tbkeys.mainkeys" = builtins.toJSON {
          # cycle panes
          "ctrl+x o" = "eval:document.commandDispatcher.advanceFocus()";
          # navigation
          "alt+n" = "cmd:cmd_nextMsg";
          "alt+p" = "cmd:cmd_previousMsg";
          # actions
          "c" = "cmd:cmd_newMessage";
          "r" = "cmd:cmd_reply";
          "a" = "cmd:cmd_replyAll";
          "f" = "cmd:cmd_forward";
          "d" = "cmd:cmd_delete";
          "e" = "cmd:cmd_archive";
          "enter" = "cmd:cmd_openMessage";
          "u" = "tbkeys:closeMessageAndRefresh";
          # unset defaults that conflict
          "j" = "unset";
          "k" = "unset";
          "o" = "unset";
          "x" = "unset";
          "#" = "unset";
        };
      };
    };

    claude-code = {
      enable = true;
      # enableMcpIntegration = true;
      # settings is intentionally unset — managed via home.activation.claudeSettings
      # below so the file stays mutable (Claude can edit it at runtime).
    };

    home-manager.enable = true;

    difftastic = {
      enable = !isDarwin;
      git.enable = true;
      options = {
        # display = "inline";
      };
    };
    firefox = {
      enable = isDesktop;
      # On Linux, env.nix sets MOZ_LEGACY_PROFILES=1 (and system Arch firefox
      # uses legacy unconditionally), so use .mozilla/firefox.
      # On macOS, Firefox reads from ~/Library/Application Support/Firefox.
      configPath = if isDarwin then "Library/Application Support/Firefox" else ".mozilla/firefox";
      # Drop Version= so Firefox uses non-dedicated profile mode and honors
      # Default=1 — without this, Firefox 67+ pins profile-per-install via
      # [Install<HASH>] sections in profiles.ini and ignores Default=.
      profileVersion = null;
      policies = {
        Homepage = {
          URL = "https://ko.ag/newtab.html";
          StartPage = "homepage";
        };
      };
      nativeMessagingHosts = lib.optionals (!isDarwin) [
        pkgs.tridactyl-native
      ];
      profiles.default = {
        extensions.packages = lib.optionals (!isDarwin) (with pkgs.nur.repos.rycee.firefox-addons; [
          tridactyl
        ]);
        settings = {
          "sidebar.verticalTabs" = true;
          "ui.key.accelKey" = 91;
          "ui.key.textcontrol.prefer_native_key_bindings_over_builtin_shortcut_key_definitions" = true;
          "signon.rememberSignons" = false;
          "browser.newtab.extensionControlled" = false;
          "browser.ml.chat.enabled" = false;
          # WebTransport workaround: this profile reports hasThirdPartyRoots=1
          # for every QUIC connection (including public sites that chain to
          # built-in roots), so Firefox's third-party-roots policy kills H3.
          # HTTPS silently falls back to H2; WebTransport has no fallback and
          # fails with "WebTransport connection rejected". See
          # netwerk/protocol/http/Http3Session.cpp Authenticated() and
          # bugzilla 1929093.
          "network.http.http3.disable_when_third_party_roots_found" = false;
        };
      };
    };
    ghostty = {
      enable = isDesktop;
      package = if isDarwin then pkgs.ghostty else config.lib.nixGL.wrap pkgs.ghostty;
      enableZshIntegration = true;
      systemd.enable = false;
      # installBatSyntax = true;

      settings = {
        keybind = [
          "ctrl+enter=text:\\r"
          "performable:super+c=copy_to_clipboard"
          "performable:super+v=paste_from_clipboard"
          "super+t=new_tab"
          "ctrl+comma=unbind"
        ];
      } // lib.optionalAttrs hidpi.enabled {
        font-size = hidpi.ghosttyFontSize;
      };
    };
    gh = {
      enable = true;
      gitCredentialHelper.enable = true;
    };
    ripgrep = {
      enable = true;
      arguments = [
        "--smart-case"
        "--type-add"
        "ql:*.{ql,qll}"
        "--hidden"
      ];
    };
    dircolors = {
      enable = true;
      enableZshIntegration = true;
      extraConfig = builtins.readFile ./home/dircolors;
    };
    direnv = {
      enable = true;
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
      options = [ "--cmd cd" ];
    };

    fzf.enable = true;

    man = {
      enable = true;
      mandoc.enable = true;
      man-db.enable = false;
    };

    git = {
      enable = true;
      ignores = [
        ".DS_Store"
        ".vscode"
        "*~"
        "\\#*#"
        "*.orig"
        ".#*"
        ".dir-locals.el"
        "*.zip"
        "*.tar"
        "*.out"
        "*.xz"
        "*.gz"
        "*.7z"
        "shell.nix"
        "flake.nix"
        "flake.lock"
        "*.local.json"
        "*.local.toml"
        ".aider*"
        "**/.claude/worktrees"
        "**/.claude/scheduled_tasks.lock"
        "**/.claude/plans"
      ];

      signing = {
        format = "openpgp";
        signByDefault = false;
        key = "git@sjle.co";
      };

      lfs.enable = true;

      settings = {
        user.name = "Sauyon Lee";
        user.email = "git@sjle.co";
        safe.directory = [
          "/tf/*"
        ];
        init.defaultBranch = "main";
        commit = {
          verbose = true;
        };
        push = {
          default = "current";
        };
        color = {
          ui = "auto";
        };
        core = {
          pager = "${pkgs.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less -RFx4";
          editor = if isDarwin then "/usr/bin/emacsclient -t" else "${pkgs.emacs30-pgtk}/bin/emacsclient -t";
          whitespace = "trailing-space,space-before-tab";
        };
        diff.algorithm = "histogram";
        pull.rebase = true;
        merge.tool = "meld";
        # credential."https://github.com".helper = "!/usr/bin/env gh auth git-credential";
        # credential."https://gist.github.com".helper = "!/usr/bin/env gh auth git-credential";
      };
    };

    gpg = {
      enable = true;
      settings = {
        keyserver = "hkps://keyserver.ubuntu.com";
      };
    };

    ssh = lib.optionalAttrs (!isDarwin) {
      enable = true;

      enableDefaultConfig = false;

      matchBlocks = {
        "aur" = {
          hostname = "aur.archlinux.org";
          user = "aur";
        };
        "github" = {
          hostname = "github.com";
          user = "git";
        };
        "shizuka" = {
          port = 59049;
        };
        "akane" = {
          port = 59049;
        };
        "kanon" = {
          user = "root";
          hostname = "kanon.alai-ionian.ts.net";
          port = 59048;
        };
        "yui mio ritsu mugi azusa" = {
          forwardAgent = true;
        };
        "testserver" = {
          hostname = "35.163.118.10";
          user = "ubuntu";
        };
        "testclient" = {
          hostname = "52.38.68.189";
          user = "ubuntu";
        };
        "tf" = {
          hostname = "kanon.ko.ag";
          port = 59048;
          forwardAgent = true;
          remoteForwards = [
            {
              bind.address = "/run/user/1000/gnupg/S.gpg-agent";
              host.address = "/run/user/1000/gnupg/S.gpg-agent.extra";
            }
          ];
        };
        "prod-db-subnet-router" = {
          user = "ec2-user";
        };
        "bcctl-subnet-router" = {
          user = "ubuntu";
        };

        "coder.*" = {
          userKnownHostsFile = "/dev/null";
          extraOptions = {
            ConnectTimeout = "0";
            StrictHostKeyChecking = "no";
            LogLevel = "ERROR";
            ProxyCommand = "/usr/bin/coder --global-config /home/sauyon/.config/coderv2 ssh --stdio --ssh-host-prefix coder. %h";
          };
        };
        "*.coder-proxy" = {
          match = "host *.coder !exec \"${pkgs.coder}/bin/.coder-wrapped connect exists %h\"";
          extraOptions = {
            ProxyCommand = "${pkgs.coder}/bin/.coder-wrapped --global-config /home/sauyon/.config/coderv2 ssh --stdio --hostname-suffix coder %h";
          };
        };
        "*.coder" = {
          userKnownHostsFile = "/dev/null";
          extraOptions = {
            ConnectTimeout = "0";
            StrictHostKeyChecking = "no";
            LogLevel = "ERROR";
          };
        };
      };
    };

    starship = {
      enable = false;

      settings = {
        add_newline = false;
        scan_timeout = 10;

        git_status = {
          ahead = "⇡\${count}";
          diverged = "⇡\${ahead_count}⇣\${behind_count}";
          behind = "⇣\${count}";
          untracked = "?\${count}";
          modified = "!\${count}";
          staged = "+\${count}";
          renamed = "»\${count}";
          deleted = "×\${count}";
        };

        kubernetes = {
          disabled = false;
        };
      };
    };

    zsh = import ./zsh.nix (
      args
      // {
        xdg = config.xdg;
        home = config.home.homeDirectory;
      }
    );

    zellij = {
      enable = true;
      enableZshIntegration = false;
      settings = {
        keybinds = {
          normal = {
            "bind \"Alt s\"".SwitchToMode = "Locked";
            unbind = "Ctrl g";
          };
          locked = {
            "bind \"Alt s\"".SwitchToMode = "Normal";
            unbind = "Ctrl g";
          };
        };

        default_mode = "locked";
        pane_frames = false;
        show_startup_tips = false;
      };
    };
  };

  xdg = {
    mime.enable = !isDarwin;

    portal = {
      enable = !isDarwin && isDesktop;
      extraPortals = lib.optionals (!isDarwin && isDesktop) [
        pkgs.xdg-desktop-portal-gtk
      ];
      xdgOpenUsePortal = !isDarwin && isDesktop;
      config = {
        common.default = [ "hyprland;gtk" ];
      };
    };

    mimeApps = {
      enable = !isDarwin && isDesktop;

      defaultApplications = {
        "text/html" = "firefox.desktop";
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/mailto" = "thunderbird.desktop";
        "message/rfc822" = "thunderbird.desktop";
      };
    };

    dataHome = "${config.home.homeDirectory}/.local/share";
    configHome = "${config.home.homeDirectory}/.config";
    cacheHome = "${config.home.homeDirectory}/.cache";

    userDirs = {
      enable = true;

      desktop = "${config.home.homeDirectory}/desktop";
      documents = "${config.home.homeDirectory}/documents";
      download = "${config.home.homeDirectory}/downloads";
      music = "${config.home.homeDirectory}/drive/music";
      pictures = "${config.home.homeDirectory}/images";
    };

    configFile."newtab.html".text = newtabHtml;

    # Standalone home-manager doesn't put ~/.nix-profile/share/systemd/user in
    # systemd's search path, so the dbus-activated portal services fail with
    # "unknown unit" and ghostty's OpenURI portal call falls back to spawning
    # xdg-open (and the browser) as a child. Symlink the units in so dbus
    # activation finds them.
    configFile."systemd/user/xdg-desktop-portal.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${pkgs.xdg-desktop-portal}/share/systemd/user/xdg-desktop-portal.service";
    };
    configFile."systemd/user/xdg-document-portal.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${pkgs.xdg-desktop-portal}/share/systemd/user/xdg-document-portal.service";
    };
    configFile."systemd/user/xdg-permission-store.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${pkgs.xdg-desktop-portal}/share/systemd/user/xdg-permission-store.service";
    };
    configFile."systemd/user/xdg-desktop-portal-rewrite-launchers.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${pkgs.xdg-desktop-portal}/share/systemd/user/xdg-desktop-portal-rewrite-launchers.service";
    };
    configFile."systemd/user/xdg-desktop-portal-gtk.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${pkgs.xdg-desktop-portal-gtk}/share/systemd/user/xdg-desktop-portal-gtk.service";
    };
    configFile."systemd/user/xdg-desktop-portal-hyprland.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${pkgs.xdg-desktop-portal-hyprland}/share/systemd/user/xdg-desktop-portal-hyprland.service";
    };

    configFile."agent-orchestrator/config.yaml".text = let
      # Indent a multiline string for embedding inside a YAML block scalar.
      # orchestratorRules is used under keys indented 8 spaces, so each line
      # needs 8 extra spaces to stay inside the YAML | block.
      indentYaml = n: s:
        let pad = lib.concatStrings (builtins.genList (_: " ") n);
        in lib.concatMapStringsSep "\n" (line: if line == "" then "" else pad + line) (lib.splitString "\n" s);
      # Rules sent to WORKER agents (the ones writing/reviewing code)
      agentRules = ''
        ## Build Verification (MANDATORY)

        You MUST verify your changes build and pass lint before creating a PR
        or reporting work as complete. A change that doesn't build is not done.

        1. Read the project's AGENTS.md / CLAUDE.md for the exact build and lint
           commands (they vary by project).
        2. After making changes, run the relevant lint/build commands for the
           files you touched. For example in mcloud:
           - Go changes: `golangci-lint run --timeout=10m`
           - Dashboard changes: `cd dashboard && pnpm install && pnpm lint`
           - Helm changes: `cd helm/bentocloud && make lint && make template`
        3. If the build or lint fails, fix the errors before proceeding.
        4. Never create a PR with known build failures.

        ## Test-Driven Development

        Follow TDD strictly:
        1. **RED** — Write a failing test that captures the requirement or bug.
           Run it to confirm it fails.
        2. **GREEN** — Implement the minimal code to make the test pass.
           Run the test to confirm it passes.
        3. **REFACTOR** — Clean up while keeping tests green.

        Do not skip running tests. If you write a test, you must execute it.
        If you implement code, you must run existing tests to check for regressions.

        ## MCP Tools Available

        You have MCP tools pre-configured. Use them — do not skip.

        ### Playwright (UI/Frontend QA)
        When your changes touch frontend code (components, pages, styles, assets):
        1. Use `mcp__playwright__browser_navigate` to open the deploy preview URL
           or local dev server in headless Chromium.
        2. Use `mcp__playwright__browser_snapshot` to capture the DOM state.
        3. Use `mcp__playwright__browser_click`, `browser_type`, etc. to interact
           with the UI and verify your changes work.
        4. Check that pages adjacent to your changes still render correctly.
        5. If no preview URL is available, note it in your PR — do not silently
           skip UI validation for frontend changes.

        ### bcctl (Remote Environment Testing)
        When your changes require a running backend to validate (API changes,
        model serving, deployment configs):
        1. Use `mcp__bcctl__list_environments` to find available dev environments.
        2. Use bcctl tools to create/attach to a dev environment and sync the
           PR branch for testing.
        3. Run the relevant test suite or manual verification against the remote
           environment.

        ## PR Workflow
        - Always create PRs as drafts.
        - Read AGENTS.md for Linear ticket requirements — PRs must link a ticket.
        - If skills are listed in AGENTS.md (e.g. `develop-feature`, `bentocloudctl`),
          invoke them as instructed.
      '';

      # Rules sent to the ORCHESTRATOR (coordination, not implementation)
      orchestratorRules = ''
        ## TDD-First Workflow (Test-Driven Development)

        Every task follows this multi-stage TDD workflow — no exceptions:

        ### Phase 1: Test Design & Implementation (RED)
        1. **Spawn a Test Engineer worker** to write ONLY the failing tests that
           capture the requirement or bug. They must NOT implement the fix or
           the feature yet.
        2. **Once the test PR is open**, spawn a **Test Review agent** with this prompt:
           "You are a Quality Engineer. Review these tests. Do they accurately
           capture the requirement? Are they sufficiently exhaustive (edge cases,
           error paths, performance constraints)? Do they fail as expected against
           the current codebase? Rate 1–5 and give an overall score. If the score
           is below 5/5, list specific changes required to make the test suite
           a perfect descriptor of the goal."
        3. **Iterate** until the Test Review agent gives a 5/5 score.

        ### Phase 2: Code Implementation (GREEN)
        4. **Only after the tests are approved (5/5)**, spawn an **Implementation worker**.
           Their task is to implement the code necessary to make the previously
           approved tests pass. They must not modify the tests unless they find
           a genuine error in the test design (which requires a new Phase 1 review).
        5. **Once the implementation PR is open**, spawn a **Code Review agent**
           with the standard checklist (correctness, readability, performance).

        ### Phase 3: Final Approval
        6. **Spawn a Code Review agent** for general correctness and readability.
        7. **If the task touches performance-sensitive paths** (data processing,
           hot loops, public APIs), also spawn a **Perf Review agent**:
           "Review this PR for performance regressions: unnecessary allocations,
           O(n²) patterns, missing indexes, and blocking I/O. Flag any degradation
           in p99 latency or memory usage."
        8. **The Code Review agent** must also verify that the tests from Phase 1
           still pass and are properly integrated.

        ### Deploy Preview Validation (all PRs)
        Every review agent MUST identify and use the deploy preview for the PR:
        1. Run `gh pr checks <PR>` and `gh pr view <PR> --comments` to find the
           deploy preview URL (look for Vercel, Netlify, or any bot-posted URL).
        2. If a preview URL exists, validate that it loads and the changed
           functionality works as described in the PR.
        3. If no preview URL is available after checks complete, note it in the
           review — do not block, but flag that preview validation was skipped.

        ### UI/Frontend QA (required when PR touches UI)
        If the PR modifies frontend code (components, pages, styles, assets),
        the QA agent MUST perform end-to-end validation against the live preview:
        1. Use the **Playwright MCP** server (available as `playwright` in your
           MCP tools) to launch a headless Chromium browser against the deploy
           preview URL. Use its navigation, click, fill, and screenshot tools.
        2. Write and run ad-hoc test flows that exercise the changed UI paths:
           navigate to affected pages, interact with modified components, verify
           visual correctness and functional behavior.
        3. Check for regressions: confirm that pages adjacent to the change still
           render and behave correctly.
        4. Capture screenshots of before/after states where relevant and attach
           them to the review.
        5. Rate the UI separately (UX correctness, accessibility, responsiveness)
           in addition to the code review score. Both must be 5/5.
        If Playwright is not available or the preview is down, escalate to the
        user — do NOT silently skip UI validation for frontend PRs.

        ### Remote Environment Testing (bcctl)
        The **bcctl MCP** server is available for provisioning and managing
        BentoCloud dev environments. Use it when:
        - The change requires a running backend to validate (API changes, model
          serving, deployment configs).
        - You need to test against a real BentoCloud cluster rather than local mocks.
        - The deploy preview is insufficient (e.g. backend-only changes with no
          preview URL).
        Use `bcctl` tools to create/attach to a dev environment, sync the PR
        branch, and run the relevant test suite or manual verification there.

        ### Worker Spawn Best Practices
        When spawning ANY worker, always include in the prompt:
        - "Read AGENTS.md for build commands and lint requirements"
        - "Run builds and tests — do not skip"
        - "You have Playwright and bcctl MCP tools available — use them"
        These instructions are critical because workers only see agentRules,
        not these orchestrator rules.

        9. **Stop only when all required review agents (Code/Perf/QA) give 5/5.**
           Then notify the user that the PR is ready for the user to manually
           promote. (Drafts forever.)

        ## Escalation
        Use `ao ask` only when you genuinely need user input:
        - Design decisions the user must make
        - Ambiguous requirements that need clarification
        - Notifying the user that a PR is ready for review (5/5 score)
        - Escalating issues that workers cannot resolve

        Do NOT use this for routine status updates — those go through `ao status`.
        Only contact the user when you genuinely need their input or attention.
      '';
    in ''
      # managed by home-manager
      defaults:
        orchestrator:
          agent: hermes
        agentRules: |
${indentYaml 10 agentRules}

      plugins:
        - name: hermes
          source: local
          path: /repos/agent-orchestrator/packages/plugins/agent-hermes

      projects:
        mcloud:
          repo: bentoml/modularcloud
          path: /repos/mcloud
          defaultBranch: main
          runtime: tmux
          agent: claude-code
          orchestrator:
            agent: hermes
          agentConfig:
            permissions: default
            model: moonshotai/kimi-k2.5
          orchestratorRules: |
${indentYaml 12 orchestratorRules}

        mammoth:
          repo: modularml/mammoth
          path: /repos/mammoth
          defaultBranch: main
          runtime: tmux
          agent: claude-code
          orchestrator:
            agent: hermes
          agentConfig:
            permissions: default
            model: moonshotai/kimi-k2.5
          orchestratorRules: |
${indentYaml 12 orchestratorRules}

        modular:
          repo: modularml/modular
          path: /repos/modular
          defaultBranch: main
          runtime: tmux
          agent: claude-code
          orchestrator:
            agent: hermes
          agentConfig:
            permissions: default
            model: moonshotai/kimi-k2.5
          orchestratorRules: |
${indentYaml 12 orchestratorRules}

      reactions:
        ci-failed:
          auto: true
          action: send-to-agent
          retries: 2
        approved-and-green:
          auto: false
          action: notify
    '';

    configFile."agent-orchestrator/Dockerfile".text = ''
      FROM node:24-bookworm

      RUN apt-get update && apt-get install -y \
          git \
          tmux \
          gh \
          python3 \
          python3-pip \
          python3-venv \
          pipx \
          sqlite3 \
          && rm -rf /var/lib/apt/lists/*

      RUN npm install -g @aoagents/ao@0.2.5 @anthropic-ai/claude-code @playwright/mcp
      RUN npx playwright install --with-deps chromium

      # Patch ao-web Next.js bundle to load the local hermes agent plugin so it
      # is available in API route handlers (e.g. POST /api/sessions/:id/message).
      # Uses new Function() to escape webpack's import() transformation, which
      # cannot handle file:// URLs for plugins outside the bundle.
      RUN node -e " \
        const fs = require('fs'); \
        const f = '/usr/local/lib/node_modules/@aoagents/ao/node_modules/@aoagents/ao-web/.next/server/chunks/886.js'; \
        let d = fs.readFileSync(f, 'utf8'); \
        const needle = 'b.register(aE);let c='; \
        if (!d.includes(needle)) { console.log('ao-web patch: needle not found, skipping'); process.exit(0); } \
        const patch = 'b.register(aE);try{const __h=await (new Function(\"return import(\\\\x27file:///repos/agent-orchestrator/packages/plugins/agent-hermes/dist/index.js\\\\x27)\")());const __m=__h&&(__h.default||__h);if(__m&&__m.manifest&&__m.create)b.register(__m);}catch(__e){process.stderr.write(\"[ao-patch] hermes load failed: \"+__e.message+\"\\\\n\");}let c='; \
        d = d.replace(needle, patch); \
        fs.writeFileSync(f, d); \
        console.log('ao-web patch: hermes plugin import inserted'); \
      "

      # Patch 627.js restore() to pass systemPromptFile for orchestrators and
      # use AO_CALLER_TYPE='orchestrator' (instead of always 'agent').
      # This ensures that after a container restart, the orchestrator hermes
      # session gets the correct ephemeral system prompt via env var.
      RUN node -e " \
        const fs = require('fs'); \
        const f = '/usr/local/lib/node_modules/@aoagents/ao/node_modules/@aoagents/ao-web/.next/server/chunks/627.js'; \
        let d = fs.readFileSync(f, 'utf8'); \
        if (d.includes('\"orchestrator\"===m.role?\"orchestrator\":\"agent\"')) { \
          console.log('627.js restore patch: already applied, skipping'); \
          process.exit(0); \
        } \
        const n1 = 'issueId:n.issueId??void 0,permissions:\"orchestrator\"===m.role?\"permissionless\":m.permissions,model:m.model,subagent:m.subagent}'; \
        const r1 = 'issueId:n.issueId??void 0,permissions:\"orchestrator\"===m.role?\"permissionless\":m.permissions,model:m.model,subagent:m.subagent,...(\"orchestrator\"===m.role?{systemPromptFile:h(c).slice(0,-9)+\"/orchestrator-prompt-\"+a+\".md\"}:{})}';\
        if (!d.includes(n1)) { console.log('627.js restore patch: needle1 not found, skipping'); process.exit(0); } \
        d = d.replace(n1, r1); \
        const n2 = 'AO_SESSION:a,AO_DATA_DIR:j,AO_SESSION_NAME:a,...s&&{AO_TMUX_NAME:s},AO_CALLER_TYPE:\"agent\",...f&&{AO_PROJECT_ID:f}'; \
        const r2 = 'AO_SESSION:a,AO_DATA_DIR:j,AO_SESSION_NAME:a,...s&&{AO_TMUX_NAME:s},AO_CALLER_TYPE:\"orchestrator\"===m.role?\"orchestrator\":\"agent\",...f&&{AO_PROJECT_ID:f}'; \
        if (!d.includes(n2)) { console.log('627.js restore patch: needle2 not found, skipping'); process.exit(0); } \
        d = d.replace(n2, r2); \
        fs.writeFileSync(f, d); \
        console.log('627.js restore patch: applied'); \
      "

      # Patch ao-core session-manager.js restore() with same fixes as 627.js above.
      RUN node -e " \
        const fs = require('fs'); \
        const f = '/usr/local/lib/node_modules/@aoagents/ao/node_modules/@aoagents/ao-core/dist/session-manager.js'; \
        let d = fs.readFileSync(f, 'utf8'); \
        if (d.includes('role === \"orchestrator\" ? \"orchestrator\"')) { \
          console.log('session-manager restore patch: already applied, skipping'); \
          process.exit(0); \
        } \
        const n1 = '            issueId: session.issueId ?? undefined,\n            permissions: selection.role === \"orchestrator\" ? \"permissionless\" : selection.permissions,\n            model: selection.model,\n            subagent: selection.subagent,\n        };'; \
        const r1 = '            issueId: session.issueId ?? undefined,\n            permissions: selection.role === \"orchestrator\" ? \"permissionless\" : selection.permissions,\n            model: selection.model,\n            subagent: selection.subagent,\n            ...(selection.role === \"orchestrator\" ? { systemPromptFile: join(getProjectBaseDir(config.configPath, project.path), \`orchestrator-prompt-\''${sessionId}.md\`) } : {}),\n        };'; \
        if (!d.includes(n1)) { console.log('session-manager restore patch: needle1 not found, skipping'); process.exit(0); } \
        d = d.replace(n1, r1); \
        const n2 = '                AO_CALLER_TYPE: \"agent\",\n                ...(projectId && { AO_PROJECT_ID: projectId }),\n                AO_CONFIG_PATH: config.configPath,\n                ...(config.port !== undefined && config.port !== null && { AO_PORT: String(config.port) }),\n            },\n        });\n        // 9. Update metadata'; \
        const r2 = '                AO_CALLER_TYPE: selection.role === \"orchestrator\" ? \"orchestrator\" : \"agent\",\n                ...(projectId && { AO_PROJECT_ID: projectId }),\n                AO_CONFIG_PATH: config.configPath,\n                ...(config.port !== undefined && config.port !== null && { AO_PORT: String(config.port) }),\n            },\n        });\n        // 9. Update metadata'; \
        if (!d.includes(n2)) { console.log('session-manager restore patch: needle2 not found, skipping'); process.exit(0); } \
        d = d.replace(n2, r2); \
        fs.writeFileSync(f, d); \
        console.log('session-manager restore patch: applied'); \
      "

      # Install aider system-wide so non-root users can run it
      ENV PIPX_HOME=/opt/pipx
      ENV PIPX_BIN_DIR=/usr/local/bin
      RUN pipx install aider-chat

      # Create user matching host uid/gid so --dangerously-skip-permissions works
      # and files created in bind-mounted dirs have correct ownership.
      # UID/GID are passed as build args from ao-run.
      ARG HOST_UID=1000
      ARG HOST_GID=1000
      ARG HOST_USER=aouser
      RUN groupadd -g $HOST_GID $HOST_USER && \
          useradd -u $HOST_UID -g $HOST_GID -m -s /bin/bash $HOST_USER

      # Pre-bake onboarding state so interactive prompts are skipped
      RUN printf '{"hasCompletedOnboarding":true,"lastOnboardingVersion":"2.1.108","numStartups":1,"migrationVersion":11,"projects":{}}\n' \
          > /home/$HOST_USER/.claude.json && chown $HOST_USER:$HOST_USER /home/$HOST_USER/.claude.json

      # MCP servers available to all Claude Code agent sessions
      RUN mkdir -p /home/$HOST_USER/.claude && cat > /home/$HOST_USER/.claude/settings.json << 'MCPCFG'
      {
        "mcpServers": {
          "playwright": {
            "command": "npx",
            "args": ["@playwright/mcp", "--headless", "--browser", "chromium"]
          },
          "bcctl": {
            "command": "/repos/bcctl-mcp/.venv/bin/python",
            "args": ["-m", "bcctl_mcp.server"],
            "env": {
              "BENTOCLOUDCTL_BIN": "/repos/bentocloudctl/bin/bentocloudctl"
            }
          }
        }
      }
      MCPCFG
      RUN chown -R $HOST_USER:$HOST_USER /home/$HOST_USER/.claude

      # Bootstrap bcctl-mcp venv and inject into project .mcp.json
      RUN cat >> /usr/local/bin/setup-mcps << 'SETUPMCP'
      #!/bin/sh
      if [ -d /repos/bcctl-mcp ]; then
        # Recreate venv if missing or broken (e.g. host Python symlink)
        if [ ! -x /repos/bcctl-mcp/.venv/bin/python ] || \
           ! /repos/bcctl-mcp/.venv/bin/python -c "pass" 2>/dev/null; then
          rm -rf /repos/bcctl-mcp/.venv
          python3 -m venv /repos/bcctl-mcp/.venv
          /repos/bcctl-mcp/.venv/bin/pip install -e /repos/bcctl-mcp 2>/dev/null
        fi
      fi
      # Inject bcctl MCP into project .mcp.json (project-level overrides global settings)
      CWD="$(pwd)"
      if [ -f "$CWD/.mcp.json" ] && command -v node >/dev/null 2>&1; then
        node -e "\
          const fs = require('fs');\
          const f = process.argv[1];\
          let c = {};\
          try { c = JSON.parse(fs.readFileSync(f, 'utf8')); } catch(e) {}\
          if (!c.mcpServers) c.mcpServers = {};\
          if (!c.mcpServers.bcctl) {\
            c.mcpServers.bcctl = {\
              command: '/repos/bcctl-mcp/.venv/bin/python',\
              args: ['-m', 'bcctl_mcp.server'],\
              env: { BENTOCLOUDCTL_BIN: '/repos/bentocloudctl/bin/bentocloudctl' }\
            };\
            fs.writeFileSync(f, JSON.stringify(c, null, 2) + '\\n');\
          }\
        " "$CWD/.mcp.json" 2>/dev/null
      fi
      SETUPMCP
      RUN chmod +x /usr/local/bin/setup-mcps

      # Wrapper: auto-trust workspace + inject API key via apiKeyHelper.
      # Uses $HOME so it works for any user.
      RUN mv /usr/local/bin/claude /usr/local/bin/claude-real && \
          cat > /usr/local/bin/claude << 'WRAPPER'
      #!/bin/sh
      CWD="$(pwd)"
      STATE="$HOME/.claude.json"
      if [ -f "$STATE" ] && command -v node >/dev/null 2>&1; then
        node -e "\
          const fs = require('fs');\
          const s = JSON.parse(fs.readFileSync(process.argv[1],'utf8'));\
          if (!s.projects) s.projects = {};\
          const k = process.argv[2];\
          if (!s.projects[k]) s.projects[k] = {};\
          s.projects[k].hasTrustDialogAccepted = true;\
          fs.writeFileSync(process.argv[1], JSON.stringify(s, null, 2));\
        " "$STATE" "$CWD"
      fi
      # Bootstrap bcctl-mcp venv on first run (bind mount not available at build time)
      setup-mcps 2>/dev/null
      FILTERED="$*"
      if [ -n "$AO_ANTHROPIC_KEY" ]; then
        # Merge apiKeyHelper into global settings (preserves MCP server config)
        SFILE=$(mktemp /tmp/claude-settings.XXXXXX.json)
        node -e "\
          const fs = require('fs');\
          let s = {};\
          try { s = JSON.parse(fs.readFileSync(process.env.HOME + '/.claude/settings.json','utf8')); } catch(e) {}\
          s.apiKeyHelper = 'echo ' + process.argv[1];\
          fs.writeFileSync(process.argv[2], JSON.stringify(s));\
        " "$AO_ANTHROPIC_KEY" "$SFILE"
        eval exec claude-real --settings "$SFILE" "$FILTERED"
      fi
      eval exec claude-real "$FILTERED"
      WRAPPER
      RUN chmod +x /usr/local/bin/claude

      # System-level git config (applies to all users)
      RUN git config --system --add safe.directory '*' && \
          git config --system user.name "ao-agent" && \
          git config --system user.email "ao-agent@localhost"

      ENV PATH="/usr/local/bin:$PATH"

      USER $HOST_USER
      WORKDIR /work

      EXPOSE 3000

      ENTRYPOINT ["ao"]
    '';


    configFile."tridactyl/tridactylrc".text = ''
      " vim: set filetype=vim

      set smoothscroll true
      set newtab https://ko.ag/newtab.html

      unbind d
      bind <A-x> fillcmdline_notrail

      " J/K for tabs, x to close
      bind x tabclose

      " Detach tab to new window
      bind gd tabdetach

      " Only hint search results on Google/DDG
      bindurl www.google.com f hint -Jc #search a
      bindurl www.google.com F hint -Jbc #search a

      " Move hover URL to right so it doesn't overlap the command line
      guiset_quiet hoverlink right

      " Ignore Tridactyl on sites with their own keybindings
      autocmd DocStart mail.google.com mode ignore

      " Emacs bindings in insert mode
      bind --mode=insert <C-f> !s xdotool key Right
      bind --mode=insert <C-b> !s xdotool key Left
      bind --mode=insert <C-n> !s xdotool key Down
      bind --mode=insert <C-p> !s xdotool key Up
      bind --mode=insert <C-a> !s xdotool key Home
      bind --mode=insert <C-e> !s xdotool key End
      bind --mode=insert <C-d> !s xdotool key Delete
      bind --mode=insert <C-k> !s xdotool key shift+End Delete
      bind --mode=insert <C-w> !s xdotool key ctrl+BackSpace

      " C-g to cancel
      bind --mode=insert <C-g> composite unfocus | mode normal
      bind --mode=ex <C-g> ex.hide_and_clear

      " Emacs bindings in command line
      bind --mode=ex <C-f> ex.next_char
      bind --mode=ex <C-b> ex.prev_char
      bind --mode=ex <C-a> text.beginning_of_line
      bind --mode=ex <C-e> text.end_of_line
      bind --mode=ex <C-d> text.delete_char
      bind --mode=ex <C-k> text.kill_line
      bind --mode=ex <C-w> text.backward_kill_word
      bind --mode=ex <C-n> ex.next_completion
      bind --mode=ex <C-p> ex.prev_completion

      " External editor
      set editorcmd emacsclient -n

      " Wayland clipboard
      set externalclipboardcmd wl-copy
    '';
  };
}
