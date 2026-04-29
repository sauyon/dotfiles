{ config, lib, pkgs, ... }:

let
  # Secrets live in ./secrets.nix (gitignored Nix attrset).  At Nix eval time
  # we import it for non-sensitive values; the activation script reads secret
  # values at *runtime* via `nix eval --file` so they never enter /nix/store.
  secretsDir = builtins.toString ./.;
  secrets =
    if builtins.pathExists ./secrets.nix
    then import ./secrets.nix
    else { apiKey = "REPLACE_ME"; gatewayToken = "REPLACE_ME"; aoApiKey = "REPLACE_ME"; rampartToken = "REPLACE_ME"; discordToken = "REPLACE_ME"; discordUserId = "REPLACE_ME"; discordHomeChannel = "REPLACE_ME"; };

  stateDir = "${config.xdg.dataHome}/hermes";
  pluginDir = "${stateDir}/plugins";
  relPluginDir = ".local/share/hermes/plugins/rampart";
  relAgentguardPluginDir = ".local/share/hermes/plugins/agentguard";

  hermes-agent = (builtins.getFlake "github:sauyon/hermes-agent").packages.${pkgs.system}.default;
  ao-mcp-pkg = (builtins.getFlake "github:sauyon/ao-mcp").packages.${pkgs.system}.default;
  ao-mcp-server = "${ao-mcp-pkg}/bin/ao-mcp-server";
  rampart = import ./rampart-patched.nix { inherit pkgs; };
  agentguard = (builtins.getFlake "github:sauyon/agentguard").packages.${pkgs.system}.default;

  rampartVerifyUpstream = pkgs.fetchFromGitHub {
    owner = "peg";
    repo = "rampart-verify";
    rev = "7443934";
    hash = "sha256-ZeX44Hpei+/XzZzUsPH6CRrLcRO5SRFvIIxlhwSmkOs=";
  };

  # Patch: use OpenAIProvider when OPENAI_BASE_URL is set (upstream only checks for "gpt")
  rampartVerifySrc = pkgs.runCommand "rampart-verify-patched" {} ''
    cp -r ${rampartVerifyUpstream} $out
    chmod -R u+w $out
    sed -i 's/"gpt" in model.lower()/"gpt" in model.lower() or os.getenv("OPENAI_BASE_URL")/' $out/providers.py
    # Strengthen parse_llm_response: check entire response for decision keywords
    sed -i 's/# Unclear response — fail open\./# Check entire response for decision keywords\n    full_upper = response.upper()\n    if "DENY" in full_upper:\n        return "deny", "Action denied by security review"\n    if "ALLOW" in full_upper:\n        return "allow", None/' $out/server.py
  '';
in
{
  # ── Rampart plugin for Hermes ────────────────────────────────────────────
  # Uses the pre_tool_call blocking hook (action: "block") to enforce
  # rampart policy decisions via the preflight API.
  home.file."${relPluginDir}/__init__.py".text = builtins.readFile ./hermes-rampart-plugin.py;

  home.file."${relPluginDir}/plugin.yaml".text = builtins.toJSON {
    name = "rampart";
    version = "0.9.15";
    description = "Rampart AI agent firewall — policy enforcement via preflight API";
    author = "sauyon";
    hooks = [ "pre_tool_call" ];
  };

  # ── AgentGuard plugin for Hermes ─────────────────────────────────────────
  # Uses httpx (already a hermes dependency) to call AgentGuard's REST API
  # on the host via the agentguard-patrol service, rather than shelling out
  # to the Node binary (which isn't available inside the container).
  home.file."${relAgentguardPluginDir}/__init__.py".text = ''
    """AgentGuard security plugin for Hermes Agent.

    Pattern-based security checks for common dangerous operations.
    Complements Rampart's policy engine with additional heuristics.
    """
    import logging
    import os
    import re

    logger = logging.getLogger(__name__)

    _LEVEL = os.environ.get("AGENTGUARD_LEVEL", "strict")

    # Patterns that should always be blocked regardless of level
    _DENY_PATTERNS = [
        re.compile(r"rm\s+(-\w+\s+)*\s*/(?!tmp)"),      # rm -rf / (except /tmp)
        re.compile(r"mkfs\b[.\w]*"),                        # format disk (mkfs, mkfs.ext4, etc)
        re.compile(r"dd\s+.*of\s*=\s*/dev/"),             # overwrite device
        re.compile(r"chmod\s+0?777\s+/"),                 # open permissions on root
        re.compile(r"curl\b.*\|\s*(?:ba)?sh"),            # pipe to shell
        re.compile(r"wget\b.*\|\s*(?:ba)?sh"),            # pipe to shell
        re.compile(r"eval\s*\(.*base64"),                 # eval base64
        re.compile(r"\.ssh/authorized_keys"),             # SSH key injection
        re.compile(r"/etc/(?:passwd|shadow|sudoers)"),    # sensitive system files
        re.compile(r"iptables\s+.*-j\s+DROP"),            # firewall manipulation
    ]

    # Additional patterns for strict mode
    _STRICT_PATTERNS = [
        re.compile(r"nc\s+(-[elp]+\s+)?.*\d{2,5}"),     # netcat listeners
        re.compile(r"nmap\b"),                            # port scanning
        re.compile(r"tcpdump\b"),                         # packet capture
        re.compile(r"strace\b"),                          # process tracing
        re.compile(r"env\s+.*=.*\bsh\b"),                 # env-based shell escape
    ]

    _TOOL_MAP = {
        "terminal": "exec",
        "read_file": "read",
        "write_file": "write",
        "patch": "write",
    }


    def _check(tool_name, args):
        tool_type = _TOOL_MAP.get(tool_name, tool_name)
        if tool_type != "exec":
            return None

        command = args.get("command", str(args))

        for pattern in _DENY_PATTERNS:
            if pattern.search(command):
                return {"action": "block", "message": f"AgentGuard: blocked by security pattern ({pattern.pattern})"}

        if _LEVEL == "strict":
            for pattern in _STRICT_PATTERNS:
                if pattern.search(command):
                    return {"action": "block", "message": f"AgentGuard: blocked by strict pattern ({pattern.pattern})"}

        return None


    def _on_pre_tool_call(tool_name, args, **kwargs):
        return _check(tool_name, args)


    def register(ctx):
        ctx.register_hook("pre_tool_call", _on_pre_tool_call)
        logger.info("AgentGuard plugin registered (level=%s)", _LEVEL)
  '';

  home.file."${relAgentguardPluginDir}/plugin.yaml".text = builtins.toJSON {
    name = "agentguard";
    version = "1.0.14";
    description = "GoPlus AgentGuard — AI agent security plugin";
    author = "sauyon";
    hooks = [ "pre_tool_call" ];
  };

  # ── Hermes gateway (API server for orchestrator→user communication) ──────
  # Runs on agent-net so the AO orchestrator can POST questions to hermes-gw:8642
  systemd.user.services.hermes-gateway = {
    Unit = {
      Description = "Hermes Agent gateway — API server for orchestrator relay";
      After = [ "network.target" "litellm.service" ];
      Requires = [ "litellm.service" ];
    };
    Service = {
      ExecStartPre = [
        "-${pkgs.docker}/bin/docker rm -f hermes-gw"
        # Copy managed config into volume so hermes can rename temp files onto it
        # (bind-mounting a single file creates a mount point; rename onto it fails with EBUSY)
        "${pkgs.coreutils}/bin/cp -f ${config.xdg.configHome}/hermes-gateway/config.yaml ${config.home.homeDirectory}/.local/share/hermes-gateway/config.yaml"
      ];
      ExecStart = builtins.concatStringsSep " " [
        "${pkgs.docker}/bin/docker run --rm"
        "--name hermes-gw"
        "--network agent-net"
        "-p 8642:8642"
        "-v /nix/store:/nix/store:ro"
        "-v ${config.home.homeDirectory}/devel:/repos"
        "-v ${pluginDir}:/plugins:ro"
        "-v ${config.home.homeDirectory}/.local/share/hermes-gateway:/root/.hermes"
        "-v ${config.home.homeDirectory}/.config/gh:/root/.config/gh:ro"
        "-v ${config.home.homeDirectory}/.rampart/remote-token:/run/secrets/rampart-token:ro"
        "-e HERMES_PLUGINS=/plugins"
        "-e RAMPART_URL=https://REDACTED"
        "-e RAMPART_FAIL_OPEN=false"
        "-e AGENTGUARD_LEVEL=strict"
        "-e DISCORD_HOME_CHANNEL=1493822526764220599"
        "-v ${config.home.homeDirectory}/.hermes-gateway-token:/run/secrets/gateway-token:ro"
        "-e API_SERVER_HOST=0.0.0.0"
        "--entrypoint sh"
        "debian:bookworm-slim"
        "-c" "'export RAMPART_TOKEN=$(cat /run/secrets/rampart-token) API_SERVER_KEY=$(cat /run/secrets/gateway-token); exec ${hermes-agent}/bin/hermes gateway run'"
      ];
      ExecStop = "${pkgs.docker}/bin/docker stop hermes-gw";
      Restart = "on-failure";
      RestartSec = 5;
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  # ── AgentGuard patrol timer (every 6 hours) ──────────────────────────────
  # Runs in a container (agentguard is a Node.js package, not available on host).
  systemd.user.services.agentguard-patrol = {
    Unit = {
      Description = "AgentGuard patrol checks — scan skills and audit logs";
    };
    Service = {
      Type = "oneshot";
      ExecStartPre = [
        "-${pkgs.docker}/bin/docker rm -f agentguard-patrol"
      ];
      ExecStart = builtins.concatStringsSep " " [
        "${pkgs.docker}/bin/docker run --rm"
        "--name agentguard-patrol"
        "--network agent-net"
        "-v /nix/store:/nix/store:ro"
        "-v ${config.xdg.dataHome}/agentguard:/data"
        "-e AGENTGUARD_HOME=/data"
        "--entrypoint ${agentguard}/bin/agentguard"
        "debian:bookworm-slim"
        "patrol" "--level" "strict"
      ];
    };
  };

  systemd.user.timers.agentguard-patrol = {
    Unit = {
      Description = "Run AgentGuard patrol checks every 6 hours";
    };
    Timer = {
      OnCalendar = "*-*-* 0/6:00:00";
      Persistent = true;
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };

  # ── Token files written via activation (not home.file) for chmod 600 ─────

  # ── Hermes gateway config (Discord + API server) ─────────────────────────
  # ── LiteLLM proxy config ─────────────────────────────────────────────────
  # Both written via activation (not xdg.configFile) because they contain
  # secrets — xdg.configFile symlinks into the world-readable nix store.

  systemd.user.services.litellm = {
    Unit = {
      Description = "LiteLLM Anthropic→OpenAI translation proxy (Docker)";
      After = [ "network.target" ];
    };
    Service = {
      ExecStartPre = [
        "-${pkgs.docker}/bin/docker rm -f litellm"
        "-${pkgs.docker}/bin/docker network create agent-net"
      ];
      ExecStart = builtins.concatStringsSep " " [
        "${pkgs.docker}/bin/docker run --rm"
        "--name litellm"
        "--network agent-net"
        "-v ${config.xdg.configHome}/litellm/config.yaml:/app/config.yaml:ro"
        "-p 4000:4000"
        "litellm/litellm"
        "--config /app/config.yaml"
        "--port 4000"
      ];
      ExecStop = "${pkgs.docker}/bin/docker stop litellm";
      Restart = "on-failure";
      RestartSec = 5;
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  # ── Permissions & secret config files ──────────────────────────────────────
  # The activation script reads secrets at RUNTIME via `nix eval --file` so
  # that no secret values are baked into the /nix/store activation script.
  # secrets.nix remains the Nix-native source of truth.
  home.activation.hermesPermissions = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    chmod 700 "${stateDir}" 2>/dev/null || true

    _FNOX_TOML="${secretsDir}/fnox.toml"
    if [ ! -f "$_FNOX_TOML" ]; then
      echo "hermes: $_FNOX_TOML not found — skipping secret provisioning."
    else
      # Fetch a single secret from Bitwarden via fnox at runtime
      _s() {
        if command -v fnox >/dev/null 2>&1; then
          fnox -c "$_FNOX_TOML" get "$1" 2>/dev/null || echo "REPLACE_ME"
        elif command -v mise >/dev/null 2>&1; then
          mise x fnox -- fnox -c "$_FNOX_TOML" get "$1" 2>/dev/null || echo "REPLACE_ME"
        else
          echo "REPLACE_ME"
        fi
      }

      umask 077

      # Individual secret files (mounted into containers at runtime)
      mkdir -p "${config.xdg.configHome}/hermes/secrets"
      _s apiKey       > "${config.xdg.configHome}/hermes/secrets/api-key"
      _s discordToken > "${config.xdg.configHome}/hermes/secrets/discord-token"

      mkdir -p "${config.home.homeDirectory}/.rampart"
      _s rampartToken > "${config.home.homeDirectory}/.rampart/remote-token"
      _s gatewayToken > "${config.home.homeDirectory}/.hermes-gateway-token"

      # Hermes orchestrator config (for the hermes agent running inside the AO container)
      mkdir -p "${config.home.homeDirectory}/.hermes-orchestrator"
      cat > "${config.home.homeDirectory}/.hermes-orchestrator/config.yaml" << HERMESORCCFGEOF
    model:
      provider: modular
      default: moonshotai/kimi-k2.5

    providers:
      modular:
        name: Modular
        api: https://deepseek.api.modular.com/v1
        api_key: $(_s apiKey)

    display:
      tool_progress: off
      show_reasoning: false
    HERMESORCCFGEOF

      # Hermes gateway config (secrets filled via nix eval, not Nix store)
      mkdir -p "${config.xdg.configHome}/hermes-gateway"
      cat > "${config.xdg.configHome}/hermes-gateway/config.yaml" << HERMESCFGEOF
    platforms:
      discord:
        enabled: true
        token: $(_s discordToken)
        respond_to_dms: true
        respond_to_mentions: true
        respond_to_all: false
        owner_id: $(_s discordUserId)
        home_channel:
          platform: discord
          chat_id: "$(_s discordHomeChannel)"
      api_server:
        enabled: true
        host: "0.0.0.0"
        port: 8642
        key: $(_s gatewayToken)

    model:
      provider: modular
      default: moonshotai/kimi-k2.5

    providers:
      modular:
        name: Modular
        api: https://deepseek.api.modular.com/v1
        api_key: $(_s apiKey)

    auxiliary:
      compression:
        provider: modular
        model: moonshotai/kimi-k2.5

    agent:
      system_prompt: >
        You are the user's personal AI assistant on Discord, powered by Hermes.
        You can help with general questions, research, coding, and conversation.
        Use your memory and skills systems to learn the user's preferences over time.

        You also serve as the communication bridge for the AO agent orchestrator.
        When the orchestrator sends a message via the API (these arrive as system
        or API-originated messages, not from Discord), present the orchestrator's
        question or update to the user in a clear, contextual way. Collect the
        user's response and return it. You may add context or clarify ambiguity
        in either direction — you are not a dumb pipe. If the orchestrator's
        question is unclear, ask it to clarify before bothering the user.

        Prioritize brevity in Discord messages. Use threads for longer discussions.

    display:
      tool_progress: off

    mcp_servers:
      ao:
        command: "${ao-mcp-server}"
        env:
          AGENT_ORCHESTRATOR_BASE_URL: "http://ao:3000"
          AGENT_ORCHESTRATOR_API_KEY: "local"
          SKIP_HEALTH_CHECKS: "true"
      browser-rendering:
        command: "npx"
        args:
          - "-y"
          - "chrome-devtools-mcp@latest"
          - "--wsEndpoint=wss://api.cloudflare.com/client/v4/accounts/$(_s cloudflareAccountId)/browser-rendering/devtools/browser?keep_alive=600000"
          - "--wsHeaders={\"Authorization\":\"Bearer $(_s cloudflareToken)\"}"
    HERMESCFGEOF

      # LiteLLM config (contains API key)
      mkdir -p "${config.xdg.configHome}/litellm"
      cat > "${config.xdg.configHome}/litellm/config.yaml" << LITELLMCFGEOF
    litellm_settings:
      drop_params: true

    model_list:
      - model_name: moonshotai/kimi-k2.5
        litellm_params:
          model: openai/moonshotai/kimi-k2.5
          api_base: https://deepseek.api.modular.com/v1
          api_key: $(_s apiKey)
    LITELLMCFGEOF
    fi
  '';
}
