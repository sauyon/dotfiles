{ config, lib, pkgs, hermes-agent, ao-mcp, agentguard, system, ... }:

let

  stateDir = "${config.xdg.dataHome}/hermes";
  pluginDir = "${stateDir}/plugins";
  relPluginDir = ".local/share/hermes/plugins/rampart";
  relAgentguardPluginDir = ".local/share/hermes/plugins/agentguard";

  hermes-agent-pkg = hermes-agent.packages.${system}.default;
  ao-mcp-pkg = ao-mcp.packages.${system}.default;
  ao-mcp-server = "${ao-mcp-pkg}/bin/ao-mcp-server";
  agentguard-pkg = agentguard.packages.${system}.default;
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
  /* DISABLED
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
        "-v ${config.sops.secrets.rampartToken.path}:/run/secrets/rampart-token:ro"
        "-v ${config.sops.secrets.rampartUrl.path}:/run/secrets/rampart-url:ro"
        "-v ${config.sops.secrets.gatewayToken.path}:/run/secrets/gateway-token:ro"
        "-e HERMES_PLUGINS=/plugins"
        "-e RAMPART_FAIL_OPEN=false"
        "-e AGENTGUARD_LEVEL=strict"
        "-e API_SERVER_HOST=0.0.0.0"
        "--entrypoint sh"
        "debian:bookworm-slim"
        "-c" "'export RAMPART_TOKEN=$(cat /run/secrets/rampart-token) RAMPART_URL=$(cat /run/secrets/rampart-url) API_SERVER_KEY=$(cat /run/secrets/gateway-token); exec ${hermes-agent-pkg}/bin/hermes gateway run'"
      ];
      ExecStop = "${pkgs.docker}/bin/docker stop hermes-gw";
      Restart = "on-failure";
      RestartSec = 5;
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
  */

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
        "--entrypoint ${agentguard-pkg}/bin/agentguard"
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

  /* DISABLED
  systemd.user.services.litellm = {
    Unit = {
      Description = "LiteLLM Anthropic→OpenAI translation proxy (Docker)";
      After = [ "network.target" "sops-nix.service" ];
      Requires = [ "sops-nix.service" ];
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
  */

  # ── sops-nix secret management ──────────────────────────────────────────────
  sops.defaultSopsFile = ./secrets.yaml;
  sops.gnupg.sshKeyPaths = [];
  sops.age.sshKeyPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];

  # File-path secrets: written to specific locations for direct use
  sops.secrets.apiKey = { path = "${config.xdg.configHome}/hermes/secrets/api-key"; mode = "0600"; };
  sops.secrets.discordToken = { path = "${config.xdg.configHome}/hermes/secrets/discord-token"; mode = "0600"; };
  sops.secrets.gatewayToken = { path = "${config.home.homeDirectory}/.hermes-gateway-token"; mode = "0600"; };
  sops.secrets.discordUserId = {};
  sops.secrets.discordHomeChannel = {};
  sops.secrets.cloudflareAccountId = {};
  sops.secrets.cloudflareToken = {};
  sops.secrets.aoApiKey = {};

  # ── sops-managed config files (contain secrets, never touch nix store) ─────
  /* DISABLED
  sops.templates."hermes-orchestrator-config" = {
    path = "${config.home.homeDirectory}/.hermes-orchestrator/config.yaml";
    mode = "0600";
    content = ''
      model:
        provider: modular
        default: moonshotai/kimi-k2.5

      providers:
        modular:
          name: Modular
          api: https://deepseek.api.modular.com/v1
          api_key: ${config.sops.placeholder.apiKey}

      display:
        tool_progress: off
        show_reasoning: false
    '';
  };

  sops.templates."hermes-gateway-config" = {
    path = "${config.xdg.configHome}/hermes-gateway/config.yaml";
    mode = "0600";
    content = ''
      platforms:
        discord:
          enabled: true
          token: ${config.sops.placeholder.discordToken}
          respond_to_dms: true
          respond_to_mentions: true
          respond_to_all: false
          owner_id: ${config.sops.placeholder.discordUserId}
          home_channel:
            platform: discord
            chat_id: "${config.sops.placeholder.discordHomeChannel}"
        api_server:
          enabled: true
          host: "0.0.0.0"
          port: 8642
          key: ${config.sops.placeholder.gatewayToken}

      model:
        provider: modular
        default: moonshotai/kimi-k2.5

      providers:
        modular:
          name: Modular
          api: https://deepseek.api.modular.com/v1
          api_key: ${config.sops.placeholder.apiKey}

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
            - "--wsEndpoint=wss://api.cloudflare.com/client/v4/accounts/${config.sops.placeholder.cloudflareAccountId}/browser-rendering/devtools/browser?keep_alive=600000"
            - "--wsHeaders={\"Authorization\":\"Bearer ${config.sops.placeholder.cloudflareToken}\"}"
    '';
  };

  sops.templates."litellm-config" = {
    path = "${config.xdg.configHome}/litellm/config.yaml";
    mode = "0600";
    content = ''
      litellm_settings:
        drop_params: true

      model_list:
        - model_name: moonshotai/kimi-k2.5
          litellm_params:
            model: openai/moonshotai/kimi-k2.5
            api_base: https://deepseek.api.modular.com/v1
            api_key: ${config.sops.placeholder.apiKey}
    '';
  };
  */

  home.activation.hermesPermissions = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    chmod 700 "${stateDir}" 2>/dev/null || true
  '';
}
