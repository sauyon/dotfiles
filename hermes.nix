{ config, lib, pkgs, hermes-agent, ao-mcp, system, ... }:

if builtins.match ".*-darwin" system != null then {} else

let

  stateDir = "${config.xdg.dataHome}/hermes";
  pluginDir = "${stateDir}/plugins";
  relPluginDir = ".local/share/hermes/plugins/rampart";

  hermes-agent-pkg = hermes-agent.packages.${system}.default;
  ao-mcp-pkg = ao-mcp.packages.${system}.default;
  ao-mcp-server = "${ao-mcp-pkg}/bin/ao-mcp-server";
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
