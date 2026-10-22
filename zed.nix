{ config, lib, pkgs, machine, nixgl, system, ... }:

let
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  isDesktop = machine.gui or true;

  # Same key opencode's and pi's `modular` providers read. Read at LAUNCH by the
  # wrapper below, never at build or activation, so it reaches neither the nix
  # store nor settings.json (which Zed rewrites freely).
  keyFile = "${config.home.homeDirectory}/.config/local-auto-mode/api-key";

  # Zed renders through Vulkan (blade), not GL, so config.lib.nixGL.wrap alone
  # is not enough: it adds the Vulkan half only when
  # targets.genericLinux.nixGL.vulkan.enable is on, and that switch is global --
  # it would rewrap ghostty, hyprlock and every other GL app here too. So the
  # Vulkan wrapper is applied to Zed alone. nixVulkanIntel is "nixGL + mesa
  # vulkan ICDs", which covers radv as well, the same way nixGLIntel covers
  # radeonsi; home.nix refuses any gpu other than mesa, so this cannot diverge.
  #
  # Zed reads an openai_compatible provider's key from `<ID>_API_KEY`, the
  # provider id upper-snake-cased, so the provider named `Modular` below takes
  # MODULAR_API_KEY. Without it the provider shows in the agent panel as
  # unauthenticated rather than failing at launch.
  zed = pkgs.symlinkJoin {
    name = "zed-editor-${pkgs.zed-editor.version}-nixgl";
    # graftNode: zed runs node for LSPs; CVE-2026-111497, see home.nix overlay.
    paths = [ (pkgs.graftNode pkgs.zed-editor) ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      rm $out/bin/zeditor
      makeWrapper ${nixgl.packages.${system}.nixVulkanIntel}/bin/nixVulkanIntel $out/bin/zeditor \
        --add-flags ${nixgl.packages.${system}.nixGLIntel}/bin/nixGLIntel \
        --add-flags ${pkgs.graftNode pkgs.zed-editor}/bin/zeditor \
        --run 'if [ -r "${keyFile}" ]; then MODULAR_API_KEY="$(cat "${keyFile}")"; export MODULAR_API_KEY; fi'
    '';
    meta.mainProgram = "zeditor";
  };
in
{
  programs.zed-editor = lib.mkIf (!isDarwin && isDesktop) {
    enable = true;
    package = zed;
    # Zed writes to settings.json itself (theme picks, model switches from the
    # agent panel). Mutable means home-manager merges these keys over the file at
    # each switch rather than owning it; a key set here wins, others survive.
    mutableUserSettings = true;
    userSettings = {
      language_models.openai_compatible.Modular = {
        # Same deployment as opencode's and pi's `modular` provider: the public
        # one carrying the GLM builds, not opencode's internal `mcloud`.
        api_url = "https://api.modular.com/v1";
        available_models = [
          {
            name = "zai-org/glm-5.3";
            display_name = "GLM 5.3 (Modular)";
            # The server's own limit, probed for pi.nix: 192000, not the 1M the
            # model is published with.
            max_tokens = 192000;
            max_output_tokens = 65536;
            # interleaved_reasoning stays off (the default): Zed reads thinking
            # only from `reasoning_content`, and this deployment sends it in
            # `reasoning` with `reasoning_content: null` (measured 2026-10-17).
            # So Zed shows no thinking for GLM here and has none to send back;
            # the answers and tool calls themselves are unaffected.
            capabilities = {
              tools = true;
              images = false;
              parallel_tool_calls = false;
              prompt_cache_key = false;
            };
          }
        ];
      };
      agent.default_model = {
        provider = "Modular";
        model = "zai-org/glm-5.3";
      };
    };
  };
}
