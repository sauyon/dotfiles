{ config, lib, ... }:

# Google's Antigravity CLI (binary `agy`), stock nixpkgs. Orca (orca.nix) knows
# the agent: its terminal tab launches `agy` from PATH (tui-agent-config's
# detectCmd), pre-writes exact-path workspace trust, and its Antigravity chat
# runs its own managed agy_acp_server.par — which reads the same
# ~/.gemini/antigravity-cli/settings.json this module seeds and refuses an
# API-key modelProvider, so the file must exist without one. The hook side
# (~/.orca/agent-hooks/antigravity-hook.sh, ~/.gemini/config/hooks.json) is
# Orca's own; nothing here touches it.
#
# agy 1.5.x flattened settings.json: the old nested keys (general.enableAutoUpdate,
# security.auth.selectedType, ui.footer, experimental.*) are gone -- agy rejects
# unknown settings at startup ("unknown setting %q"). The live schema is
# https://antigravity.google/schemas/v1/settings.json; what we set from it:
#   - agentMode "": default mode asks before edits; "auto" needs the terminal
#     sandbox and a non-business account, so it is not forced here.
#   - toolPermission "proceed-in-sandbox": commands run unasked inside the
#     terminal sandbox when it is on; irreversible actions still prompt.
#   - enableTerminalSandbox: the sandbox toolPermission leans on.
#   - editor "auto": $EDITOR, then vim. The host's EDITOR is emacs.
#   - verbosity "medium": grouped tool calls, commands and responses visible.
# No model pin: "auto" is invalid as a value and modelSteering is gone; pick
# with /model (saved by label into the writable file below).
{
  programs.antigravity-cli = {
    enable = true;
    # agy rewrites settings.json atomically (picking a model, trusting a
    # workspace), replacing a store symlink with a regular file, which the
    # next switch refuses to clobber. mutableSettings keeps the file ours:
    # home-manager merges the declared keys into the writable file at
    # activation (jq '$dynamic * $static') and leaves everything else --
    # trustedWorkspaces, model, gcp -- to agy.
    mutableSettings = true;
    settings = {
      agentMode = "";
      toolPermission = "proceed-in-sandbox";
      enableTerminalSandbox = true;
      editor = "auto";
      verbosity = "medium";
    };
  };
}
