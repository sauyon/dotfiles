{ ... }:

# gemini-cli was EOL'd and replaced by Google's Antigravity CLI (binary `agy`).
# We now use the stock nixpkgs `antigravity-cli` package (the
# programs.antigravity-cli default) directly. The old gemini-cli override is
# gone: the npm-tarball version pin, the bundle/-copy postInstall, and the
# nss_wrapper LD_PRELOAD shim were all gemini-cli (Node) workarounds that don't
# apply to the Go-based antigravity binary.
{
  programs.antigravity-cli = {
    enable = true;
    settings = {
      general = {
        enableAutoUpdate = false;
        enableAutoUpdateNotification = false;
      };
      security = {
        auth = {
          selectedType = "oauth-personal";
        };
        enablePermanentToolApproval = true;
        autoAddToPolicyByDefault = true;
        environmentVariableRedaction = {
          enabled = true;
        };
      };
      ui = {
        footer = {
          items = [ "workspace" "git-branch" "sandbox" "model-name" "quota" ];
        };
        showCitations = true;
      };
      # No explicit `model.name`: "auto" is not a valid value (agy rejects it as
      # invalid settings). Automatic model selection is handled by
      # experimental.modelSteering below. Pin a value from `agy models` here
      # (e.g. "Claude Opus 4.6 (Thinking)") to force a default instead.
      experimental = {
        worktrees = true;
        memoryManager = true;
        contextManagement = true;
        generalistProfile = true;
        autoMemory = true;
        modelSteering = true;
      };
    };
  };

  # agy treats its settings.json as mutable state: picking a model or trusting
  # a workspace makes it write the file, which atomically replaces the
  # home-manager store symlink with a regular file. The next switch then aborts
  # in checkLinkTargets ("would be clobbered") and takes every other activation
  # step down with it. Let nix win instead — same posture as the per-profile
  # Claude settings.json files in home.nix.
  #
  # Consequence: agy's runtime writes to this file are transient and revert on
  # each switch, so it re-prompts to trust a workspace after `hms`. Reverting
  # `model` is if anything desirable — see the modelSteering note above.
  # Anything worth keeping belongs in `settings` above, declared.
  home.file.".gemini/antigravity-cli/settings.json".force = true;
}
