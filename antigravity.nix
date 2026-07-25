{ config, lib, ... }:

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
  # a workspace rewrites the file, atomically replacing a home-manager store
  # symlink with a regular file. Managing it as a symlink therefore either
  # aborts the next switch in checkLinkTargets ("would be clobbered") or, with
  # `force = true`, silently throws away whatever agy recorded. Upstream has the
  # same bug open for gemini-cli with no fix (nix-community/home-manager#8654).
  #
  # So: seed the file, don't manage it. The module still renders and type-checks
  # `settings` above, but we suppress its symlink and install a writable copy
  # only when nothing is there. Once the file exists, activation leaves it
  # alone forever and agy owns it.
  #
  # Trade-off, deliberately chosen: edits to `settings` above do NOT reach an
  # existing install. To re-seed after changing them, delete the file and
  # switch:
  #   rm ~/.gemini/antigravity-cli/settings.json && hms
  home.file.".gemini/antigravity-cli/settings.json".enable = false;

  # entryAfter linkGeneration, not writeBoundary: linkGeneration is what removes
  # the previous generation's symlink at this path. Seeding before that would
  # write a file only to have it cleaned up as an orphan link moments later.
  home.activation.antigravitySettingsSeed = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    target="$HOME/.gemini/antigravity-cli/settings.json"
    if [ ! -e "$target" ] && [ ! -L "$target" ]; then
      $DRY_RUN_CMD install -Dm600 \
        ${config.home.file.".gemini/antigravity-cli/settings.json".source} "$target"
    fi
  '';
}
