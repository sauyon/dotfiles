{ config, lib, ... }:

# gemini-cli was EOL'd and replaced by Google's Antigravity CLI (binary `agy`).
# We use the stock nixpkgs `antigravity-cli` package directly. The old
# gemini-cli override (npm-tarball version pin, bundle/-copy postInstall,
# nss_wrapper LD_PRELOAD shim) was all Node workarounds that don't apply to the
# Go-based antigravity binary.
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
      # No explicit `model.name`: "auto" is invalid (agy rejects it). Automatic
      # selection is handled by experimental.modelSteering below. Pin a value
      # from `agy models` here (e.g. "Claude Opus 4.6 (Thinking)") to force a
      # default instead.
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

  # agy treats settings.json as mutable state: picking a model or trusting a
  # workspace rewrites the file, atomically replacing a home-manager store
  # symlink with a regular file. Managing it as a symlink then either aborts the
  # next switch in checkLinkTargets ("would be clobbered") or, with
  # `force = true`, discards whatever agy recorded. Same bug is open upstream
  # for gemini-cli with no fix (nix-community/home-manager#8654).
  #
  # So: seed the file, don't manage it. The module still renders and type-checks
  # `settings` above; we suppress its symlink and install a writable copy only
  # when nothing is there. Once it exists, activation leaves it alone and agy
  # owns it.
  #
  # Deliberate trade-off: edits to `settings` above do NOT reach an existing
  # install. To re-seed after changing them, delete the file and switch:
  #   rm ~/.gemini/antigravity-cli/settings.json && hms
  home.file.".gemini/antigravity-cli/settings.json".enable = false;

  # entryAfter linkGeneration, not writeBoundary: linkGeneration removes the
  # previous generation's symlink at this path. Seeding earlier would write a
  # file only to have it cleaned up as an orphan link moments later.
  home.activation.antigravitySettingsSeed = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    target="$HOME/.gemini/antigravity-cli/settings.json"
    if [ ! -e "$target" ] && [ ! -L "$target" ]; then
      $DRY_RUN_CMD install -Dm600 \
        ${config.home.file.".gemini/antigravity-cli/settings.json".source} "$target"
    fi
  '';
}
