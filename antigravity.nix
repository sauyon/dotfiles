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
      model = {
        name = "auto";
      };
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
}
