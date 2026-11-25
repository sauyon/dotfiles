{ lib, pkgs, isCursorHost, ... }:

# Official Google Workspace CLI (`gwscli`), distributed by Google as a prebuilt
# static binary (Linux) / signed Mach-O binary (macOS) at dl.google.com/workspace-cli.
# Fronts Google's official Workspace MCP services (gmailmcp, drivemcp,
# calendarmcp, docsmcp, sheetsmcp, slidesmcp, chatmcp, tasksmcp, people) and
# ships packaged agent skills extracted at build time via `gwscli skills install`.
#
# One-time auth is manual and per host (creds live outside nix):
#   1. Create a Desktop OAuth client (Personal Mode shortcut:
#      https://console.cloud.google.com/auth/overview/personal?apis=ws,wsmcp,tasksmcp&clientType=desktop&isAgent=true)
#      and download client_secret.json.
#   2. Register it: `gwscli auth setup --oauth_client_file=/path/to/client_secret.json`
#   3. Authorize scopes per service: `gwscli auth login gmail`, `gwscli auth login drive`, ...
#      On headless hosts, forward the dynamic localhost callback port over SSH.
#
# Note: Google's download URLs are unversioned; bumping means refreshing
# `version` (from `gwscli version`) and the four platform hashes below.
let
  version = "1.0.6.1026726559";

  platformSpec =
    {
      x86_64-linux = {
        dir = "linux";
        hash = "sha256-RDpJBcUh3xhISAX+bnicAqv3lDW0rI/qMhnrOjEVsdU=";
      };
      aarch64-linux = {
        dir = "linux_arm64";
        hash = "sha256-vNcEIQEwomllSaZsjfh5mzfOLqi5aNXWOc6BM5xUKYU=";
      };
      x86_64-darwin = {
        dir = "macos_intel";
        hash = "sha256-on2hou2GQ/Pz5pX6cc9lyMi76dVVLKntCwx+qx1vi74=";
      };
      aarch64-darwin = {
        dir = "macos_silicon";
        hash = "sha256-Cpxu6ng6LJEaV2dywCJIhIpfRGv8dztouaUoIklfjEE=";
      };
    }.${pkgs.stdenvNoCC.hostPlatform.system}
      or (throw "gwscli: unsupported system ${pkgs.stdenvNoCC.hostPlatform.system}");

  gwscli = pkgs.stdenvNoCC.mkDerivation {
    pname = "gwscli";
    inherit version;

    src = pkgs.fetchurl {
      name = "gwscli-${platformSpec.dir}-${version}";
      url = "https://dl.google.com/workspace-cli/${platformSpec.dir}/gwscli";
      hash = platformSpec.hash;
    };

    dontUnpack = true;
    dontConfigure = true;
    dontBuild = true;
    # Static ELF on Linux; signed Mach-O on Darwin (stripping invalidates
    # Apple Silicon code signatures).
    dontStrip = true;
    dontPatchELF = true;

    installPhase = ''
      runHook preInstall

      install -Dm755 "$src" "$out/bin/gwscli"
      mkdir -p "$out/share/gwscli/skills"
      HOME="$TMPDIR" "$out/bin/gwscli" skills install --dir "$out/share/gwscli/skills"

      runHook postInstall
    '';

    doInstallCheck = true;
    installCheckPhase = ''
      runHook preInstallCheck

      got_version=$(HOME="$TMPDIR" "$out/bin/gwscli" version)
      echo "$got_version"
      case "$got_version" in
        *"${version}"*) ;;
        *)
          echo "gwscli version mismatch: got '$got_version', want '${version}'" >&2
          exit 1
          ;;
      esac
      test -s "$out/share/gwscli/skills/gwscli/SKILL.md"

      runHook postInstallCheck
    '';

    meta = {
      description = "Official Google Workspace CLI for terminal and AI agent workflows";
      homepage = "https://dl.google.com/workspace-cli/README.md";
      license = lib.licenses.unfree;
      mainProgram = "gwscli";
      platforms = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
    };
  };

  # Same global agent skill roots as orca.nix:
  #   - .agents/skills: shared root (Codex, OMP, Kimi, etc.)
  #   - .claude/skills: Claude Code
  #   - .gemini/config/skills: Antigravity (`agy`) and Orca native Antigravity chat
  #   - .cursor/skills: Cursor, gated on isCursorHost (tests/agent-clis.sh)
  skillDirs = [
    ".agents/skills"
    ".claude/skills"
    ".gemini/config/skills"
  ] ++ lib.optional isCursorHost ".cursor/skills";
in
{
  home.packages = [ gwscli ];

  home.file = lib.listToAttrs (map (dir: {
    name = "${dir}/gwscli";
    value.source = "${gwscli}/share/gwscli/skills/gwscli";
  }) skillDirs);
}
