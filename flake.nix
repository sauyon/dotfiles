{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    walker.url = "github:abenz1267/walker";
    nixgl.url = "github:guibou/nixGL";
    agent-orchestrator.url = "github:sauyon/agent-orchestrator";
    ao-mcp.url = "github:sauyon/ao-mcp";
    explore-mcp.url = "github:sauyon/explore-mcp";
    explore-mcp.inputs.nixpkgs.follows = "nixpkgs";
    # drovr — CLI for single-writer/compressed-handoff agent phases; ships the
    # `drovr` binary plus drovr:* Claude skills under share/drovr/.
    drovr.url = "git+https://forge.ko.ag/drovr/drovr.git";
    drovr.inputs.nixpkgs.follows = "nixpkgs";
    # Terminal diff viewer for agent changesets; ships a `hunk-review` Claude
    # skill under `${hunk}/skills/`.
    hunk.url = "github:modem-dev/hunk";
    # Seamless OIDC SSH gate (gate binary + nixos/darwin modules).
    ssh-oidc.url = "git+https://codeberg.org/sauyon/ssh-oidc";
  };

  outputs = { nixpkgs, home-manager, nix-darwin, sops-nix, walker, nixgl, agent-orchestrator, ao-mcp, explore-mcp, drovr, hunk, ssh-oidc, ... }:
  let
    mkHome = system: machine: home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      extraSpecialArgs = {
        inherit sops-nix walker nixgl agent-orchestrator ao-mcp explore-mcp drovr hunk machine;
        inherit system;
      };
      modules = [ ./home.nix ];
    };
    linuxHome = mkHome "x86_64-linux";
  in {
    homeConfigurations.utsuho = linuxHome {
      hostname = "utsuho";
      gui = true;
      gpu = "amd";
    };
    homeConfigurations.setsuna = linuxHome {
      hostname = "setsuna";
      gui = true;
    };
    homeConfigurations.fujiwara = linuxHome {
      hostname = "fujiwara";
      gui = true;
      gpu = "amd";
    };
    homeConfigurations.mari = mkHome "aarch64-darwin" {
      hostname = "mari";
      gui = true;
    };

    darwinConfigurations.mari = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        sops-nix.darwinModules.sops
        ssh-oidc.darwinModules.default
        ({ config, ... }: {
          programs.zsh.enable = true;
          nix.enable = true;
          nix.settings.experimental-features = [ "nix-command" "flakes" ];

          # Self-hosted attic binary cache (kube cluster); signing key is public.
          # Assumes nix-darwin manages nix (nix.enable = true). If mari moves to
          # Determinate Nix, relocate this to /etc/determinate like the Linux boxes.
          nix.settings.extra-substituters = [ "https://attic.ko.ag/kube" ];
          nix.settings.extra-trusted-public-keys = [ "kube:YLRejBKnIVKqvZRXBvFR4KmosPZPg9phiM+pRlhbQ+c=" ];
          # Private cache → read token via netrc, rendered as root by sops-nix.
          sops.secrets.atticPullToken = { };
          sops.templates."attic-netrc".content = "machine attic.ko.ag password ${config.sops.placeholder.atticPullToken}";
          nix.settings.netrc-file = config.sops.templates."attic-netrc".path;

          # Remote builder: Linux boxes delegate aarch64-darwin builds here over
          # Tailscale. The gate's ForceCommand would hijack the non-interactive
          # `nix-store --serve` path if it shared the human `sauyon` login, so the
          # builder connects as the dedicated `nixremote` user, carved out by the
          # ssh-oidc-gate Match block below. Both users must be nix trusted-users.
          # DEPLOY: update the Linux boxes' nix.custom.conf `builders` line from
          #   ssh-ng://sauyon@100.106.204.103 ...   to   ssh-ng://nixremote@100.106.204.103 ...
          # and ensure the builder pubkey is in /run/secrets/ssh-oidc-builder-key.pub here.
          nix.settings.trusted-users = [ "sauyon" "nixremote" ];
          system.stateVersion = 6;
          nixpkgs.hostPlatform = "aarch64-darwin";

          environment.etc."sops/age-unused.txt".text = "";
          sops.defaultSopsFile = ./secrets.yaml;
          sops.age.keyFile = "/etc/sops/age-unused.txt";
          sops.age.sshKeyPaths = [ ];
          sops.gnupg.sshKeyPaths = [ ];
          sops.environment.GOOGLE_APPLICATION_CREDENTIALS = "/Users/sauyon/.config/sops/gcp-key.json";
          # Human key(s). With the OIDC gate active, its always-accept
          # AuthorizedKeysCommand (not this file) decides the publickey stage for
          # the `sauyon` login; the builder carve-out is handled separately
          # (sshOidcBuilderKey, below).
          sops.secrets."ssh-authorized-keys-sauyon" = {
            mode = "0644";
          };

          # ssh-oidc gate. Shared bearer token for the enrollment service;
          # rendered by sops-nix, read by the gate at runtime (never in the store).
          # NOTE: add an `sshOidcToken` key to secrets.yaml (the gate's service token).
          sops.secrets.sshOidcToken = {
            # Readable by the login (sauyon) user — the gate runs as that user.
            mode = "0444";
          };
          # The remote-builder's PUBLIC key, in its own authorized_keys file, used by
          # the gate's Match-block carve-out for the `nixremote` builder user.
          # NOTE: add an `sshOidcBuilderKey` key to secrets.yaml (the builder pubkey,
          # the same value currently in `mariBuilderKey`'s public half).
          sops.secrets.sshOidcBuilderKey = {
            mode = "0444";
          };

          services.ssh-oidc-gate = {
            enable = true;
            serviceUrl = "https://ssh-oidc.ko.ag";
            tokenFile = config.sops.secrets.sshOidcToken.path;
            # Remote-build carve-out (MANDATORY): the nix daemon connects as `nixremote`
            # and bypasses the ForceCommand gate via this Match block.
            builderMatchUser = "nixremote";
            builderAuthorizedKeysFile = config.sops.secrets.sshOidcBuilderKey.path;
          };

          # Dedicated, no-login builder account the nix daemon SSHes in as;
          # nix-darwin creates it (knownUsers drives dscl). Must be a trusted-user
          # (above) to run builds; the gate carve-out lets its key skip the OIDC
          # ForceCommand.
          users.knownUsers = [ "nixremote" ];
          users.users.nixremote = {
            uid = 541;
            gid = 541;
            home = "/var/empty";
            shell = "/usr/bin/false";
            description = "nix remote builder";
            isHidden = true;
          };
          users.knownGroups = [ "nixremote" ];
          users.groups.nixremote = {
            gid = 541;
            description = "nix remote builder";
          };

          services.openssh = {
            enable = true;
            # The gate module appends AuthenticationMethods/AuthorizedKeysCommand/
            # ForceCommand/ExposeAuthInfo (+ the nixremote Match carve-out) via mkAfter.
            # Keep only host-level policy here; do NOT set AuthorizedKeysFile (the gate
            # sets it to `none` globally and to the builder key inside the Match block).
            extraConfig = ''
              PermitRootLogin no
              PasswordAuthentication no
            '';
          };
        })
      ];
    };

    # Patched tailscaled for the Linux boxes. Tailscale SSH creates a logind
    # session before the user's shell runs, which for a systemd-homed user whose
    # LUKS home is still locked makes logind start user@<uid>.service against
    # homed's fallback home ("/") -- permanently breaking that user manager's
    # unit search path for the rest of the boot. See the patch header.
    #
    # Only `patches` is overridden, deliberately: buildGoModule builds its
    # goModules derivation from src + go.mod/go.sum, and this patch touches a
    # single .go file, so vendoring is unaffected and no vendorHash change is
    # needed. Overriding `src` to a different tailscale release would NOT be
    # safe this way -- goModules would still be built from the original source.
    #
    # Deployed by system/deploy, which builds this, pins it with a root-owned GC
    # root outside the encrypted home (so it survives `nix-collect-garbage` and
    # is readable at boot before /home/sauyon is unlocked), and points
    # /etc/systemd/system/tailscaled.service at it.
    packages.x86_64-linux.tailscale-patched =
      let
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in
      pkgs.tailscale.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [
          ./patches/tailscale-ssh-skip-logind-session-for-locked-homed-user.patch
        ];
      });
  };
}
