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
    # Terminal diff viewer for agent-authored changesets; ships a bundled
    # `hunk-review` Claude skill under `${hunk}/skills/`.
    hunk.url = "github:modem-dev/hunk";
    # The seamless OIDC SSH gate (gate binary + nixos/darwin modules).
    ssh-oidc.url = "git+https://codeberg.org/sauyon/ssh-oidc";
  };

  outputs = { nixpkgs, home-manager, nix-darwin, sops-nix, walker, nixgl, agent-orchestrator, ao-mcp, explore-mcp, hunk, ssh-oidc, ... }:
  let
    mkHome = system: machine: home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      extraSpecialArgs = {
        inherit sops-nix walker nixgl agent-orchestrator ao-mcp explore-mcp hunk machine;
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

          # Self-hosted attic binary cache (kube cluster). The signing key is public.
          # Assumes nix-darwin manages nix (nix.enable = true). If mari is ever moved
          # to Determinate Nix, relocate this to /etc/determinate like the Linux boxes.
          nix.settings.extra-substituters = [ "https://attic.ko.ag/kube" ];
          nix.settings.extra-trusted-public-keys = [ "kube:YLRejBKnIVKqvZRXBvFR4KmosPZPg9phiM+pRlhbQ+c=" ];
          # Private cache → read token via netrc, rendered as root by sops-nix.
          sops.secrets.atticPullToken = { };
          sops.templates."attic-netrc".content = "machine attic.ko.ag password ${config.sops.placeholder.atticPullToken}";
          nix.settings.netrc-file = config.sops.templates."attic-netrc".path;

          # Remote builder: the Linux boxes delegate aarch64-darwin builds here over
          # Tailscale. With the OIDC gate's ForceCommand in place, the non-interactive
          # `nix-store --serve` path would be HIJACKED if it shared the human `sauyon`
          # login — so the builder now connects as the dedicated `nixremote` user, which
          # the ssh-oidc-gate Match-block carves out below. Both users must be nix
          # trusted-users to be allowed to run builds.
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
          # Human key(s). With the OIDC gate active the gate's always-accept
          # AuthorizedKeysCommand (not this file) decides the publickey stage for the
          # `sauyon` login, so this file is now only authoritative for the builder
          # carve-out is handled separately (sshOidcBuilderKey, below).
          sops.secrets."ssh-authorized-keys-sauyon" = {
            mode = "0644";
          };

          # ssh-oidc gate. Shared bearer token presented to the enrollment service;
          # rendered by sops-nix and read by the gate at runtime (never in the store).
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
          # CF Access service-token client SECRET (client id is non-secret, set inline
          # below). ssh-oidc.ko.ag is fronted by CF Access; the gate sends the
          # CF-Access-Client-Id/Secret headers so its non-interactive request is admitted.
          # NOTE: add `sshOidcCfAccessClientSecret` to secrets.yaml (tf output
          # ssh_oidc_cf_access_client_secret).
          sops.secrets.sshOidcCfAccessClientSecret = {
            mode = "0444";
          };

          services.ssh-oidc-gate = {
            enable = true;
            serviceUrl = "https://ssh-oidc.ko.ag";
            tokenFile = config.sops.secrets.sshOidcToken.path;
            # CF Access service token (ssh-oidc.ko.ag sits behind a CF Access policy).
            cfAccessClientId = "a3cccc560551e6cafef2ab9e230744cf.access";
            cfAccessClientSecretFile = config.sops.secrets.sshOidcCfAccessClientSecret.path;
            # Remote-build carve-out (MANDATORY): the nix daemon connects as `nixremote`
            # and bypasses the ForceCommand gate via this Match block.
            builderMatchUser = "nixremote";
            builderAuthorizedKeysFile = config.sops.secrets.sshOidcBuilderKey.path;
          };

          # Dedicated, no-login builder account the nix daemon SSHes in as. nix-darwin
          # creates it (knownUsers drives dscl). It must be a trusted-user (above) to
          # run builds; the gate carve-out lets its key skip the OIDC ForceCommand.
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
  };
}
