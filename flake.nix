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
    codex-desktop-linux = {
      url = "github:ilysenko/codex-desktop-linux";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, nix-darwin, sops-nix, walker, nixgl, agent-orchestrator, ao-mcp, codex-desktop-linux, ... }:
  let
    mkHome = system: machine: home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      extraSpecialArgs = {
        inherit sops-nix walker nixgl agent-orchestrator ao-mcp codex-desktop-linux machine;
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
          # Tailscale (ssh-ng://sauyon@mari-1...). The connecting SSH user must be a
          # nix trusted-user to be allowed to run builds. The builder's public key is
          # authorized via sops `ssh-authorized-keys-sauyon` (services.openssh above).
          nix.settings.trusted-users = [ "sauyon" ];
          system.stateVersion = 6;
          nixpkgs.hostPlatform = "aarch64-darwin";

          environment.etc."sops/age-unused.txt".text = "";
          sops.defaultSopsFile = ./secrets.yaml;
          sops.age.keyFile = "/etc/sops/age-unused.txt";
          sops.age.sshKeyPaths = [ ];
          sops.gnupg.sshKeyPaths = [ ];
          sops.environment.GOOGLE_APPLICATION_CREDENTIALS = "/Users/sauyon/.config/sops/gcp-key.json";
          sops.secrets."ssh-authorized-keys-sauyon" = {
            mode = "0644";
          };

          services.openssh = {
            enable = true;
            extraConfig = ''
              AuthorizedKeysFile /run/secrets/ssh-authorized-keys-sauyon
              PermitRootLogin no
              PasswordAuthentication no
            '';
          };
        })
      ];
    };
  };
}
