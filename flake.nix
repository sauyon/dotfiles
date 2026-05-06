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
    rampart.url = "github:sauyon/rampart-1";
    hermes-agent.url = "github:sauyon/hermes-agent";
    agentguard.url = "github:sauyon/agentguard";
  };

  outputs = { nixpkgs, home-manager, nix-darwin, sops-nix, walker, nixgl, agent-orchestrator, ao-mcp, rampart, hermes-agent, agentguard, ... }:
  let
    system = "x86_64-linux";
  in {
    homeConfigurations.sauyon = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      extraSpecialArgs = {
        inherit sops-nix walker nixgl agent-orchestrator ao-mcp rampart hermes-agent agentguard system;
      };
      modules = [ ./home.nix ];
    };

    darwinConfigurations.mari = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        {
          programs.zsh.enable = true;
          nix.enable = true;
          nix.settings.experimental-features = [ "nix-command" "flakes" ];
          system.stateVersion = 6;
          nixpkgs.hostPlatform = "aarch64-darwin";
        }
      ];
    };
  };
}
