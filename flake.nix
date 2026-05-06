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
  };

  outputs = { nixpkgs, home-manager, nix-darwin, ... }: {
    homeConfigurations.sauyon = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
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
