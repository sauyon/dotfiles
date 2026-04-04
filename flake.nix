{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, nix-darwin, ... }: {
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
