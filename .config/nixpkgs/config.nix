{
  allowUnfree = true;

  packageOverrides = pkgs: rec {
    nix = super.nix.override {
      storeDir = "/users/ug15sl/.local/nix/store";
      storeDir = "/users/ug15sl/.local/nix/var";
    };
  };
}
