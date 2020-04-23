{
  allowUnfree = true;

  packageOverrides = pkgs: {
    emacs = pkgs.emacs.override {
      withGTK2 = false; withGTK3 = false;
    };
  };
}
