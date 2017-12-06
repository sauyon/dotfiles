{
  allowUnfree = true;

  packageOverrides = pkgs: rec {
    quodlibet = if pkgs?quodlibet-full then
      pkgs.quodlibet-full.override { webkitgtk = null; }
    else null;
    # chromium = (pkgs.chromium.override {
    #   enableNaCl = true;
    # });

    chromium = pkgs.chromium.overrideAttrs (oldAttrs: rec {
      buildCommand = oldAttrs.buildCommand + ''
        wrapProgram $out/bin/chromium --add-flags "--device-scale-factor=1.5"
      '';
    });
  };
}
