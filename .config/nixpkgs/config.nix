{
  allowUnfree = true;

  packageOverrides = pkgs: rec {
    # chromium = pkgs.chromium.overrideAttrs (oldAttrs: rec {
    #   buildCommand = oldAttrs.buildCommand + ''
    #     wrapProgram $out/bin/chromium --add-flags "--device-scale-factor=1.5"
    #   '';
    # });
  };
}
