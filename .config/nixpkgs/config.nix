{
  allowUnfree = true;

  packageOverrides = pkgs: rec {
    # quodlibet = if pkgs.quodlibet-full != null then pkgs.quodlibet-full.override { webkitgtk = null; } else null;
    chromium = pkgs.chromium.override { enableNaCl = true; };
  };
}
