# Gas City (`gc`), the orchestration SDK that replaced Gas Town (`gt`).
#
# Gas Town's last release (v1.2.1, Jun 2026) predates beads 1.3, and a fresh
# `gt install` on bd 1.3.x deadlocks on bd's dirty-table migration guard
# (gastownhall/beads#6089). Gas City is where the work moved: its v1.5.0
# deps.env pins bd v1.3.1 and wants dolt >= 2.1.0.
#
# The release binary, not a source build: go.mod at v1.5.0 asks for go 1.26.6
# and our nixpkgs has 1.26.2. It is statically linked, so no patchelf. The
# hash is GitHub's published digest for the asset.
#
# The name: oh-my-zsh's git plugin aliases `gc` to `git commit -v`, so in
# interactive zsh `gc` stays git commit and Gas City is typed as `gascity`.
# The binary must still be on PATH as `gc`: Gas City calls itself by that name
# in the hooks and prompts it hands agents (`gc prime`, `gc hook`, `gc mail`,
# ~89 files at v1.5.0) and in a few exec("gc") calls. Hooks run under sh, where
# the alias does not apply, so they reach the real binary.
{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  pname = "gascity";
  version = "1.5.0";

  src = pkgs.fetchurl {
    url = "https://github.com/gastownhall/gascity/releases/download/v${version}/gascity_${version}_linux_amd64.tar.gz";
    sha256 = "0ff5cfc3f1cb2e4260d3d93fcce7a4e40bb5675b74eb9de185156757d4f7fa9c";
  };

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall
    install -Dm755 gc $out/bin/gc
    ln -s gc $out/bin/gascity
    runHook postInstall
  '';

  meta = {
    description = "Orchestration-builder SDK for multi-agent coding workflows";
    homepage = "https://github.com/gastownhall/gascity";
    license = pkgs.lib.licenses.mit;
    mainProgram = "gc";
    platforms = [ "x86_64-linux" ];
  };
}
