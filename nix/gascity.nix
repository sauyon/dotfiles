# Gas City (`gc`), the orchestration SDK that replaced Gas Town (`gt`).
#
# Gas Town's last release (v1.2.1, Jun 2026) predates beads 1.3, and a fresh
# `gt install` on bd 1.3.x deadlocks on bd's dirty-table migration guard
# (gastownhall/beads#6089). Gas City is where the work moved: its v1.5.0
# deps.env pins bd v1.3.1 and wants dolt >= 2.1.0.
#
# Built from source to carry ../patches/gascity-runtime-dir.patch, which adds
# GC_RUNTIME_DIR. Upstream couples the runtime dir to GC_HOME: any GC_HOME off
# ~/.gc counts as an "isolated" home and moves the supervisor socket and lock
# under it instead of $XDG_RUNTIME_DIR/gc. env.nix relocates GC_HOME to XDG
# state, so without the patch sockets would live in ~/.local/state.
#
# The name: oh-my-zsh's git plugin aliases `gc` to `git commit -v`, so in
# interactive zsh `gc` stays git commit and Gas City is typed as `gascity`.
# The binary must still be on PATH as `gc`: Gas City calls itself by that name
# in the hooks and prompts it hands agents (`gc prime`, `gc hook`, `gc mail`,
# ~89 files at v1.5.0) and in a few exec("gc") calls. Hooks run under sh, where
# the alias does not apply, so they reach the real binary.
{ pkgs }:

pkgs.buildGoModule rec {
  pname = "gascity";
  version = "1.5.0";
  commit = "f66474617d2bb7fbe51a3d746da5f3ceab2b8517";

  src = pkgs.fetchFromGitHub {
    owner = "gastownhall";
    repo = "gascity";
    rev = "v${version}";
    hash = "sha256-le/Cit409xz8VaNwB+MlloxTv7slHGXKEhP3cXbeExw=";
  };

  patches = [ ../patches/gascity-runtime-dir.patch ];

  vendorHash = "sha256-+9G0t52oA+nj8uDCoryUwWQioFgx/m2QvJcwYzmNFUs=";

  subPackages = [ "cmd/gc" ];

  env.CGO_ENABLED = "0";
  env.GOTOOLCHAIN = "local";

  # Mirrors .goreleaser.yml. compiledReleaseTag classifies telemetry as a
  # release (vs canary) build; left empty it would report as a dev build.
  ldflags = [
    "-s" "-w"
    "-X main.version=v${version}"
    "-X main.commit=${commit}"
    "-X main.date=1970-01-01T00:00:00Z"
    "-X github.com/gastownhall/gascity/internal/productmetrics.compiledReleaseTag=${version}"
  ];

  # Upstream's cmd/gc suite drives tmux, dolt and a live supervisor; it does not
  # run in the sandbox. The patch's own tests were run against the source tree.
  doCheck = false;

  postInstall = ''
    ln -s gc $out/bin/gascity
  '';

  meta = {
    description = "Orchestration-builder SDK for multi-agent coding workflows";
    homepage = "https://github.com/gastownhall/gascity";
    license = pkgs.lib.licenses.mit;
    mainProgram = "gc";
  };
}
