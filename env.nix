{
  pkgs,
  lib,
  home,
  xdg,
  config,
  # Passed in from home.nix rather than re-derived from `machine`, so the
  # `machine.gui or true` default is defined in exactly one place.
  isDesktop,
  ...
}:
rec {
  # Unclutter home directory
  LESSHISTFILE = "${xdg.dataHome}/less/history";
  CARGO_HOME = "${xdg.dataHome}/cargo";
  RUSTUP_HOME = "${xdg.dataHome}/rustup";
  ASPELL_CONF = "per-conf ${xdg.configHome}/aspell/aspell.conf; personal ${xdg.configHome}/aspell/en.personal; repl ${xdg.configHome}/aspell/en.prepl";
  ZSH_CACHE_DIR = "${xdg.cacheHome}/zsh";

  GOPATH = "${home}/devel/go";

  # Prepend the home-manager profile bin so it lands in environment.d too. Login
  # shells get it from hm-session-vars.sh, but the systemd user manager (and thus
  # services like walker that spawn `firefox` by name) only sees this PATH; its
  # PAM-inherited $PATH is just /usr/local/bin:/usr/bin.
  PATH = "${home}/.nix-profile/bin:$PATH:/snap/bin:$GOPATH/bin:${xdg.dataHome}/cargo/bin:${home}/.local/bin:${home}/.krew/bin";

  GRAVEYARD = "$XDG_RUNTIME_DIR/trash";

  PAGER = "${pkgs.bat}/bin/bat --paging=always --color=always --decorations=never --";
  BAT_PAGER = "${pkgs.less}/bin/less";
  LESS = "-RFx4";

  # gpg for ssh
  GPG_TTY = "$(tty)";

  "_JAVA_AWT_WM_NONREPARENTING" = "1";

  QT_QPA_PLATFORM = "wayland";
  QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";

  MOZ_ENABLE_WAYLAND = "1";
  MOZ_LEGACY_PROFILES = "1";

  WARP_ENABLE_WAYLAND = "1";

  TG_PROVIDER_CACHE = "1";

  # cursor-agent otherwise dumps verbose debug logs to
  # $TMPDIR/cursor-agent-logs-<uid>; $TMPDIR is /tmp (tmpfs, i.e. RAM). These
  # grow unbounded (observed 11G) and get paged to swap. Disable them.
  CURSOR_AGENT_DISABLE_DEBUG_LOG = "1";

  # `coder config-ssh` (run by IDE integrations / `coder` tooling) rewrites its
  # target SSH config in place, dereferencing and clobbering the read-only store
  # symlink home-manager puts at ~/.ssh/config. Redirect those writes to a
  # throwaway file nothing Includes; the nix-managed *.coder blocks in home.nix's
  # programs.ssh.settings are the real source of truth.
  CODER_SSH_CONFIG_FILE = "${xdg.configHome}/coderv2/ssh-config";
}
// lib.optionalAttrs (!pkgs.stdenv.isDarwin) {
  SSH_AGENT_PID = "";
  # ssh-tpm-agent is the primary SSH agent (programs.ssh-tpm-agent.enable in
  # home.nix); gpg-agent has SSH support explicitly disabled there. ssh-tpm-agent
  # falls back to gpg's S.gpg-agent.ssh for non-TPM keys via its --fallback arg.
  SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/ssh-tpm-agent.sock";

  # Nix-built ncurses doesn't search distro paths by default, and home-manager's
  # generic-linux only wires TERMINFO_DIRS for systemd units, not interactive
  # shells. Set it here so Nix-built tools (less, etc.) find both Nix-installed
  # terminfo (e.g. ghostty) and the distro's.
  TERMINFO_DIRS = "${home}/.nix-profile/share/terminfo:/usr/share/terminfo";
}
// lib.optionalAttrs (!pkgs.stdenv.isDarwin && isDesktop) {
  # Desktop-only: programs.firefox.enable is isDesktop-gated, so on a headless
  # host this would name a binary that isn't installed. Leave BROWSER unset
  # there and let each tool fall back to its own default rather than fail with
  # a bare command-not-found.
  BROWSER = "firefox";
}
