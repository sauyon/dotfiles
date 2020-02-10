{ pkgs, ... }: {
  enable = true;

  enableAutosuggestions = true;

  autocd = true;
  defaultKeymap = "emacs";

  dotDir = ".config/zsh";

  history = {
    expireDuplicatesFirst = true;
    ignoreDups = true;
    path = ".local/share/zsh/history";
    save = 100000;
    size = 100000;
  };

  initExtra = ''
    export "XDG_CONFIG_HOME=$HOME/.config"
    source "$XDG_CONFIG_HOME/zsh/utils"
    source "$XDG_CONFIG_HOME/zsh/config"
    source "$XDG_CONFIG_HOME/zsh/aliases"
    source "$XDG_CONFIG_HOME/zsh/powerlevel10k/powerlevel10k.zsh-theme"
    source "$XDG_CONFIG_HOME/zsh/p10k"

    [ -f $HOME/.nix-profile/etc/profile.d/nix.sh ] && [ -z $NIX_PATH ] && source $HOME/.nix-profile/etc/profile.d/nix.sh

    if exists dircolors; then
      if [[ -r "$XDG_CONFIG_HOME/dircolors" ]]; then
        eval "$(dircolors -b "$XDG_CONFIG_HOME/dircolors")"
      else
        eval "$(dircolors -b)"
      fi
    fi

    man() { ${pkgs.man}/bin/man $@ 2>/dev/null || /usr/bin/man $@ }

    fpath=("$XDG_CONFIG_HOME/zsh/completions" $fpath)
    compinit -u

    PAGER="${pkgs.bat}/bin/bat --paging=always --color=always --decorations=never --"
  '';

  oh-my-zsh = {
    enable = true;

    plugins = [ "git" "golang" "sudo" "cargo" "adb" ];
  };

  sessionVariables = {
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
  };

  shellAliases = {
    ls = "${pkgs.coreutils}/bin/ls -q --color=auto";
    grep = "${pkgs.gnugrep}/bin/grep --color=auto";
    diff = "${pkgs.diffutils}/bin/diff --color=auto -utr";

    ll = "ls -alhF";
    la = "ls -A";
    l = "ls -FB";

    # rsync is better.
    rcp = "${pkgs.rsync}/bin/rsync -rgoP";

    chat = "LUA_PATH='$HOME/.config/weechat/lua/cjson.so' abduco -A chat weechat";

    ".." = "cd ..";
    "..." = "..;..";
    "...." = "...;..";
    "....." = "....;..";
    "......" = ".....;..";

    gnight = "${pkgs.cowsay}/bin/cowsay \"GNIGHT's not IGHT\"; systemctl suspend";

    sctl = "systemctl";
    uctl = "systemctl --user";
    jctl = "journalctl -le";
    jctlu = "jctl -u";
    jctlf = "journalctl -lf";
    jctlfu = "jctlf -u";

    nb = "nix-build";
    ne = "nix-env";
    nei = "nix-env -i";
    nee = "nix-env -e";
    neu = "nix-env -u";
    neq = "nix-env -q";
    nbd = "nix build";
    ns = "nix-shell";
    nsp = "nix-shell -p";
    nsr = "nix-shell --run";

    nix-clean = "nix-env --delete-generations old; nix-store --gc";

    nre = "sudo -i nixos-rebuild switch";
    nreu = "sudo -i nixos-rebuild switch --upgrade";

    swaycheatsheet = "egrep '^\s*bind' $XDG_CONFIG_HOME/sway/config | sed -E 's/ --[\S-]*\b//g' | cut -d ' ' -f 2- | sed -E 's/ +/\t/' | column -ts $'\t' -c 100 -W2 -o ' | ' | less";

    gcf = "git commit --fixup";
    gcaf = "git commit -a --fixup";

    ykoath = "yubioath-desktop";

    quck = "codeql query compile --warnings=error -n --search-path . --additional-packs . -j8";
    qlfmt = "qlformat --input";
  };
}
