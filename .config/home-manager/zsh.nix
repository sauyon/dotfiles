{
  pkgs,
  home,
  xdg,
  ...
}:
{
  enable = true;

  autosuggestion.enable = true;

  syntaxHighlighting.enable = true;

  autocd = true;
  defaultKeymap = "emacs";

  dotDir = ".config/zsh";

  history = {
    expireDuplicatesFirst = true;
    ignoreDups = true;
    path = "${home}/.local/share/zsh/history";
    save = 1000000;
    size = 1000000;
  };

  initContent = ''
    export "XDG_CONFIG_HOME=$HOME/.config"
    source "$XDG_CONFIG_HOME/zsh/utils"
    source "$XDG_CONFIG_HOME/zsh/config"
    source "$XDG_CONFIG_HOME/zsh/aliases"

    [ -f /etc/profile.d/google-cloud-cli.sh ] && source /etc/profile.d/google-cloud-cli.sh

    [ -f $HOME/.nix-profile/etc/profile.d/nix.sh ] && [ -z $NIX_PATH ] && source $HOME/.nix-profile/etc/profile.d/nix.sh

    PAGER="${pkgs.bat}/bin/bat --paging=always --color=always --decorations=never --"

    bindkey '^T' transpose-chars
  '';

  oh-my-zsh = {
    enable = true;

    plugins = [
      "git"
      "golang"
      "sudo"
      "rust"
      "kubectl"
    ];
  };

  sessionVariables = {
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_RUNTIME_DIR = "/run/user/$(id -u)";
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

    gnight = ''${pkgs.cowsay}/bin/cowsay "GNIGHT's not IGHT"; systemctl suspend'';

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

    swaycheatsheet = "egrep '^s*bind' $XDG_CONFIG_HOME/sway/config | sed -E 's/ --[S-]*b//g' | cut -d ' ' -f 2- | sed -E 's/ +/	/' | column -ts $'	' -c 100 -W2 -o ' | ' | less";

    gcf = "git commit --fixup";
    gcaf = "git commit -a --fixup";

    ykoath = "yubioath-desktop";

    quck = "codeql query compile --warnings=error -n --search-path . --additional-packs . -j8";
    qlfmt = "codeql autoformat";

    tf = "terraform";
    t = "terragrunt";
    tra = "terragrunt run-all";
    traa = "terragrunt run-all apply";
    ts = "tailscale";
    tsk = "tailscale configure kubeconfig";

    b = "bentoml";
    bctl = "bentocloudctl";
  };
}
