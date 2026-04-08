{
  pkgs,
  home,
  xdg,
  ...
}:
{
  enable = true;
  enableCompletion = true;
  autosuggestion.enable = true;
  syntaxHighlighting.enable = true;

  autocd = true;
  defaultKeymap = "emacs";

  dotDir = "${xdg.configHome}/zsh";

  history = {
    expireDuplicatesFirst = true;
    ignoreDups = true;
    path = "${home}/.local/share/zsh/history";
    save = 1000000;
    size = 1000000;
  };

  plugins = [
    {
      name = "powerlevel10k";
      src = pkgs.zsh-powerlevel10k;
      file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
    }
  ];

  envExtra = ''
    # brew
    if [[ -f /home/linuxbrew/.linuxbrew/bin/brew ]]; then
      eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
    elif [[ -f /opt/homebrew/bin/brew ]]; then
      eval "$(/opt/homebrew/bin/brew shellenv)"
    fi

    eval "$(${pkgs.mise}/bin/mise activate zsh)"
  '';

  initContent = ''
    export "XDG_CONFIG_HOME=$HOME/.config"
    source "$XDG_CONFIG_HOME/zsh/utils"
    source "$XDG_CONFIG_HOME/zsh/config"
    source "$XDG_CONFIG_HOME/zsh/aliases"

    # mise completions
    if [[ ! -f "$ZSH_CACHE_DIR/completions/_mise" ]]; then
      typeset -g -A _comps
      autoload -Uz _mise
      _comps[mise]=_mise
    fi
    ${pkgs.mise}/bin/mise completion zsh >| "$ZSH_CACHE_DIR/completions/_mise" &|

    [ -f /etc/profile.d/google-cloud-cli.sh ] && source /etc/profile.d/google-cloud-cli.sh

    [ -f $HOME/.nix-profile/etc/profile.d/nix.sh ] && [ -z $NIX_PATH ] && source $HOME/.nix-profile/etc/profile.d/nix.sh

    PAGER="${pkgs.bat}/bin/bat --paging=always --color=always --decorations=never --"

    yank() { printf '\033]52;c;%s\a' "$(base64 | tr -d '\n')"; }

    bindkey '^T' transpose-chars

    if [ -n "$SSH_CONNECTION" ] || [ -n "$SSH_CLIENT" ]; then
        if [ -z "$ZELLIJ_SESSION_NAME" ]; then
            zellij attach -c main_ssh
        fi
    fi

    # The greeting. Yeah, yeah, I'm unimaginative. :'(
    echo "Hello, $(${pkgs.inetutils}/bin/hostname -s)"'!'
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
    _ZO_DOCTOR = "0";
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

    chat = "LUA_PATH='$HOME/.config/weechat/lua/cjson.so' ${pkgs.dtach}/bin/dtach -A chat weechat";

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

    tf = "tofu";
    t = "mise run tg --";
    tm = "terramate";
    taa = "terragrunt apply --all";
    tpa = "terragrunt plan --all -out tfplan";
    ts = "tailscale";
    tsk = "tailscale configure kubeconfig";

    b = "bentoml";
    bc = "bentocloudctl";

    mr = "mise run";
    mrk = "mise run kubeconfig";

    cl = "claude --enable-auto-mode";
    clw = "claude --enable-auto-mode --worktree";

    z = "zellij";
    zs = "zellij -s";
    za = "zellij attach -c";
    zl = "zellij list-sessions | rg -v EXITED";

		hm = "home-manager";
		hms = "home-manager switch";
  };
}
