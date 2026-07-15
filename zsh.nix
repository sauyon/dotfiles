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
  autosuggestion.highlight = "fg=2";
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

    if command -v mise >/dev/null 2>&1; then
      eval "$(mise activate zsh)"
    fi

    # pnpm
    export PNPM_HOME="/home/sauyon/.local/share/pnpm"
    case ":$PATH:" in
      *":$PNPM_HOME:"*) ;;
      *) export PATH="$PNPM_HOME:$PATH" ;;
    esac
  '';

  initContent = ''
    # ── Utils ──────────────────────────────────────────────────────────────
    if [ -z $_SAUYON_UTILS_RUN ]; then
    _SAUYON_UTILS_RUN=true

    include() { [[ -f "$@" ]] && source "$@" }

    is_ssh() { [[ -n $SSH_CONNECTION ]] || [[ -n $SSH_CLIENT ]] || [[ -n $SSH_TTY ]] }
    non_gui() { ! xhost &> /dev/null && [ -z $WAYLAND_DISPLAY ] }

    exists() {
      type "$1" &> /dev/null
    }

    # Claude Remote Control for a project dir (default: cwd), via the
    # claude-remote-control@.service template. Start/stop on the fly.
    clp-rc() {
      local d=''${1:-$PWD}
      d=$(realpath "$d") || return 1
      git -C "$d" rev-parse --is-inside-work-tree &>/dev/null \
        || echo "clp-rc: warning: $d is not a git repo; --spawn worktree needs one" >&2
      # Pre-accept the workspace-trust dialog in the personal profile so headless
      # RC doesn't refuse to start (it can't answer the prompt with no TTY).
      local cfg="''${XDG_CONFIG_HOME:-$HOME/.config}/claude-personal/.claude.json"
      if [[ -f "$cfg" ]]; then
        local tmp="$cfg.clp-rc.$$"
        if jq --arg d "$d" '.projects[$d] = ((.projects[$d] // {}) + {hasTrustDialogAccepted: true})' "$cfg" > "$tmp"; then
          mv "$tmp" "$cfg"
        else
          rm -f "$tmp"; echo "clp-rc: failed to set trust in $cfg" >&2
        fi
      fi
      local unit="claude-remote-control@$(systemd-escape -p "$d").service"
      systemctl --user start "$unit" && echo "started $unit"
      systemctl --user --no-pager status "$unit" | head -5
    }
    clp-rc-stop() {
      local d=''${1:-$PWD}
      d=$(realpath "$d") || return 1
      systemctl --user stop "claude-remote-control@$(systemd-escape -p "$d").service"
    }

    # Cursor Agent pool worker for a project dir (default: cwd), via the
    # cursor-agent-worker@.service template. Start/stop on the fly.
    ca-rc() {
      local d=''${1:-$PWD}
      d=$(realpath "$d") || return 1
      # Pre-accept workspace trust so headless worker start doesn't refuse (no TTY).
      local slug="''${d#/}"
      slug="''${slug//\//-}"
      local trust="$HOME/.cursor/projects/$slug/.workspace-trusted"
      if [[ ! -f "$trust" ]]; then
        mkdir -p "$HOME/.cursor/projects/$slug"
        jq -n --arg d "$d" --arg t "$(date -u +%Y-%m-%dT%H:%M:%S.000Z)" \
          '{trustedAt: $t, workspacePath: $d}' > "$trust" \
          || { echo "ca-rc: failed to write $trust" >&2; return 1; }
      fi
      local unit="cursor-agent-worker@$(systemd-escape -p "$d").service"
      systemctl --user start "$unit" && echo "started $unit"
      systemctl --user --no-pager status "$unit" | head -5
    }
    ca-rc-stop() {
      local d=''${1:-$PWD}
      d=$(realpath "$d") || return 1
      systemctl --user stop "cursor-agent-worker@$(systemd-escape -p "$d").service"
    }

    fi

    # ── Config ─────────────────────────────────────────────────────────────
    export "XDG_CONFIG_HOME=$HOME/.config"

    # completions
    fpath+="$HOME/.config/zsh/.zfunc"
    compinit -u

    # Enable Powerlevel10k instant prompt.
    if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
      source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
    fi

    # Nebius CLI
    if [ -f '/home/sauyon/.nebius/path.zsh.inc' ]; then source '/home/sauyon/.nebius/path.zsh.inc'; fi
    if [ -f '/home/sauyon/.nebius/completion.zsh.inc' ]; then source '/home/sauyon/.nebius/completion.zsh.inc'; fi

    [[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

    # LD_LIBRARY_PATH?!
    LD_LIBRARY_PATH=$HOME/.rustc/rust/rustc/lib:$LD_LIBRARY_PATH

    # Needed for nix :(
    [[ -z $LOCALE_ARCHIVE ]] && export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

    # Nix!
    export NIX_PATH=nixpkgs=channel:nixpkgs-unstable:home-manager=https://github.com/rycee/home-manager/archive/master.tar.gz

    # Emacs ftw!
    if non_gui; then
      export EDITOR='emacsclient -t'
    else
      export EDITOR='emacsclient'
    fi

    export QT_STYLE_OVERRIDE=gtk
    export GTK_OVERLAY_SCROLLING=0
    export MOZ_ENABLE_WAYLAND=1
    export BENTOML_HOME="$XDG_CONFIG_HOME/bentoml"

    # Smart command-not-found with pkgfile
    if exists pkgfile; then
      command_not_found_handler() {
        local pkgs cmd="$1"

        pkgs=(''${(f)"$(pkgfile -b -v -- "$cmd" 2>/dev/null)"})
        if [[ -n "$pkgs" ]]; then
          printf '%s may be found in the following packages:\n' "$cmd"
          printf '  %s\n' $pkgs[@]
          return 127
        fi

        printf 'zsh: command not found: %s\n' "$cmd"

        return 127
      }
    fi

    # Ok, fine, sometimes emacs is stupid. But at least it knows it.
    [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '

    if [[ -f /tmp/checkupdates.log ]]; then
      cat /tmp/checkupdates.log
    fi

    if [[ -f /usr/share/nvm/init-nvm.sh ]]; then
      source /usr/share/nvm/init-nvm.sh
    fi

    export AWS_KEY_PAIR_NAME=sauyon-tofu
    export BENTOCLOUDCTL_PRIVATE_KEY_PATH="$HOME/.ssh/id_sauyon_tofu"
    export PATH="/opt/homebrew/opt/rustup/bin:$PATH"

    # ── Aliases & functions ────────────────────────────────────────────────
    # Add an "alert" alias for long running commands.  Use like so:
    #   sleep 10; alert
    alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '"'"'s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert\s*$//'"'"')"'
    alias cowfortune='fortune | cowsay'

    if [[ -f /usr/bin/man ]]; then
      man() { env man $@ 2>/dev/null || /usr/bin/man $@ }
    fi

    nsk() {
      mkdir .gcroots
      if [[ -f shell.nix ]]; then
        nix-instantiate shell.nix --indirect --add-root $(pwd)/.gcroots/shell.drv
        nix-shell $(readlink $(pwd)/.gcroots/shell.drv) "$@"
      elif [[ -f default.nix ]]; then
        nix-instantiate default.nix --indirect --add-root $(pwd)/.gcroots/default.drv
        nix-shell $(readlink $(pwd)/.gcroots/default.drv) "$@"
      fi
    }

    function workspace() {
      sleep 0.05 && hyprctl dispatch workspace $1
    }

    if non_gui; then
      alias edit="emacsclient -t"
      sedit() { emacsclient -te "(find-file-root \"''${''${1:A}//\"/\\\"}\")" }
    else
      edit() { emacsclient -n "$@" }
      sedit() { emacsclient -ne "(find-file-root \"''${''${1:A}//\"/\\\"}\")" }
    fi

    kdn() {
      kubectl debug --profile=sysadmin --image=ubuntu -it "node/$1" -- bash
    }

    kdnh() {
      kubectl debug --profile=sysadmin --image=ubuntu -it "node/$1" -- nsenter --mount=/host/proc/1/ns/mnt -- bash
    }

    venv:activate() {source "$1/bin/activate"}

    if exists gist; then
      up() {
        local MIME
        while [[ $# > 0 ]]; do
          MIME=$(file -b --mime-type "$1")
          if [[ $MIME =~ "^text/" ]]; then
            if [[ -z $OUT ]]; then
              OUT="$(gist "$1")"
            else
              OUT="''${OUT}\n$(gist "$1")"
            fi
          fi
          shift
        done
        echo $OUT | xclip -selection clipboard
        echo $OUT
      }
    fi

    # ── Remaining init ─────────────────────────────────────────────────────
    # mise completions
    if command -v mise >/dev/null 2>&1; then
      if [[ ! -f "$ZSH_CACHE_DIR/completions/_mise" ]]; then
        typeset -g -A _comps
        autoload -Uz _mise
        _comps[mise]=_mise
      fi
      mise completion zsh >| "$ZSH_CACHE_DIR/completions/_mise" &|
    fi

    [ -f /etc/profile.d/google-cloud-cli.sh ] && source /etc/profile.d/google-cloud-cli.sh

    [ -f $HOME/.nix-profile/etc/profile.d/nix.sh ] && [ -z $NIX_PATH ] && source $HOME/.nix-profile/etc/profile.d/nix.sh

    yank() { printf '\033]52;c;%s\a' "$(base64 | tr -d '\n')"; }

    bindkey '^T' transpose-chars

    eval $(kcs init)

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

    m = "mise";
    mt = "mise tasks";
    mr = "mise run";
    mrk = "mise run kubeconfig";

    ca = "cursor-agent";
    # Route bare `claude` through the personal profile so its runtime state
    # (.claude.json, .credentials.json, history, sessions, oauthAccount,
    # tipsHistory, …) lands in ~/.config/claude-personal/, not the unscoped
    # ~/.claude/ — which is fully nix-managed (store symlinks for
    # settings.json / settings.local.json, plus activation-managed dirs),
    # so unprofiled invocations would fail on writes and/or pollute shared
    # state with whatever subscription you happen to be logged into.
    # Use `command claude` for the raw binary.
    claude = "claude-prof run personal";
    cl = "claude-prof run work";
    clw = "claude-prof run work --worktree";
    clp = "claude-prof run personal";
    clpw = "claude-prof run personal --worktree";
    clz = "claude-prof run zai";
    clzw = "claude-prof run zai --worktree";
    gm = "gemini";

    z = "zellij";
    zs = "zellij -s";
    za = "zellij attach -c";
    zl = "zellij list-sessions | rg -v EXITED";

    hm = "home-manager";
    hms = "home-manager switch --flake ${home}/devel/dotfiles#$HOST";
    hmu = "nix flake update --flake ${home}/devel/dotfiles";
  };
}
