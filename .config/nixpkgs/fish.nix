{ pkgs, lib, ... }:

let
  fishPlugin = repo: {
    name = lib.lists.last (lib.strings.splitString "/" repo);
    src = fetchTarball ("https://github.com/" + repo + "/archive/master.tar.gz");
  };
in {
  enable = true;

  plugins = [
    (fishPlugin "jhillyerd/plugin-git")
  ];

  shellAbbrs = {
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

  functions = {
    _prompt_gitstatus = ''
      set STATUS (gitstatus)
      echo "$STATUS[1]" -n
      if [ $TEST[4] != "" ]; echo " $TEST[4]" -n; end
      if
    '';
  };

  interactiveShellInit = ''
    bind \ct transpose-chars
  '';
}
