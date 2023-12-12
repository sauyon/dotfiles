use path
use str
# use edit

try { eval (any-nix-shell elvish --info-right | slurp) } catch { }

edit:add-var ls~ {|@_args| e:ls -q --color=auto $@_args }
edit:add-var l~ {|@_args| ls -FB $@_args }
edit:add-var ll~ {|@_args| ls -alhF $@_args }

edit:add-var grm~ {|@_args| git rm $@_args }
edit:add-var gsi~ {|@_args| git submodule init $@_args }
edit:add-var gsu~ {|@_args| git submodule update $@_args }

var aliases = [
  &grep= [grep --color=auto]
  &diff= [diff --color=auto -utr]

  &sctl= [systemctl]
  &uctl= [systemctl --user]
  &jctl= [journalctl -le]
  &jctlu= [jctl -u]
  &jctlf= [journalctl -lf]
  &jctlfu= [jctlf -u]

  &nb= [nix-build]
  &ne= [nix-env]
  &nei= [nix-env -i]
  &nee= [nix-env -e]
  &neu= [nix-env -u]
  &neq= [nix-env -q]

  &g= [git]
  &ga= [git add]
  &gb= [git branch]
  &gc= [git commit -v]
  &gca= [git commit -v -a]
  &gcam= [git commit -am]
  &gcmsg= [git commit -m]
  &gcb= [git checkout -b]
  &gd= [git diff --patience]
  &gdca= [git diff --cached]
  &gp= [git push]
  &gpf= [git push --force-with-lease]
  &gsb= [git status --branch]
  &gsw= [git switch]
  &gcf= [git commit --fixup]
  &gcaf= [git commit -a --fixup]
  &gcl= [git clone --recurse-submodules]
  &gcp= [git cherry-pick]
  &gco= [git checkout]
  &grb= [git rebase]
  &gl= [git pull]
  &glg= [git log --stat]
  &glgg= [git log --graph]
  &glol= [git log --graph --pretty='''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset']
  &glol= [git log --graph --pretty='''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset' --all]
  &glols= [git log --graph --pretty='''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar)
 %C(bold blue)<%an>%Creset' --stat]
  &grhh= [git reset --hard]
  &gr= [git remote]
  &grv= [git remote -v]
  &gra= [git remote add]
  &grset= [git remote set-url]
  &gwch= [git whatchanged -p --abbrev-commit --pretty=medium]
  &gf= [git fetch]

  &rcp= [rsync -rgoP]

  &k= [kubectl]

  &b= [bentoml]
]

edit:add-var nix-clean~ {|@_args|
  nix-env --delete-generations old
  nix-store --gc
}

edit:add-var nf~ {|@_args| nix --extra-experimental-features nix-command --extra-experimental-features flakes search nixpkgs $@_args }

edit:add-var nre~ {|@_args| sudo -i nixos-rebuild switch $@_args }
edit:add-var nreu~ {|@_args| sudo -i nixos-rebuild switch --upgrade $@_args }

edit:add-var gnight~ {
  cowsay "GNIGHT's not IGHT"
  systemctl suspend
}

fn git_main_branch {
  if (not ?(git rev-parse --git-dir)) {
    fail "not in git directory"
  }

  for ref [refs/{heads remotes/{origin upstream}}/{main trunk}] {
    if ?(git show-ref -q --verify $ref >/dev/null) {
      put [(str:split '/' $ref)][-1]
      return
    }
  }
  put master
}

fn git_current_branch {
  var ref = ""
  try {
    set ref = (git --no-optional-locks symbolic-ref --quiet HEAD 2>/dev/null)
  } catch e {
    var reason = $e[reason]
    if (and (==s $reason[type] "external-cmd/exited") (== $reason[exit-status] 128)) {
      fail "not in git directory"
    }
    set ref = (git --no-optional-locks rev-parse --short HEAD 2>/dev/null)
  }
  put (str:trim-prefix $ref "refs/heads/")
}

edit:add-var gchanged~ {
  git diff --name-only --diff-filter=d (git_main_branch) -z | from-terminated "\x00"
}

edit:add-var gcm~ {|@_args|
  git switch (git_main_branch)
}

edit:add-var grbm~ {|@_args|
  git rebase (git_main_branch) $@_args
}

edit:add-var gwip~ {|@_args|
  git add -A
  try { git rm (git ls-files --deleted) 2>/dev/null } catch { nop }
  git commit --no-verify --no-gpg-sign -m "--wip-- [skip ci]" $@_args
}

edit:add-var gunwip~ {
  if ?(git log -n 1 | grep -q -c '\--wip--') {
    git reset HEAD~1
  }
}

edit:add-var ggsup~ {
  git branch --set-upstream-to=origin/(git_current_branch)
}

edit:add-var gpristine~ {|@_args|
  git reset --hard
  git clean -dffx
}

edit:add-var edit~ {|@_args|
  emacsclient -n $@_args
}

edit:add-var sedit~ {|_arg|
  var escaped = (str:replace '"' '\"' (path:abs $_arg))
  emacsclient -ne '(find-file-root "'$escaped'")'
}

edit:add-var nix~ {|@_args|
  if (and (> 0 (count $_args)) (==s $_args[0] "shell")) {
    .any-nix-shell-wrapper zsh (all $_args[1..])
  } else {
    nix $@_args
  }
}

edit:add-var ns~ {|@_args|
  nix-shell $@_args
}

edit:add-var nsp~ {|@_args|
  nix-shell -p $@_args
}

edit:add-var nsr~ {|@_args|
  nix-shell --run $@_args
}

edit:add-var nbd~ {|@_args|
  nix-build -E 'with import <nixpkgs> { }; callPackage ./default.nix {}'
}

edit:add-var '..~' { cd .. }
edit:add-var '...~' { cd ../.. }
edit:add-var '....~' { cd ../../.. }

edit:add-var bctl~ {|@_args|
  bentocloudctl $@_args
}

fn make-alias {|cmds|
  put {|@_args| /usr/bin/env $@cmds $@_args }
}

for alias [(keys $aliases)] {
  edit:add-var $alias"~" (make-alias $aliases[$alias])
}
