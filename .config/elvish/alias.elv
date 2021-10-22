use str

var aliases = [
  &ls= [ls -q --color=auto]
  &l= [ls -FB]
  &ll= [ls -qalhF --color=auto]
  &grep= [grep -q --color=auto]

  &nb= [nix-build]
  &ne= [nix-env]
  &nei= [nix-env -i]
  &nee= [nix-env -e]
  &neu= [nix-env -u]
  &neq= [nix-env -q]
  &nbd= [nix-build]
  &ns= [nix-shell]
  &nsp= [nix-shell -p]
  &nsr= [nix-shell --run]

  &nre= [sudo -i nixos-rebuild switch]
  &nreu= [sudo -i nixos-rebuild switch --upgrade]

  &g= [git]
  &ga= [git add]
  &gb= [git branch]
  &gc= [git commit -v]
  &gca= [git commit -v -a]
  &gcam= [git commit -am]
  &gcmsg= [git commit -m]
  &gcb= [git checkout -b]
  &gd= [git diff]
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
  &glg= [git log --graph]
  &glol= [git log --graph --pretty='''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset']
  &glol= [git log --graph --pretty='''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset' --all]
  &glols= [git log --graph --pretty='''%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%ar)
 %C(bold blue)<%an>%Creset' --stat]
  &grhh= [git reset --hard]

  &rcp= [rsync -rgoP]
]

edit:add-var "nix-clean~" [@_args]{
  nix-env --delete-generations old
  nix-store --gc
}

fn git_main_branch {
  if (not ?(git rev-parse --git-dir)) {
    fail "not in git directory"
  }

  for ref [refs/{heads remotes/{origin upstream}}/{main trunk}] {
    if ?(git show-ref -q --verify $ref >/dev/null) {
      put (split '/' $ref)[-1]
      return
    }
  }
  put master
}

fn git_current_branch {
  var ref = ""
  try {
    ref = (git --no-optional-locks symbolic-ref --quiet HEAD 2>/dev/null)
  } except e {
    var reason = $e[reason]
    if (and (==s $reason[type] "external-cmd/exited") (== $reason[exit-status] 128)) {
      fail "not in git directory"
    }
    ref = (git --no-optional-locks rev-parse --short HEAD 2>/dev/null)
  }
  put (str:trim-prefix $ref "refs/heads/")
}

edit:add-var gcm~ [@_args]{
  git switch (git_main_branch)
}

edit:add-var "grbm~" [@_args]{
  git rebase (git_main_branch)
}

edit:add-var "gwip~" [@_args]{
  git add -A
  try { git rm (git ls-files --deleted) 2>/dev/null } except { nop }
  git commit --no-verify --no-gpg-sign -m "--wip-- [skip ci]"
}

edit:add-var "gunwip~" [@_args]{
  if ?(git log -n 1 | grep -q -c '\-\-wip\-\-') {
    git reset HEAD~1
  }
}

edit:add-var "ggsup~" [@_args]{
  git branch --set-upstream-to=origin/(git_current_branch)
}

edit:add-var gpristine~ [@_args]{
  git reset --hard
  git clean -dffx
}

fn make-alias [cmds]{
  put [@_args]{ /usr/bin/env $@cmds $@_args }
}

for alias [(keys $aliases)] {
  edit:add-var $alias"~" (make-alias $aliases[$alias])
}
