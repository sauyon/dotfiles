#!/usr/bin/env bash
# Cases for the claim that elephant re-reads its desktop-entry index after a
# home-manager switch -- i.e. that a package added to home.packages shows up in
# walker without anyone restarting a unit by hand.
#
# The bug this exists to prevent, because it is invisible and it lasted days.
# elephant indexes $XDG_DATA_DIRS/applications once at startup and then keeps the
# set current with inotify. Its watch on ~/.nix-profile/share/applications resolves
# to the real directory, /nix/store/<hash>-home-manager-path/share/applications,
# and inotify watches *inodes*. A store path is immutable and a switch does not
# rewrite it -- it builds a new one and repoints the symlink. So no event ever
# fires on the watched inode, and the index elephant serves is frozen at whichever
# generation happened to be current when the service last started.
#
# Observed 2026-09-30 on shiori: elephant had been up since 2026-09-27 15:40
# against generation profile-6. Ten switches later it still answered `vesktop`
# with nothing (added after it started) and still offered `dev.warp.Warp.desktop`
# (removed after it started). Nothing logged, nothing failed, and the launcher
# looked healthy -- the only symptom is an app you installed not being there.
#
# The fix, and therefore what these cases check: put the package-set path into the
# unit so sd-switch restarts elephant whenever that set changes. Two halves, and
# both are load-bearing:
#
#   The trigger must be the package-set path, not any other store path. It has to
#   change exactly when the contents of share/applications change, and
#   config.home.path is that: the collection every desktop entry in the profile
#   comes from. A trigger keyed on elephant's own settings (which is what the
#   upstream module already ships, and which is why this went unnoticed) changes
#   only when the config changes, so it never fires on a package add.
#
#   Deliberately not asserted: that ~/.nix-profile/share resolves *into*
#   home-manager-path. It does under the nix-env layout, where the generation's
#   share is a symlink into it -- and it does not once nix migrates the profile to
#   its own format, where share/applications is a merged directory in a `-profile`
#   store path and home-manager-path is nowhere in the resolved path. An earlier
#   cut of this file pinned the first layout and went red the moment a `nix profile`
#   command migrated the live profile mid-session. The fix does not care: both
#   layouts hand elephant an immutable store directory, and home.path still changes
#   whenever the entry set does.
#
#   X-SwitchMethod must not be keep-old. sd-switch consults it before it looks at
#   anything else, so a unit marked keep-old is left running untouched no matter
#   how much of its text changed -- the trigger would be inert. That is the right
#   setting for hyprland-cleanup (whose ExecStop closes every window, see home.nix);
#   it would be exactly wrong here, and the two are easy to confuse.
#
# Checked against the *rendered* unit rather than the option value, because an
# option home-manager accepted and dropped on the floor would pass an option-only
# check while the file systemd reads stayed unchanged.
#
#   ./tests/elephant-reindex.sh            # tests this checkout
#   ./tests/elephant-reindex.sh /path/to/flake
set -u

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

# Evaluate one option out of a host's config. nix eval's stderr is noise on a
# healthy box and the whole story on a broken one, so it is held back and shown
# only when the eval fails.
eval_raw() { # eval_raw <host> <option path>
  if ! nix eval --raw "$FLAKE#homeConfigurations.$1.config.$2" 2>"$D/err"; then
    echo >&2; echo "nix eval failed for $1's $2:" >&2; cat "$D/err" >&2
    exit 1
  fi
}

# The rendered elephant.service, as systemd will read it. Two wrinkles:
# home.file is keyed by absolute target here, so the home directory has to come
# out of the same config rather than being assumed; and .source names a file
# *inside* a derivation output, which eval alone will not have built -- reading it
# through builtins.readFile forces the build and hands back the text, where a
# plain `cat` of the evaluated path fails on any host whose unit is not already
# realised locally.
unit_text_for() { # unit_text_for <host>
  local home attr
  home=$(eval_raw "$1" home.homeDirectory)
  attr="home.file.\"$home/.config/systemd/user/elephant.service\".source"
  if ! nix eval --raw "$FLAKE#homeConfigurations.$1.config.$attr" \
       --apply builtins.readFile 2>"$D/err"; then
    echo >&2; echo "could not read $1's rendered elephant.service:" >&2
    cat "$D/err" >&2
    exit 1
  fi
}

check() { # check <what> <expected> <actual>
  n=$((n+1))
  if [ "$2" = "$3" ]; then
    echo "ok   $1"
  else
    echo "FAIL $1: expected '$2', got '$3'"; fails=$((fails+1))
  fi
}

# The hosts that run walker, read out of the flake rather than written down: a new
# GUI host added to flake.nix is covered the day it lands.
hosts=$(nix eval --json "$FLAKE#homeConfigurations" --apply builtins.attrNames 2>"$D/err" \
  | jq -r '.[]') || { echo "could not list hosts:" >&2; cat "$D/err" >&2; exit 1; }
walker_hosts=""
for host in $hosts; do
  [ "$(nix eval "$FLAKE#homeConfigurations.$host.config.programs.walker.enable" 2>/dev/null)" = true ] \
    && walker_hosts="$walker_hosts $host"
done

# --- the cases ---------------------------------------------------------------
for host in $walker_hosts; do
  pkgpath=$(eval_raw "$host" home.path)
  text=$(unit_text_for "$host")

  if printf '%s' "$text" | grep -qF "$pkgpath"; then
    found=present
  else
    found=absent
  fi
  check "$host: elephant.service names its package-set path" present "$found"

  check "$host: elephant.service is not marked keep-old" \
    "" "$(printf '%s' "$text" | sed -n 's/^X-SwitchMethod=//p')"
done

# The gate. The trigger is attached by naming systemd.user.services.elephant from
# home.nix, which *creates* that unit if the host does not already have one -- so
# an unguarded assignment would plant a bare elephant.service, with a trigger and
# no ExecStart, on every host that does not run walker (and on darwin, which has
# no systemd at all). The gate has to match walker's own, so these hosts must show
# no elephant unit whatsoever.
for host in $hosts; do
  case " $walker_hosts " in *" $host "*) continue ;; esac
  check "$host (no walker) has no elephant unit at all" false \
    "$(nix eval --json "$FLAKE#homeConfigurations.$host.config.systemd.user.services" \
       --apply 's: builtins.elem "elephant" (builtins.attrNames s)' 2>/dev/null)"
done

# --- the teeth ---------------------------------------------------------------
# Every case above is inside a loop over hosts discovered at runtime, so a config
# that stopped enabling walker anywhere would pass having checked nothing.
n=$((n+1))
if [ -n "$walker_hosts" ]; then
  echo "ok   at least one host runs walker:$walker_hosts"
else
  echo "FAIL no host enables programs.walker: every case above is vacuous"
  fails=$((fails+1))
fi

# The premise. A restart trigger is only the right lever because the directory
# elephant watches is an immutable store path -- that is exactly why its inotify
# watch cannot see a switch, and why nothing short of restarting the process
# re-reads the set. If that directory ever became a mutable one outside the store,
# inotify would work on its own and this whole mechanism would be dead weight
# nobody had a reason to remove. Asserted against the live profile when there is
# one; which store path it is does not matter (see the note above on layouts).
n=$((n+1))
live=$(readlink -f "$HOME/.nix-profile/share/applications" 2>/dev/null || true)
case "$live" in
  /nix/store/*)
    echo "ok   live desktop-entry dir is an immutable store path ($live)"
    ;;
  "")
    echo "ok   no live nix profile here; premise not checked"
    ;;
  *)
    echo "FAIL live desktop-entry dir is '$live', outside /nix/store:"
    echo "     elephant's inotify watch would now see switches by itself, so the"
    echo "     restart trigger is no longer the mechanism this file describes."
    fails=$((fails+1))
    ;;
esac

echo
if [ "$fails" -eq 0 ]; then echo "all $n passed"; else echo "$fails of $n failed"; fi
exit $(( fails > 0 ))
