#!/usr/bin/env bash
# Host-package helpers for ./deploy. Sourced, never executed: sourcing is what
# lets ../tests/system-packages.sh drive these against a temp tree instead of
# /etc. Nothing here has an effect at source time, and every path is an
# argument -- no function reaches for a location of its own.
#
# SUDO is the privilege seam, and it is `${SUDO-sudo}`, not `${SUDO:-sudo}`:
# the tests set it to the empty string to mean "write as me", which the :-
# form would quietly turn back into sudo.

# Packages wanted on this host: the shared ./packages plus the optional
# per-host ./packages.<host>, which is how a box gets something the others
# have no business installing (steam on shiori and not on headless fujiwara).
#
# The format is one package per line with # comments, and the split is on any
# whitespace rather than per line: a two-names-on-one-line slip then fails as
# two packages pacman reports missing, instead of one concatenated name that
# silently never installs. Deduped, order preserved -- a name in both lists
# reaches pacman once, so the "installing missing" line stays readable.
#
# A missing per-host file is the normal case, not an error. The shared list's
# existence is ./deploy's check, because only deploy knows it is fatal there.
pacman_wants() { # pacman_wants <dir> <host>
  local dir=$1 host=$2 f
  for f in "$dir/packages" "$dir/packages.$host"; do
    # Quoted, so a host name containing a glob character is looked up
    # literally rather than expanded against the directory.
    [ -e "$f" ] || continue
    sed 's/#.*//' -- "$f"
  done | tr -s '[:space:]' '\n' | sed '/^$/d' | awk '!seen[$0]++'
}

# Append <line> to <conf> unless it is already there, and say so when it does.
# The one place this repo writes to a pacman-owned file: /etc/pacman.conf is a
# pacman backup file, so it cannot live under ./etc the way our override files
# do, and enabling a repo needs exactly one line in it. Everything that line
# pulls in stays in the repo, under ./etc/pacman.d/conf.d/.
#
#   • -qxF: whole line, fixed string. The stock pacman.conf ships commented
#     example Include lines, and a substring match would read one as "already
#     enabled" and never enable anything.
#   • the leading \n: an /etc file edited by hand may end without a newline,
#     and a blind append would glue the Include onto that last line. A blank
#     line in pacman.conf is inert.
#   • the echo: deploy's output is where a reader learns a root-owned file
#     outside this repo was edited. Silence on the no-op run is the point.
pacman_ensure_include() { # pacman_ensure_include <conf> <line>
  local conf=$1 line=$2
  grep -qxF -- "$line" "$conf" 2>/dev/null && return 0
  printf '\n%s\n' "$line" | ${SUDO-sudo} tee -a -- "$conf" >/dev/null
  echo "enabled in ${conf}: ${line}"
}

# Does <dbdir> hold a sync db for <repo>? Enabling a repo does not fetch one,
# and `pacman -S` against a repo with no db answers "target not found" -- which
# reads as a bad package name rather than as "you never synced". deploy checks
# this so it can say the real thing instead. It deliberately does not fix it:
# `pacman -Sy` without the -u is the partial-upgrade footgun deploy avoids
# everywhere else, so the cure is a human running `pacman -Syu`.
pacman_repo_synced() { # pacman_repo_synced <dbdir> <repo>
  [ -e "$1/$2.db" ]
}
