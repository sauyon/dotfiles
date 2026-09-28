#!/usr/bin/env bash
# Cases for system/pacman.sh, the pieces of system/deploy's host-package
# convergence whose logic is a model of something else's rules:
#
#   pacman_wants          -- the list format (comments, blanks, whitespace
#                            splitting) plus the per-host overlay, which decides
#                            what `pacman -S` is handed on each box;
#   pacman_ensure_include -- the one line appended to /etc/pacman.conf, a
#                            pacman-owned file we otherwise never touch. It must
#                            match whole lines (the stock config ships the same
#                            text commented out) and must never run twice.
#
# Both are sourced, not executed, so every case runs against the real functions
# in a temp tree. The privilege seam is SUDO: deploy leaves it at `sudo`, the
# cases set it empty so appends land in a temp file as the invoking user.
#
# The last group stops modelling and asks pacman itself, against a copy of this
# host's /etc/pacman.conf and the repo's real drop-in, because the design rests
# on a claim about pacman's parser: that Include takes a glob, and that a
# [multilib] section reached through one registers as a repo.
#
#   ./tests/system-packages.sh                  # tests ../system/pacman.sh
#   ./tests/system-packages.sh /path/to/pacman.sh
#
# Nothing here writes to /etc or needs root; /etc/pacman.conf is read once, to
# be copied into the temp tree.
set -u

LIB="${1:-$(dirname "$0")/../system/pacman.sh}"
[ -r "$LIB" ] || { echo "not readable: $LIB" >&2; exit 1; }
echo "testing $LIB"
# shellcheck source=../system/pacman.sh
. "$LIB"

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0
SUDO=

check() { # check <name> <expected> <actual>
  local name=$1 want=$2 got=$3
  n=$((n+1))
  if [ "$got" = "$want" ]; then
    printf 'ok %d - %s\n' "$n" "$name"
  else
    printf 'FAIL %d - %s\n      want: [%s]\n      got:  [%s]\n' "$n" "$name" "$want" "$got"
    fails=$((fails+1))
  fi
}

# ── pacman_wants ─────────────────────────────────────────────────────────────

mkdir -p "$D/pkgs"
cat > "$D/pkgs/packages" <<'EOF'
# a comment
btrfs-progs

zsh   # trailing comment
EOF

check "shared list: comments and blanks dropped" \
  "btrfs-progs zsh" \
  "$(pacman_wants "$D/pkgs" nosuchhost | tr '\n' ' ' | sed 's/ $//')"

cat > "$D/pkgs/packages.shiori" <<'EOF'
# host-only
steam
lib32-mesa
EOF

check "per-host overlay appends after the shared list" \
  "btrfs-progs zsh steam lib32-mesa" \
  "$(pacman_wants "$D/pkgs" shiori | tr '\n' ' ' | sed 's/ $//')"

check "another host sees only the shared list" \
  "btrfs-progs zsh" \
  "$(pacman_wants "$D/pkgs" utsuho | tr '\n' ' ' | sed 's/ $//')"

# The documented format is one package per line. Splitting on whitespace (rather
# than trusting the line) makes a two-names-on-one-line slip fail as two
# packages pacman reports as one missing each, not as one concatenated name that
# would silently never install.
printf 'a b\n' > "$D/pkgs/packages.split"
check "two names on one line split into two" \
  "btrfs-progs zsh a b" \
  "$(pacman_wants "$D/pkgs" split | tr '\n' ' ' | sed 's/ $//')"

# A package named in both lists must reach pacman once: the "installing
# missing" line is read by a human, and -Qq would be asked twice for nothing.
printf 'zsh\nsteam\n' > "$D/pkgs/packages.dup"
check "a name in both lists is emitted once" \
  "btrfs-progs zsh steam" \
  "$(pacman_wants "$D/pkgs" dup | tr '\n' ' ' | sed 's/ $//')"

printf '# nothing but a comment\n' > "$D/pkgs/packages.bare"
check "a comment-only host file adds nothing" \
  "btrfs-progs zsh" \
  "$(pacman_wants "$D/pkgs" bare | tr '\n' ' ' | sed 's/ $//')"

# A host whose name would glob or traverse must not reach past the list dir.
check "host name is used literally, not as a pattern" \
  "btrfs-progs zsh" \
  "$(pacman_wants "$D/pkgs" '*' | tr '\n' ' ' | sed 's/ $//')"

# ── pacman_ensure_include ────────────────────────────────────────────────────

INC='Include = /etc/pacman.d/conf.d/*.conf'

printf '[options]\nHoldPkg = pacman\n' > "$D/pacman.conf"
pacman_ensure_include "$D/pacman.conf" "$INC" >/dev/null
check "absent -> appended" 1 "$(grep -cxF -- "$INC" "$D/pacman.conf")"

pacman_ensure_include "$D/pacman.conf" "$INC" >/dev/null
pacman_ensure_include "$D/pacman.conf" "$INC" >/dev/null
check "idempotent across further runs" 1 "$(grep -cxF -- "$INC" "$D/pacman.conf")"

check "the pre-existing config is left intact" \
  "HoldPkg = pacman" "$(grep -F HoldPkg "$D/pacman.conf")"

# Stock pacman.conf carries commented example Include lines; a substring match
# would read one as "already enabled" and quietly never enable anything.
printf '[options]\n#%s\n' "$INC" > "$D/commented.conf"
pacman_ensure_include "$D/commented.conf" "$INC" >/dev/null
check "a commented-out copy does not count as present" \
  1 "$(grep -cxF -- "$INC" "$D/commented.conf")"

# An /etc file edited by hand may have no trailing newline. Appending blind
# would glue the Include onto that last line, where pacman reads neither.
printf '[options]\nHoldPkg = pacman' > "$D/nonewline.conf"
pacman_ensure_include "$D/nonewline.conf" "$INC" >/dev/null
check "no trailing newline -> still a line of its own" \
  1 "$(grep -cxF -- "$INC" "$D/nonewline.conf")"
check "no trailing newline -> the last line survives" \
  1 "$(grep -cxF -- 'HoldPkg = pacman' "$D/nonewline.conf")"

# It says what it did, once: deploy's output is the only place a reader learns
# that a root-owned file outside this repo was edited.
printf '[options]\n' > "$D/quiet.conf"
check "appending is announced" 1 \
  "$(pacman_ensure_include "$D/quiet.conf" "$INC" | grep -c . )"
check "a no-op says nothing" 0 \
  "$(pacman_ensure_include "$D/quiet.conf" "$INC" | grep -c . )"

# ── the whole multilib block, through pacman's own parser ────────────────────
# Everything above is this repo's model of pacman.conf. This case asks pacman
# instead: a copy of the host's real /etc/pacman.conf, the repo's real drop-in,
# and the real Include line -- only the paths repointed at the temp tree, so
# nothing under /etc is read for anything but that copy, and nothing is written
# there at all. What it pins is the load-bearing assumption of the whole design:
# that pacman expands a glob in Include, and that a [multilib] section in an
# included file registers as a repo.
#
# Skipped where pacman-conf or /etc/pacman.conf is absent (mari), where there is
# nobody to ask.
if command -v pacman-conf >/dev/null && [ -r /etc/pacman.conf ]; then
  DROPIN="$(dirname "$LIB")/etc/pacman.d/conf.d/multilib.conf"
  mkdir -p "$D/conf.d"
  cp /etc/pacman.conf "$D/real.conf"
  cp "$DROPIN" "$D/conf.d/multilib.conf"
  pacman_ensure_include "$D/real.conf" "Include = $D/conf.d/*.conf" >/dev/null
  check "pacman reads the drop-in and registers multilib" \
    1 "$(pacman-conf --config "$D/real.conf" --repo-list | grep -cx multilib)"
  # A repo with no mirrors is enabled in name only; the drop-in's own Include
  # of the mirrorlist has to resolve too.
  check "multilib resolves to real mirrors" \
    yes "$([ "$(pacman-conf --config "$D/real.conf" --repo=multilib Server 2>/dev/null | wc -l)" -gt 0 ] && echo yes || echo no)"
else
  echo "skip - pacman-conf or /etc/pacman.conf absent; not asking pacman"
fi

# ── pacman_repo_synced ───────────────────────────────────────────────────────
# Enabling a repo does not fetch its db, and `pacman -S` against a repo with no
# db answers "target not found" -- which would read as a broken package name
# rather than as "run pacman -Syu". deploy warns instead, so this has to be
# detectable without refreshing anything.

mkdir -p "$D/sync"
check "no db -> not synced" 1 "$(pacman_repo_synced "$D/sync" multilib; echo $?)"
: > "$D/sync/multilib.db"
check "db present -> synced" 0 "$(pacman_repo_synced "$D/sync" multilib; echo $?)"
check "a different repo is still unsynced" \
  1 "$(pacman_repo_synced "$D/sync" extra; echo $?)"
check "a missing sync dir is not fatal" \
  1 "$(pacman_repo_synced "$D/nosuchdir" multilib; echo $?)"

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
