#!/usr/bin/env bash
# Cases for the `steam` wrapper and home/steam-desktop-override (both wired up in
# home.nix), which together fix what a nix-profile-first PATH does to a
# pacman-installed Steam.
#
# The bug they exist for, measured on shiori 2026-09-28:
#
#   xdg-user-dir: symbol lookup error: /usr/lib/libc.so.6: undefined symbol:
#   __pointer_chk_guard, version GLIBC_PRIVATE
#
# Steam shells out to `xdg-user-dir <key>` to locate a user directory.
# `~/.nix-profile/bin` sits ahead of `/usr/bin` and xdg.userDirs.enable puts nix's
# xdg-user-dirs in the profile, so the call lands on the nix copy -- whose ELF
# interpreter is nix's ld.so, which cannot satisfy the reference Arch's
# /usr/lib/libc.so.6 leaves undefined for its own loader to fill. The wrapper puts
# the host's directories first so Steam gets /usr/bin/xdg-user-dir.
#
# The half that is easy to get wrong is the other direction, which is why it has
# cases of its own: the profile must NOT be dropped from PATH. `xdg-open` exists
# ONLY in the nix profile on this box (Arch's xdg-utils is not installed), and it
# is how Steam opens a link in a browser. A wrapper that sanitised PATH by
# stripping nix would trade a broken download path for broken links.
#
#   ./tests/steam-env.sh                 # builds the wrapper, then tests it
#   ./tests/steam-env.sh /path/to/steam  # tests one you already have
#
# Two seams for driving mutated copies: $1 for the wrapper, and
# $STEAM_DESKTOP_OVERRIDE for the rewrite script.
#
# The wrapper is resolved from homeConfigurations.shiori specifically, not from
# $(cat /etc/hostname): steam is a shiori-only package, and the cases are about the
# wrapper's own logic, so there is no reason they should only run on the one box.
# The flake ref is derived from this script's own location rather than `.#`, so
# running it from a worktree tests THAT tree and not whichever one happens to be
# the cwd.
set -u

REPO=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

SCRIPT="${1:-}"
WRAPPER_STORE=""
if [ -z "$SCRIPT" ]; then
  drv=$(nix eval --raw "$REPO#homeConfigurations.shiori.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: p.name or "" == "steam") ps)).drvPath') \
    || { echo "could not evaluate the steam wrapper for shiori" >&2; exit 1; }
  WRAPPER_STORE="$(nix-store --realise "$drv" | tail -1)"
  SCRIPT="$WRAPPER_STORE/bin/steam"
  echo "testing $SCRIPT"
fi
[ -x "$SCRIPT" ] || { echo "not executable: $SCRIPT" >&2; exit 1; }

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0; skips=0

# The stub stands in for /usr/bin/steam through the wrapper's STEAM_BIN seam and
# reports the environment it was handed. Everything the cases assert is read out of
# this, so a wrapper that sets the right variables but never execs would fail every
# case rather than passing vacuously.
cat > "$D/stub" <<'STUB'
#!/bin/sh
echo "PATH=$PATH"
echo "GIO_EXTRA_MODULES=${GIO_EXTRA_MODULES-<unset>}"
echo "GIO_MODULE_DIR=${GIO_MODULE_DIR-<unset>}"
echo "XDG_DATA_DIRS=${XDG_DATA_DIRS-<unset>}"
echo "ARGC=$#"
for a in "$@"; do echo "ARG=$a"; done
exit "${STUB_RC:-0}"
STUB
chmod +x "$D/stub"

# Every case runs the wrapper with a deliberately hostile parent environment:
# GIO_EXTRA_MODULES set (the thing that must be dropped) and GIO_MODULE_DIR set (a
# neighbour that must survive, so an over-broad `unset GIO_*` fails here).
run() {
  env -i \
    HOME="${HOME:?}" \
    PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/bin:/bin" \
    GIO_EXTRA_MODULES=/nix/store/deadbeef-gvfs/lib/gio/modules \
    GIO_MODULE_DIR=/keep/me \
    XDG_DATA_DIRS=/keep/me/too \
    STEAM_BIN="$D/stub" \
    "$@"
}

ok()  { n=$((n + 1)); printf '  ok       %s\n' "$1"; }
bad() { n=$((n + 1)); fails=$((fails + 1)); printf '  FAILED   %s\n%s\n' "$1" "$2"; }
# skip() counts toward n, so a precondition-dependent case stays in the total instead
# of silently shrinking it. Not a guarantee that the total is always README's number:
# a branch that cannot even evaluate the config emits one skip per case it could not
# reach, and the footer is then honestly smaller.
skip() { n=$((n + 1)); skips=$((skips + 1)); printf '  skipped  %s (%s)\n' "$1" "$2"; }

# want_line <case> <exact line that must appear in the stub's output>
want_line() {
  local case=$1 want=$2 out
  out=$(run "$SCRIPT" 2>&1) || true
  if printf '%s\n' "$out" | grep -qxF "$want"; then ok "$case"
  else bad "$case" "    wanted line: $want
    got:
$(printf '%s\n' "$out" | sed 's/^/      /')"
  fi
}

echo
echo "--- the teeth: the collision this all exists for still exists --------------"

# Without this, every case below can pass on a box where the collision is absent
# (no /usr/bin/xdg-user-dir, or xdg.userDirs.enable dropped) -- the suite would
# report success having checked nothing that matters. tests/steam-ui-scaling.sh
# invented this guard for the same reason; it is the one case that must never skip.
if [ -x /usr/bin/xdg-user-dir ] && [ -x "$HOME/.nix-profile/bin/xdg-user-dir" ]; then
  ok "xdg-user-dir exists in BOTH /usr/bin and the nix profile"
else
  bad "xdg-user-dir exists in BOTH /usr/bin and the nix profile" \
    "    /usr/bin: $([ -x /usr/bin/xdg-user-dir ] && echo yes || echo no), profile: $([ -x "$HOME/.nix-profile/bin/xdg-user-dir" ] && echo yes || echo no)
    the wrapper's whole purpose is this collision; without it the PATH cases below prove nothing"
fi

echo
echo "--- PATH: the host's helpers win -------------------------------------------"

out=$(run "$SCRIPT" 2>&1) || true
path=$(printf '%s\n' "$out" | sed -n 's/^PATH=//p')
usr=$(printf '%s' "$path" | tr ':' '\n' | grep -nxF /usr/bin | head -1 | cut -d: -f1)
prof=$(printf '%s' "$path" | tr ':' '\n' | grep -nxF "$HOME/.nix-profile/bin" | head -1 | cut -d: -f1)
if [ -n "$usr" ] && [ -n "$prof" ] && [ "$usr" -lt "$prof" ]; then
  ok "/usr/bin precedes ~/.nix-profile/bin"
else
  bad "/usr/bin precedes ~/.nix-profile/bin" "    /usr/bin at ${usr:-absent}, profile at ${prof:-absent}
    PATH=$path"
fi

# The other direction, and the one a careless fix breaks. Stripping nix from PATH
# would satisfy the case above and take xdg-open (nix-only here) with it.
if [ -n "$prof" ]; then
  ok "~/.nix-profile/bin is still on PATH (not stripped)"
else
  bad "~/.nix-profile/bin is still on PATH (not stripped)" "    PATH=$path"
fi

# Through readlink, not on the name `command -v` prints: ~/.nix-profile/bin entries
# are symlinks into the store, so the unresolved string is never /nix/store/... and
# an assertion on it passes with the bug still in place.
if [ -x /usr/bin/xdg-user-dir ] && [ -x "$HOME/.nix-profile/bin/xdg-user-dir" ]; then
  got=$(PATH="$path" command -v xdg-user-dir || true)
  real=$(readlink -f "$got" 2>/dev/null || true)
  case "$real" in
    /nix/store/*) bad "xdg-user-dir resolves outside the nix store" "    $got -> $real" ;;
    "") bad "xdg-user-dir resolves outside the nix store" "    did not resolve at all" ;;
    *) ok "xdg-user-dir resolves outside the nix store ($real)" ;;
  esac
else
  skip "xdg-user-dir resolves outside the nix store" "needs it in both /usr/bin and the profile"
fi

# RUNNABILITY, not just resolution. A pure `command -v` check here would be a
# tautology -- it can only fail if the "still on PATH" case above already did. What
# matters is that the nix-only handler actually executes, because nix's xdg-open
# dies exactly like xdg-user-dir did when a Steam-runtime LD_LIBRARY_PATH is in
# force. It works because steam.sh restores SYSTEM_LD_LIBRARY_PATH before handing a
# URL to an external handler, so the env asserted here is the env the handler gets.
if [ -x "$HOME/.nix-profile/bin/xdg-open" ] && [ ! -x /usr/bin/xdg-open ]; then
  # Two runs, because the interesting part is the CONTRAST, and the first version of
  # this case tested only the easy half. Under a clean environment the nix-only handler
  # must load; under a Steam-runtime-shaped LD_LIBRARY_PATH it is EXPECTED to die, and
  # that expectation is what makes prepending (rather than the library path) the thing
  # the wrapper fixes.
  clean_err=$(env -i HOME="$HOME" PATH="$path" xdg-open --manual 2>&1 >/dev/null || true)
  dirty_err=$(env -i HOME="$HOME" PATH="$path" LD_LIBRARY_PATH=/usr/lib \
    xdg-open --manual 2>&1 >/dev/null || true)
  # A loader failure is named explicitly rather than inferred from an exit code:
  # `xdg-open --manual` exits non-zero on a box with no man/pager, which is not this.
  loader_re='symbol lookup error|error while loading shared libraries|cannot execute|No such file or directory'
  if printf '%s' "$clean_err" | grep -qE "$loader_re"; then
    bad "xdg-open (nix-only) loads in a clean env and is what PATH must keep reachable" \
      "    failed to load with no LD_LIBRARY_PATH set: $clean_err"
  elif printf '%s' "$dirty_err" | grep -qE "$loader_re"; then
    ok "xdg-open (nix-only) loads clean, and dies under a Steam-shaped LD_LIBRARY_PATH"
  else
    # Not a failure of the wrapper: it means the nix/host glibc pair stopped colliding,
    # so the premise of the whole change needs re-reading rather than a green tick.
    skip "xdg-open dies under a Steam-shaped LD_LIBRARY_PATH" \
      "it survived /usr/lib -- the glibc collision may be gone; re-read home.nix's comment"
  fi
else
  skip "xdg-open (nix-only) loads in a clean env" "only meaningful when nix has it and /usr/bin does not"
fi

echo
echo "--- GIO: drop the one broken module dir, keep the rest ----------------------"

want_line "GIO_EXTRA_MODULES is unset for the child" "GIO_EXTRA_MODULES=<unset>"
want_line "GIO_MODULE_DIR is left alone" "GIO_MODULE_DIR=/keep/me"
want_line "XDG_DATA_DIRS is left alone" "XDG_DATA_DIRS=/keep/me/too"

echo
echo "--- argv is forwarded verbatim ---------------------------------------------"

out=$(run "$SCRIPT" steam://store 2>&1) || true
if printf '%s\n' "$out" | grep -qxF "ARGC=1" && printf '%s\n' "$out" | grep -qxF "ARG=steam://store"; then
  ok "a steam:// URL is passed through as one argument"
else
  bad "a steam:// URL is passed through as one argument" "$(printf '%s\n' "$out" | sed 's/^/      /')"
fi

out=$(run "$SCRIPT" "two words" 'a$b' 2>&1) || true
if printf '%s\n' "$out" | grep -qxF "ARGC=2" \
  && printf '%s\n' "$out" | grep -qxF "ARG=two words" \
  && printf '%s\n' "$out" | grep -qxF 'ARG=a$b'; then
  ok "spaces and \$ survive unsplit and unexpanded"
else
  bad "spaces and \$ survive unsplit and unexpanded" "$(printf '%s\n' "$out" | sed 's/^/      /')"
fi

want_line "no arguments means no arguments" "ARGC=0"

echo
echo "--- the production exec target ---------------------------------------------"

# Every case above drives the STEAM_BIN seam, so the DEFAULT -- the only value that
# ever runs on the real host -- has no coverage from them at all: changing it to
# /usr/lib/steam/steam, or dropping the `:-`, passes all of them. Executing it for
# real would launch actual Steam, so this asserts the built script's text instead.
if grep -qF '${STEAM_BIN:-/usr/bin/steam}' "$SCRIPT"; then
  ok "the wrapper's default exec target is /usr/bin/steam"
else
  bad "the wrapper's default exec target is /usr/bin/steam" \
    "    not found in $SCRIPT:
$(grep -n 'exec' "$SCRIPT" | sed 's/^/      /')"
fi

echo
echo "--- home.nix wiring --------------------------------------------------------"

# The script and the wrapper can both be perfect while the activation entry passes
# the wrong paths, and nothing above would notice: transpose src and dst and the
# no-Steam branch makes activation succeed having done nothing. Derived from the
# config the way tests/steam-ui-scaling.sh does it, rather than restating the paths.
if data=$(nix eval --raw \
    "$REPO#homeConfigurations.shiori.config.home.activation.steamDesktopOverride.data" 2>/dev/null); then
  # ORDER, not just presence. Checking that each path merely appears somewhere in the
  # blob cannot catch the transposition this case exists for: swap argument 1 and 2
  # and every string is still present, while activation reads the override as its
  # source, finds it absent, and succeeds having done nothing. So require src before
  # dst before wrapper, positionally.
  # Character offsets via awk index(), which needs no regex escaping of store paths.
  # Flattened to one line first: awk's index() is per-record, and the arguments sit on
  # separate continuation lines, so per-line offsets would compare nothing.
  flat=$(printf '%s' "$data" | tr '\n' ' ')
  at() { printf '%s' "$flat" | awk -v s="$1" '{ print index($0, s) }'; }
  # dataHome from the config, not $HOME/.local/share by hand, so a non-default
  # xdg.dataHome cannot fail this case for a reason unrelated to ordering.
  dh=$(nix eval --raw "$REPO#homeConfigurations.shiori.config.xdg.dataHome" 2>/dev/null) \
    || dh="$HOME/.local/share"
  src_at=$(at /usr/share/applications/steam.desktop)
  dst_at=$(at "$dh/applications/steam.desktop")
  exe_at=$(at "$WRAPPER_STORE/bin/steam")
  # `-gt 0`, not `-n`: awk's index() prints 0 when the needle is ABSENT, and "0" is a
  # non-empty string -- so `-n` guards were dead, and a src argument that had been
  # dropped or renamed scored offset 0 and sailed through as "earliest".
  if [ -z "$WRAPPER_STORE" ]; then
    skip "activation passes src, then dst, then the wrapper -- in that order" \
      "wrapper was supplied as \$1, so its store path is unknown"
  elif [ "$src_at" -gt 0 ] && [ "$dst_at" -gt 0 ] && [ "$exe_at" -gt 0 ] \
    && [ "$src_at" -lt "$dst_at" ] && [ "$dst_at" -lt "$exe_at" ]; then
    ok "activation passes src, then dst, then the wrapper -- in that order"
  else
    bad "activation passes src, then dst, then the wrapper -- in that order" \
      "    offsets: src ${src_at:-absent}, dst ${dst_at:-absent}, wrapper ${exe_at:-absent}
    data: $data"
  fi

  # `|| warnEcho`, not just `warnEcho`: the operator IS the policy. A mutation to
  # `; warnEcho "..."` still prints the warning and still aborts activation under
  # `set -e`, so a bare substring check would score it green.
  if printf '%s' "$data" | grep -qF '|| warnEcho'; then
    ok "activation entry keeps its || warnEcho"
  else
    bad "activation entry keeps its || warnEcho" \
      "    absent: a bare failure here would abort the activation entries after it"
  fi

  # The third argument must be the same derivation home.packages installs, or the
  # launcher points at a wrapper nobody has.
  if [ -n "$WRAPPER_STORE" ]; then
    if printf '%s' "$data" | grep -qF "$WRAPPER_STORE/bin/steam"; then
      ok "activation points at the same wrapper home.packages installs"
    else
      bad "activation points at the same wrapper home.packages installs" \
        "    expected $WRAPPER_STORE/bin/steam"
    fi
  else
    skip "activation points at the same wrapper home.packages installs" "wrapper was supplied as \$1"
  fi

  # C1's failure policy: the entry runs under `set -eu` before zenInstallsIni, so a
  # bare non-zero call would abort the rest of activation.
  if after=$(nix eval --json \
      "$REPO#homeConfigurations.shiori.config.home.activation.steamDesktopOverride.after" 2>/dev/null) \
     && printf '%s' "$after" | grep -qF linkGeneration; then
    ok "activation entry is ordered after linkGeneration"
  else
    bad "activation entry is ordered after linkGeneration" "    after = ${after:-<eval failed>}"
  fi
else
  # One skip per case the success branch would have emitted, so a config that cannot
  # be evaluated shrinks the pass count instead of shrinking the total silently.
  skip "activation passes src, then dst, then the wrapper" "could not evaluate the activation entry"
  skip "activation entry keeps its || warnEcho" "could not evaluate the activation entry"
  skip "activation points at the same wrapper home.packages installs" "could not evaluate the activation entry"
  skip "activation entry is ordered after linkGeneration" "could not evaluate the activation entry"
fi

echo
echo "--- home/steam-desktop-override: Exec= rewrite ------------------------------"

# STEAM_DESKTOP_OVERRIDE is the same kind of seam as the wrapper's $1: it exists so
# a mutated copy can be driven through these cases without editing the tracked script.
OVERRIDE="${STEAM_DESKTOP_OVERRIDE:-$REPO/home/steam-desktop-override}"
# A missing script drops every case below rather than skipping them one by one, so the
# footer's total shrinks sharply. That is deliberate -- it is already a hard failure,
# and enumerating thirty skips under it would bury the one line that matters.
if [ ! -r "$OVERRIDE" ]; then
  bad "home/steam-desktop-override exists" "    not found at $OVERRIDE"
else
  # A fixture with the shape that matters: the main Exec= with %U, an action's Exec=
  # with a steam:// argument, a translated Name= that must come through untouched,
  # and two lookalikes that must NOT be rewritten.
  cat > "$D/steam.desktop" <<'FIXTURE'
[Desktop Entry]
Name=Steam
Comment[ja]=Steam 上でゲームを管理＆プレイするためのアプリケーション
Exec=/usr/bin/steam %U
Icon=steam
Actions=Store;
X-Not-Exec=/usr/bin/steam %U

[Desktop Action Store]
Name[uk]=Крамниця
Exec=/usr/bin/steam steam://store

[Desktop Action Bare]
Exec=/usr/bin/steam

[Desktop Action Lookalike]
Exec=/usr/bin/steamfoo --wat
FIXTURE

  # Deliberately NOT a store dir ending in `-steam`. The first version of the cleanup
  # predicate matched `^Exec=/nix/store/[^/]*-steam/bin/steam`, which a fixture ending
  # in `-steam` would satisfy by luck -- so the marker that replaced it would look
  # equally good. This name makes the difference observable.
  W=/nix/store/fake-steamwrapper/bin/steam
  out="$D/out/applications/steam.desktop"   # a directory that does not exist yet
  if bash "$OVERRIDE" "$D/steam.desktop" "$out" "$W" 2>"$D/err"; then
    if [ -f "$out" ]; then ok "rewrite succeeds and creates the destination directory"
    else bad "rewrite succeeds and creates the destination directory" "    exited 0 but $out does not exist"; fi
  else
    bad "rewrite succeeds and creates the destination directory" "$(sed 's/^/      /' "$D/err")"
  fi

  chk() { # chk <case> <expected exact line>
    if grep -qxF "$2" "$out" 2>/dev/null; then ok "$1"
    else bad "$1" "    wanted: $2"; fi
  }
  chk "main Exec= is rewritten, %U kept"           "Exec=$W %U"
  chk "an action's steam:// argument is kept"      "Exec=$W steam://store"
  chk "a bare Exec= with no argument is rewritten" "Exec=$W"
  chk "a translated Name= is untouched"            "Name[uk]=Крамниця"
  chk "a translated Comment= is untouched"         "Comment[ja]=Steam 上でゲームを管理＆プレイするためのアプリケーション"
  chk "X-Not-Exec= is not rewritten"               "X-Not-Exec=/usr/bin/steam %U"
  chk "Exec=/usr/bin/steamfoo is not rewritten"    "Exec=/usr/bin/steamfoo --wat"

  # Nothing but a rewritten Exec= may move. The exclusion patterns are anchored on
  # the RIGHT too (trailing space / end-of-line), because an unanchored
  # `Exec=/usr/bin/steam` also swallows the steamfoo lookalike -- which would make
  # this case blind to exactly the widening it is here to catch.
  # awk with exact string comparison rather than a regex: $W is a path, and escaping
  # it for a regex is how the right-anchoring silently stops working.
  changed=$(diff "$D/steam.desktop" "$out" | awk -v w="$W" '
    /^[<>] / {
      line = substr($0, 3)
      if (line == "Exec=/usr/bin/steam" || index(line, "Exec=/usr/bin/steam ") == 1) next
      if (line == "Exec=" w        || index(line, "Exec=" w " ")        == 1) next
      # The marker the script prepends to mark the file as its own.
      if (index(line, "# generated by dotfiles home/steam-desktop-override") == 1) next
      c++
    }
    END { print c + 0 }')
  if [ "$changed" = 0 ]; then ok "no line other than a rewritten Exec= differs"
  else bad "no line other than a rewritten Exec= differs" "$(diff "$D/steam.desktop" "$out" | sed 's/^/      /')"; fi

  if mode=$(stat -c '%a' "$out" 2>/dev/null); then
    if [ "$mode" = 644 ]; then ok "the override is mode 0644"
    else bad "the override is mode 0644" "    got $mode"; fi
  else
    skip "the override is mode 0644" "no GNU stat -c here"
  fi

  echo
  echo "--- the rewrite's refusals -------------------------------------------------"

  # THE silent-failure case. If upstream ever stops using Exec=/usr/bin/steam, sed
  # matches nothing and a verbatim copy gets installed at a path that OUTRANKS
  # /usr/share -- so the launcher bypasses the wrapper and the original crash comes
  # back, behind a file that looks like the fix. Must refuse, not succeed.
  printf '[Desktop Entry]\nName=Steam\nExec=steam %%U\n' > "$D/newshape.desktop"
  if bash "$OVERRIDE" "$D/newshape.desktop" "$D/newshape-out" "$W" 2>"$D/err2"; then
    bad "a source with no matching Exec= is refused" "    exited 0; installed $(cat "$D/newshape-out" 2>/dev/null | tr '\n' '|')"
  elif [ -e "$D/newshape-out" ]; then
    bad "a source with no matching Exec= is refused" "    refused but still wrote the file"
  else
    ok "a source with no matching Exec= is refused, and writes nothing"
  fi

  # An empty or truncated source is the same failure wearing different clothes: no
  # Exec= line survives, so the post-condition above is what refuses it. Kept as a
  # case because it is a distinct input worth pinning, not because it exercises a
  # distinct guard -- the separate emptiness check it used to have was dead code.
  : > "$D/empty.desktop"
  if bash "$OVERRIDE" "$D/empty.desktop" "$D/empty-out" "$W" 2>/dev/null; then
    bad "an empty source is refused" "    exited 0"
  elif [ -e "$D/empty-out" ]; then
    bad "an empty source is refused" "    refused but still wrote the file"
  else
    ok "an empty source is refused, and writes nothing"
  fi

  # $exe reaches both a sed replacement and a Desktop Entry Exec= field, whose
  # metacharacters differ. Only the SPACE case is caught by the up-front validation
  # and nothing else: `|` makes sed die and `&` reinserts the match, which the
  # post-condition then refuses. All three must end non-zero either way.
  for badexe in '/nix/a&b/steam' '/nix/a|b/steam' '/nix/a b/steam'; do
    if bash "$OVERRIDE" "$D/steam.desktop" "$D/badexe-out" "$badexe" 2>/dev/null; then
      bad "a wrapper path containing '$badexe' is refused" "    exited 0"
    else
      ok "a wrapper path containing shell/sed metacharacters is refused ($badexe)"
    fi
    rm -f "$D/badexe-out"
  done

  # A PARTIAL rewrite is the realistic upstream drift and the dangerous one: the real
  # file has ten Exec= lines, and an update that changed only the nine Desktop Action
  # ones would leave every right-click action unwrapped while the main entry made a
  # "did anything get rewritten?" check pass.
  printf '[Desktop Entry]\nExec=/usr/bin/steam %%U\n\n[Desktop Action Store]\nExec=/usr/bin/steam\t--odd\n' \
    > "$D/partial.desktop"
  if bash "$OVERRIDE" "$D/partial.desktop" "$D/partial-out" "$W" 2>/dev/null; then
    bad "a partial rewrite is refused" "    exited 0; installed $(grep -c . "$D/partial-out" 2>/dev/null) lines with a survivor"
  elif [ -e "$D/partial-out" ]; then
    bad "a partial rewrite is refused" "    refused but still wrote the file"
  else
    ok "a partial rewrite -- some Exec= lines left unreached -- is refused"
  fi

  # A directory as source is an anomaly, not "this host has no Steam": it must not be
  # able to take the quiet branch (and so delete a working override), nor fall through
  # to sed.
  mkdir -p "$D/srcdir"
  if bash "$OVERRIDE" "$D/srcdir" "$D/srcdir-out" "$W" 2>/dev/null; then
    bad "a directory as source is refused, not treated as no-Steam" "    exited 0"
  elif [ -e "$D/srcdir-out" ]; then
    bad "a directory as source is refused, not treated as no-Steam" "    wrote a file"
  else
    ok "a directory as source is refused, not treated as no-Steam"
  fi

  mkdir -p "$D/dstdir"
  if bash "$OVERRIDE" "$D/steam.desktop" "$D/dstdir" "$W" 2>/dev/null; then
    bad "a directory as destination is refused" "    exited 0; mv would have hidden a file inside it"
  else
    ok "a directory as destination is refused"
  fi

  echo
  echo "--- the no-Steam branch cleans up after itself ------------------------------"

  # If Steam leaves system/packages.shiori, the source stops being readable and this
  # branch runs forever. Leaving the override behind is the worst shape available:
  # ~/.local/share outranks /usr/share, so a dead Exec= keeps shadowing the packaged
  # entry, and after GC the launcher entry still looks right and silently does nothing.
  # Keyed on the file the script ITSELF produced above, not on a hand-written guess at
  # what its output looks like. A fixture would let the recognition predicate and the
  # written file drift apart -- which is exactly how the first version of this broke,
  # by matching on `-steam` in the store path and so failing the moment the wrapper
  # derivation were renamed.
  cp "$out" "$D/stale"
  if bash "$OVERRIDE" "$D/absent.desktop" "$D/stale" "$W" 2>"$D/err7"; then
    # And it must say so: this is the one state change here that is not a failure, so
    # the caller's `|| warnEcho` never fires and stderr is the only notice there is.
    if [ ! -e "$D/stale" ] && [ -s "$D/err7" ]; then
      ok "an override this script wrote is removed when Steam goes away, and says so"
    else
      bad "an override this script wrote is removed when Steam goes away, and says so" \
        "    removed: $([ ! -e "$D/stale" ] && echo yes || echo no), said: $(cat "$D/err7")"
    fi
  else
    bad "an override this script wrote is removed when Steam goes away, and says so" "    exited non-zero"
  fi

  # ...but only one of ours. A hand-written steam.desktop is not ours to delete.
  printf '[Desktop Entry]\nName=Steam\nExec=/usr/bin/steam %%U\n' > "$D/handmade"
  if bash "$OVERRIDE" "$D/absent.desktop" "$D/handmade" "$W" 2>/dev/null; then
    if [ -e "$D/handmade" ]; then ok "a hand-written steam.desktop is left alone"
    else bad "a hand-written steam.desktop is left alone" "    deleted a file we did not write"; fi
  else
    bad "a hand-written steam.desktop is left alone" "    exited non-zero"
  fi

  rm -f "$D/never"
  if bash "$OVERRIDE" "$D/absent.desktop" "$D/never" "$W" 2>"$D/err3"; then
    if [ ! -e "$D/never" ] && [ ! -s "$D/err3" ]; then
      ok "an absent source with nothing to clean exits 0, writes nothing, says nothing"
    else
      bad "an absent source with nothing to clean exits 0, writes nothing, says nothing" \
        "    created: $([ -e "$D/never" ] && echo yes || echo no), stderr: $(cat "$D/err3")"
    fi
  else
    bad "an absent source with nothing to clean exits 0, writes nothing, says nothing" "    exited non-zero"
  fi

  echo
  echo "--- re-running, which is what every hms after the first does ----------------"

  # The exit status is checked, not discarded: this is the only case covering
  # "destination already exists", and if the second run failed outright the file
  # would still be byte-identical and a content-only assertion would report ok.
  before=$(cat "$out")
  if bash "$OVERRIDE" "$D/steam.desktop" "$out" "$W" 2>"$D/err4"; then
    if [ "$before" = "$(cat "$out")" ]; then ok "re-running over an existing override is idempotent"
    else bad "re-running over an existing override is idempotent" "    second run changed the file"; fi
  else
    bad "re-running over an existing override is idempotent" "$(sed 's/^/      /' "$D/err4")"
  fi

  # A SIGKILL mid-switch skips the EXIT trap; nothing else ever sweeps the residue,
  # and a leftover is invisible to XDG scanners but permanent. The exit status is
  # checked and $out asserted to survive, so this cannot pass on a run that failed.
  touch "$(dirname "$out")/steam.desktop.hm-ABCDEF"
  if bash "$OVERRIDE" "$D/steam.desktop" "$out" "$W" 2>"$D/err5"; then
    leftovers=$(find "$(dirname "$out")" -name 'steam.desktop.hm-??????' | wc -l)
    if [ "$leftovers" = 0 ] && [ -f "$out" ]; then ok "a leftover temp from a killed run is swept"
    else bad "a leftover temp from a killed run is swept" "    leftovers=$leftovers, out present=$([ -f "$out" ] && echo yes || echo no)"; fi
  else
    bad "a leftover temp from a killed run is swept" "$(sed 's/^/      /' "$D/err5")"
  fi

  # ...but a deliberate copy is not residue. The sweep used a bare `.??????`, which is
  # exactly six characters and therefore also matched steam.desktop.backup.
  touch "$(dirname "$out")/steam.desktop.backup"
  if bash "$OVERRIDE" "$D/steam.desktop" "$out" "$W" 2>"$D/err9"; then
    if [ -e "$(dirname "$out")/steam.desktop.backup" ]; then
      ok "a user's steam.desktop.backup is not swept"
    else
      bad "a user's steam.desktop.backup is not swept" "    the temp glob ate a real file"
    fi
  else
    # Status checked, or this passes on any regression that fails before the sweep.
    bad "a user's steam.desktop.backup is not swept" "$(sed 's/^/      /' "$D/err9")"
  fi

  echo
  echo "--- the produced file is a real desktop entry, against the real input -------"

  # Everything above asserts on a fixture with grep and diff, so nothing had ever fed
  # the script the actual pacman file or asked a parser whether the result is valid.
  # The marker is the reason this matters: it puts a `#` comment BEFORE
  # [Desktop Entry], and if that were invalid every launch would break silently while
  # all the grep-based cases stayed green.
  if [ -r /usr/share/applications/steam.desktop ]; then
    if bash "$OVERRIDE" /usr/share/applications/steam.desktop "$D/real.desktop" "$W" 2>"$D/err8"; then
      # Derived from the input, not hard-coded: the count is upstream's to change.
      want=$(grep -c '^Exec=/usr/bin/steam' /usr/share/applications/steam.desktop)
      got=$(grep -c "^Exec=$W" "$D/real.desktop")
      if [ "$want" = "$got" ]; then
        ok "every Exec= in the real steam.desktop is rewritten ($got of $want)"
      else
        bad "every Exec= in the real steam.desktop is rewritten" "    rewrote $got of $want"
      fi
    else
      bad "every Exec= in the real steam.desktop is rewritten" "$(sed 's/^/      /' "$D/err8")"
    fi

    if command -v desktop-file-validate >/dev/null 2>&1; then
      # Compared against the packaged original rather than demanding silence: upstream
      # ships a Categories hint and a deprecated X-KDE- key, and inheriting those is
      # correct behaviour. What must not happen is the override adding a complaint.
      ours=$(desktop-file-validate "$D/real.desktop" 2>&1 | sed "s|$D/real.desktop||" | sort)
      theirs=$(desktop-file-validate /usr/share/applications/steam.desktop 2>&1 \
        | sed 's|/usr/share/applications/steam.desktop||' | sort)
      if [ "$ours" = "$theirs" ]; then
        ok "desktop-file-validate says no more about ours than about the packaged file"
      else
        bad "desktop-file-validate says no more about ours than about the packaged file" \
          "    ours:   $ours
    theirs: $theirs"
      fi
    else
      skip "desktop-file-validate agrees" "desktop-file-utils not installed"
    fi
  else
    skip "every Exec= in the real steam.desktop is rewritten" "no /usr/share/applications/steam.desktop"
    skip "desktop-file-validate agrees" "no /usr/share/applications/steam.desktop"
  fi

  echo
  echo "--- a refusal must not leave a dead override shadowing the packaged entry ----"

  # The state the absent-source branch calls the worst shape available, reached by a
  # different road: upstream drifts, every later hms refuses, and $dst keeps an Exec=
  # naming a wrapper that garbage collection will remove.
  cp "$out" "$D/doomed"
  if bash "$OVERRIDE" "$D/newshape.desktop" "$D/doomed" "$W" 2>/dev/null; then
    bad "a refusal drops our own stale override" "    exited 0"
  elif [ -e "$D/doomed" ]; then
    bad "a refusal drops our own stale override" "    refused but left the dead override in place"
  else
    ok "a refusal drops our own stale override, so the packaged entry takes over"
  fi

  # ...but a config error is not upstream drift: the override in place is still valid,
  # so a rejected $exe must leave it alone.
  cp "$out" "$D/keepme"
  bash "$OVERRIDE" "$D/steam.desktop" "$D/keepme" '/nix/a b/steam' 2>/dev/null || true
  if [ -e "$D/keepme" ]; then
    ok "a rejected wrapper path leaves the existing override alone"
  else
    bad "a rejected wrapper path leaves the existing override alone" "    deleted a working override"
  fi

  echo
  echo "--- mv is rename(2), which is why a store symlink at \$dst is safe -----------"

  # The script header argues this specifically: rename replaces the destination NAME,
  # so a leftover home-manager symlink into the read-only store is replaced rather
  # than written *through*. Untested, an `mv` -> `cp` change passes every other case
  # while turning this into a write into /nix/store (EROFS) or, worse, through the
  # link. Pointed at a real store path so the read-only property is genuine.
  store_target=$(readlink -f "$SCRIPT")
  ln -sf "$store_target" "$D/out/applications/symlinked.desktop"
  if bash "$OVERRIDE" "$D/steam.desktop" "$D/out/applications/symlinked.desktop" "$W" 2>"$D/err6"; then
    if [ ! -L "$D/out/applications/symlinked.desktop" ] \
      && [ -f "$D/out/applications/symlinked.desktop" ] \
      && grep -qxF "Exec=$W %U" "$D/out/applications/symlinked.desktop" \
      && [ "$(readlink -f "$store_target")" = "$store_target" ] \
      && grep -qF 'STEAM_BIN' "$store_target"; then
      ok "a store symlink at \$dst is replaced, not written through"
    else
      bad "a store symlink at \$dst is replaced, not written through" \
        "    still a symlink: $([ -L "$D/out/applications/symlinked.desktop" ] && echo yes || echo no)"
    fi
  else
    bad "a store symlink at \$dst is replaced, not written through" "$(sed 's/^/      /' "$D/err6")"
  fi
fi

echo
echo "--- the wrapper under an environment with no PATH at all -------------------"

# home.nix claims `set -u` cannot trip on the $PATH reference because bash supplies a
# compiled-in default. Asserted rather than trusted, since it is the difference
# between a launcher that passes no PATH getting Steam and getting exit 1.
out=$(env -i STEAM_BIN="$D/stub" "$SCRIPT" 2>&1) || true
if printf '%s\n' "$out" | grep -q '^PATH=/usr/local/bin:/usr/bin:/bin'; then
  ok "an environment with no PATH still reaches the exec, with the prepend intact"
else
  bad "an environment with no PATH still reaches the exec, with the prepend intact" \
    "$(printf '%s\n' "$out" | sed 's/^/      /')"
fi

echo
[ "$skips" -gt 0 ] && echo "$skips of $n cases skipped"
if [ "$fails" -eq 0 ]; then
  echo "all $n cases passed"
else
  echo "$fails of $n cases FAILED"
fi
exit $((fails > 0))
