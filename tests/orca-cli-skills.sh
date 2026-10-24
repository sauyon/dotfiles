#!/usr/bin/env bash
# Cases for the claims orca.nix makes about the Orca CLI and its agent skills:
# that `orca-ide` and `orca` are the CLI and run from any shell, not only an
# Orca terminal; that the GUI is `orca-ide-app` and the CLI can launch it; and
# that the skills in ~/.claude/skills are the bytes this Orca build expects.
#
# Why this needs a test. Orca's own installer only reaches an Orca terminal: it
# puts a shim dir on PATH there (ORCA_CLI_BIN_DIR) and symlinks
# ~/.local/bin/orca-ide at a raw /nix/store path, a script that runs Electron
# against the host's libraries. The skills are stubs that tell the agent to
# run `orca-ide skills get <name>` outside an Orca terminal, so a skill without
# a working CLI under that name is a dead end. And the skills are pinned to a
# commit separately from the AppImage version; bumping one without the other
# silently installs guides for a different CLI. Orca ships the expected hashes
# in resources/skills/current-manifest.json, so that is what the files are
# held to.
#
# Usage:
#   tests/orca-cli-skills.sh    eval + build this host's config and test it

set -u
set -o pipefail

fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }
T=$(mktemp -d); trap 'rm -rf "$T"' EXIT

FLAKE=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
# /etc/hostname is what CI writes to say which host it is; a bwrap-era Orca
# terminal has none, so fall back to uname -n there.
host=$(cat /etc/hostname 2>/dev/null); HM="$FLAKE#homeConfigurations.${host:-$(uname -n)}.config"
SKILLS="orca-cli orchestration computer-use orca-per-workspace-env orca-linear linear-tickets"

pkg_attr() { # pkg_attr <pname> <attr>
  nix eval --raw "$HM.home.packages" \
    --apply "ps: (builtins.head (builtins.filter (p: (p.pname or \"\") == \"$1\") ps)).$2"
}
drv=$(pkg_attr orca-cli drvPath) || { echo "no orca-cli in home.packages here" >&2; exit 1; }
want_version=$(pkg_attr orca-cli version) && [ -n "$want_version" ] \
  || { echo "orca-cli has no version" >&2; exit 1; }
CLI=$(nix-store --realise "$drv" | tail -1) || { echo "could not build orca-cli" >&2; exit 1; }
gui_drv=$(pkg_attr orca drvPath) || { echo "no orca in home.packages here" >&2; exit 1; }
GUI=$(nix-store --realise "$gui_drv" | tail -1) || { echo "could not build orca" >&2; exit 1; }
echo "testing $CLI and $GUI"

# The skills' own rule: outside an Orca terminal, on Linux, the CLI is
# `orca-ide`; elsewhere it is `orca`. Both names must be the CLI, and the GUI
# must not also claim `orca-ide` or `orca` -- they would collide in the
# profile.
for b in orca orca-ide; do
  if [ -x "$CLI/bin/$b" ]; then pass "orca-cli provides bin/$b"
  else fail "bin/$b" "missing from $CLI"; fi
done
gone() { [ ! -e "$1" ] && [ ! -L "$1" ]; }
if [ -x "$GUI/bin/orca-ide-app" ] && gone "$GUI/bin/orca-ide" && gone "$GUI/bin/orca"; then
  pass "the GUI is orca-ide-app and leaves orca-ide and orca to the CLI"
else fail "GUI bin names" "$(ls "$GUI/bin" | paste -sd' ')"; fi

# No sandbox between the caller and the CLI: a plain exec of node, so
# signals and exit codes are the CLI's own.
if grep -q 'exec .*/bin/node ' "$CLI/bin/orca-ide" && ! grep -q bwrap "$CLI/bin/orca-ide"; then
  pass "the CLI is a plain exec of node"
else fail "CLI launcher" "$CLI/bin/orca-ide is not a plain node exec"; fi

# Under plain node, `orca-ide open` has no Orca binary to spawn but
# ORCA_APP_EXECUTABLE (without it, it fails, and `serve` refuses to start).
# Exactly the profile's GUI: the unwrapped one has an orca-ide-app too,
# minus nixGL.
app=$(sed -nE 's|.*ORCA_APP_EXECUTABLE:-([^}]*)\}.*|\1|p' "$CLI/bin/orca-ide")
if [ "$app" = "$GUI/bin/orca-ide-app" ]; then
  pass "orca open launches the profile's nixGL-wrapped GUI"
else fail "ORCA_APP_EXECUTABLE" "'${app:-unset}', want $GUI/bin/orca-ide-app"; fi

# The GUI wrapper's argv decision, on the real generated script with its
# final exec swapped for a printf, so nothing launches: --ozone-platform=
# wayland first, except in node mode or when the caller names a platform
# itself -- `--ozone-platform` or `--ozone-platform=...` before any `--`,
# the rule of Orca's own hasExplicitOzonePlatform; a -hint does not count.
inner=$(grep -o '/nix/store/[^ ]*/bin/orca-ide-app' "$GUI/bin/orca-ide-app" | tail -1)
[ -n "$inner" ] || inner="$GUI/bin/orca-ide-app"  # no nixGL layer
sed -E 's|^exec .*/lib/orca/orca-ide" +"\$@" *$|printf "%s\\n" "$@"|' "$inner" > "$T/argv"
grep -q '^printf' "$T/argv" && ! grep -q '^exec ' "$T/argv" \
  || { echo "could not swap the one exec line in $inner" >&2; exit 1; }
argv() { env -u ELECTRON_RUN_AS_NODE bash "$T/argv" "$@" | paste -sd' '; }
for c in "open|--ozone-platform=wayland open" \
         "--serve --ozone-platform=x11|--serve --ozone-platform=x11" \
         "--ozone-platform x11|--ozone-platform x11" \
         "--ozone-platform=|--ozone-platform=" \
         "--ozone-platform-hint=auto|--ozone-platform=wayland --ozone-platform-hint=auto" \
         "-- --ozone-platform=x11|--ozone-platform=wayland -- --ozone-platform=x11"; do
  in=${c%%|*}; want=${c#*|}
  # shellcheck disable=SC2086
  got=$(argv $in)
  if [ "$got" = "$want" ]; then pass "GUI argv: '$in' -> '$want'"
  else fail "GUI argv '$in'" "got '$got', want '$want'"; fi
done
got=$(ELECTRON_RUN_AS_NODE=1 bash "$T/argv" -e 1 | paste -sd' ')
if [ "$got" = "-e 1" ]; then pass "GUI argv: node mode gets no flag"
else fail "GUI argv node mode" "got '$got'"; fi

# And for real: the CLI's `serve` probes ORCA_APP_EXECUTABLE with
# ELECTRON_RUN_AS_NODE=1 and a script as argv, and Node rejects the flag
# ("bad option", exit 9). With no display, and stdout to a file, not a pipe:
# were node mode lost, this would be a GUI launch -- without a display it
# exits before any window opens, and a file has no writer to wait on.
nodisplay() { env -u WAYLAND_DISPLAY -u WAYLAND_SOCKET -u DISPLAY "$@"; }
ELECTRON_RUN_AS_NODE=1 nodisplay timeout 60 "$GUI/bin/orca-ide-app" -p '"node-ok"' >"$T/node-out" 2>"$T/node-err"
node_ok=
if [ "$(cat "$T/node-out")" = node-ok ]; then
  node_ok=1; pass "orca-ide-app runs in node mode (serve's runtime probe)"
else fail "orca-ide-app node mode" "$(cat "$T/node-out")$(tail -1 "$T/node-err")"; fi

# `orca-ide open` spawns the GUI detached, waits for it (up to 15s), then
# exits. Each GUI wrapper layer execs the next, so Electron is the CLI's own
# child, and must not go when `open` returns -- what bwrap's
# --die-with-parent did, back when the GUI was sandboxed. The parent stays
# up a while first, as `open` does.
if [ -z "$node_ok" ]; then
  fail "GUI lifetime" "skipped: node mode failed above, so this would launch a real GUI"
else
ELECTRON_RUN_AS_NODE=1 nodisplay bash -c '"$1" -e "
  setTimeout(() => require(\"fs\").writeFileSync(process.argv[1], \"alive\"), 6000);
  setTimeout(() => process.exit(0), 7000);" "$2" >/dev/null 2>&1 & sleep 4' _ "$GUI/bin/orca-ide-app" "$T/alive"
for _ in $(seq 75); do [ -s "$T/alive" ] && break; sleep 0.2; done
if [ -s "$T/alive" ]; then pass "the GUI outlives the parent that spawned it (orca-ide open)"
else fail "GUI lifetime" "no alive marker within 15s (died with its parent, or never started)"; fi
fi

# The manifest travels with the extracted AppImage, which the CLI's script
# references, so it is in the CLI's closure.
contents=$(nix-store -qR "$CLI" | while read -r p; do
  [ -f "$p/resources/skills/current-manifest.json" ] && echo "$p"; done | head -1)
[ -n "$contents" ] || { echo "no extracted AppImage in the closure of $CLI" >&2; exit 1; }
manifest="$contents/resources/skills/current-manifest.json"

# Outside an Orca terminal: none of its env, none of its shim dir on PATH.
# ${!ORCA_@}, not compgen: it works in a bash built without programmable
# completion too.
clean_path=$(printf '%s' "$PATH" | tr ':' '\n' | grep -v 'linux-orca-cli-shim' | paste -sd:)
outside() {
  local u=() v
  for v in ${!ORCA_@}; do u+=(-u "$v"); done
  env "${u[@]}" PATH="$clean_path" "$@"
}

got=$(outside "$CLI/bin/orca" --version 2>&1)
if printf '%s' "$got" | grep -qwF "$want_version"; then pass "orca --version reports $want_version outside Orca"
else fail "orca --version" "got '$got', want $want_version"; fi

# What every skill stub runs, under both names the stubs choose between.
for b in orca-ide orca; do
  got=$(outside "$CLI/bin/$b" skills get orca-cli 2>&1)
  if printf '%s' "$got" | grep -q '^# Orca CLI'; then pass "$b skills get orca-cli prints the guide outside Orca"
  else fail "$b skills get" "$(printf '%s' "$got" | head -3)"; fi
done

# A CLI failure is the CLI's exit status, not swallowed by a wrapper.
outside "$CLI/bin/orca-ide" no-such-command >/dev/null 2>&1; rc=$?
if [ "$rc" -eq 1 ]; then pass "an unknown command exits 1"
else fail "exit status" "unknown command exited $rc"; fi

# The rev is pinned by hand next to the version; the app knows its own.
rev=$(nix eval --raw "$HM.home.file" --apply 'f: f.".claude/skills/orca-cli".source' 2>/dev/null | sed -nE 's|.*-orca-skills-([0-9a-f]{40})/.*|\1|p')
if [ -n "$rev" ] && grep -aqF "sourceCommit:\`$rev\`" "$contents/resources/app.asar"; then
  pass "skills rev ${rev:0:8} is this build's sourceCommit"
else fail "skills rev" "'${rev:-unparsed}' is not the sourceCommit in app.asar"; fi

for s in $SKILLS; do
  src=$(nix eval --raw "$HM.home.file" --apply "f: toString f.\".claude/skills/$s\".source" 2>/dev/null) \
    || { fail "$s" "no home.file .claude/skills/$s"; continue; }
  # The source is a subpath of a fetch; realise the fetch's derivation itself.
  srcdrv=$(nix eval --raw "$HM.home.file" --apply \
    "f: builtins.head (builtins.attrNames (builtins.getContext f.\".claude/skills/$s\".source))" 2>/dev/null)
  nix-store --realise "$srcdrv" >/dev/null
  # Every file the manifest lists for the skill, as "<sha256>  <path>" lines.
  want=$(jq -r --arg n "$s" '.skills[] | select(.name == $n) | .files[] | "\(.exactSha256)  \(.path)"' "$manifest")
  [ -n "$want" ] || { fail "$s" "no entry in the manifest"; continue; }
  if out=$(cd "$src" 2>/dev/null && printf '%s\n' "$want" | sha256sum --quiet -c - 2>&1); then
    pass "$s matches this build's manifest ($(printf '%s\n' "$want" | wc -l) files)"
  else fail "$s" "${out:-$src is missing}"; fi
done

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
