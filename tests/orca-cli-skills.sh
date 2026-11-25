#!/usr/bin/env bash
# Cases for the claims orca.nix makes about the Orca CLI and its agent skills:
# that `orca-ide` and `orca` are the CLI and run from any shell, not only an
# Orca terminal; that the GUI is `orca-ide-app` and the CLI can launch it; and
# that the skills in the agent skill roots are the bytes this Orca build expects
# and land on every Orca host (desktop Linux and headless orca-serve).
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
host=$(cat /etc/hostname 2>/dev/null); host=${host:-$(uname -n)}; HM="$FLAKE#homeConfigurations.$host.config"
SKILLS="orca-cli orchestration orca-computer-use orca-per-workspace-env orca-linear linear-tickets"

pkg_attr() { # pkg_attr <pname> <attr>
  nix eval --raw "$HM.home.packages" \
    --apply "ps: (builtins.head (builtins.filter (p: (p.pname or \"\") == \"$1\") ps)).$2"
}
drv=$(pkg_attr orca-cli drvPath) || { echo "no orca-cli in home.packages here" >&2; exit 1; }
want_version=$(pkg_attr orca-cli version) && [ -n "$want_version" ] \
  || { echo "orca-cli has no version" >&2; exit 1; }
CLI=$(nix-store --realise "$drv" | tail -1) || { echo "could not build orca-cli" >&2; exit 1; }
gui_in_profile=1
if gui_drv=$(pkg_attr orca drvPath 2>/dev/null); then
  GUI=$(nix-store --realise "$gui_drv" | tail -1) || { echo "could not build orca" >&2; exit 1; }
else
  # Serve-only host (machine.orca-serve without gui): orcaGui is referenced by
  # orcaCli's ORCA_APP_EXECUTABLE rather than placed on PATH directly.
  gui_in_profile=0
  app=$(sed -nE 's|.*ORCA_APP_EXECUTABLE:-([^}]*)\}.*|\1|p' "$CLI/bin/orca-ide")
  GUI=${app%/bin/orca-ide-app}
  [ -n "$GUI" ] && [ -x "$GUI/bin/orca-ide-app" ] \
    || { echo "no orca in home.packages and ORCA_APP_EXECUTABLE unusable: ${app:-unset}" >&2; exit 1; }
fi
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
# On a desktop host this is the profile's nixGL-wrapped GUI; on a serve-only
# host it is the unwrapped runtime.
app=$(sed -nE 's|.*ORCA_APP_EXECUTABLE:-([^}]*)\}.*|\1|p' "$CLI/bin/orca-ide")
if [ "$app" = "$GUI/bin/orca-ide-app" ]; then
  if [ "$gui_in_profile" -eq 1 ]; then
    pass "orca open launches the profile's nixGL-wrapped GUI"
  else
    pass "orca serve/open launches the unwrapped Orca runtime"
  fi
else fail "ORCA_APP_EXECUTABLE" "'${app:-unset}', want $GUI/bin/orca-ide-app"; fi

# The GUI wrapper's argv decision, on the real generated script with its
# final exec swapped for a printf, so nothing launches: --ozone-platform=
# wayland first, except in node mode or when the caller names a platform
# itself -- `--ozone-platform` or `--ozone-platform=...` before any `--`,
# the rule of Orca's own hasExplicitOzonePlatform; a -hint does not count.
# Likewise --password-store=gnome-libsecret, unless the caller names a store
# before any `--`, or it is serve mode by Orca's rule (`--serve` anywhere, or
# a first positional `serve` before `--`): Orca's own probe picks libsecret only if
# the keyring is already unlocked, and a launch that loses that race cannot
# read the last run's v11 account session (utsuho, 2026-11-05: signed out).
# bash -e: the wrapper's own shebang flag.
inner=$(grep -o '/nix/store/[^ ]*/bin/orca-ide-app' "$GUI/bin/orca-ide-app" | tail -1)
[ -n "$inner" ] || inner="$GUI/bin/orca-ide-app"  # no nixGL layer
sed -E 's|^exec .*/lib/orca/orca-ide" +"\$@" *$|printf "%s\\n" "$@"|' "$inner" > "$T/argv"
grep -q '^printf' "$T/argv" && ! grep -q '^exec ' "$T/argv" \
  || { echo "could not swap the one exec line in $inner" >&2; exit 1; }
argv() { env -u ELECTRON_RUN_AS_NODE bash -e "$T/argv" "$@" | paste -sd' '; }
ls=--password-store=gnome-libsecret
for c in "open|--ozone-platform=wayland $ls open" \
         "--serve --ozone-platform=x11|--serve --ozone-platform=x11" \
         "--serve|--ozone-platform=wayland --serve" \
         "--ozone-platform x11|$ls --ozone-platform x11" \
         "--ozone-platform=|$ls --ozone-platform=" \
         "--ozone-platform-hint=auto|--ozone-platform=wayland $ls --ozone-platform-hint=auto" \
         "-- --ozone-platform=x11|--ozone-platform=wayland $ls -- --ozone-platform=x11" \
         "--password-store=basic|--ozone-platform=wayland --password-store=basic" \
         "--password-store basic|--ozone-platform=wayland --password-store basic" \
         "-- --password-store=basic|--ozone-platform=wayland $ls -- --password-store=basic" \
         "--ozone-platform=x11 --password-store=basic|--ozone-platform=x11 --password-store=basic" \
         "-- --serve|--ozone-platform=wayland -- --serve" \
         "serve|--ozone-platform=wayland serve" \
         "--no-sandbox serve|--ozone-platform=wayland --no-sandbox serve" \
         "open serve|--ozone-platform=wayland $ls open serve" \
         "-- serve|--ozone-platform=wayland $ls -- serve" \
         "--serve=1|--ozone-platform=wayland $ls --serve=1"; do
  in=${c%%|*}; want=${c#*|}
  # shellcheck disable=SC2086
  got=$(argv $in)
  if [ "$got" = "$want" ]; then pass "GUI argv: '$in' -> '$want'"
  else fail "GUI argv '$in'" "got '$got', want '$want'"; fi
done
got=$(ELECTRON_RUN_AS_NODE=1 bash -e "$T/argv" -e 1 | paste -sd' ')
if [ "$got" = "-e 1" ]; then pass "GUI argv: node mode gets no flag"
else fail "GUI argv node mode" "got '$got'"; fi

# And for real: the CLI's `serve` probes ORCA_APP_EXECUTABLE with
# ELECTRON_RUN_AS_NODE=1 and a script as argv, and Node rejects the flag
# ("bad option", exit 9). With no display, and stdout to a file, not a pipe:
# were node mode lost, this would be a GUI launch -- without a display it
# exits before any window opens, and a file has no writer to wait on.
# And no way to the live session at all: a runtime dir with no socket in it
# (unset, Wayland falls back to wayland-0 there) and no session bus.
mkdir -p -m700 "$T/nodisplay"
nodisplay() { env -u WAYLAND_DISPLAY -u WAYLAND_SOCKET -u DISPLAY -u DBUS_SESSION_BUS_ADDRESS \
  XDG_RUNTIME_DIR="$T/nodisplay" "$@"; }
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

# The rev is pinned by hand next to the version; the app knows its own. Newer
# ASARs do not expose it as a raw string, so search the extracted app's
# authoritative build-metadata fields, not an arbitrary match elsewhere.
rev=$(nix eval --raw "$HM.home.file" --apply 'f: f.".claude/skills/orca-cli".source' 2>/dev/null | sed -nE 's|.*-orca-skills-([0-9a-f]{40})/.*|\1|p')
asarpkg=$(nix build --no-link --print-out-paths "$FLAKE#homeConfigurations.${host}.pkgs.asar") \
  || { echo "could not build the configured ASAR tool" >&2; exit 1; }
asar="$asarpkg/bin/asar"
[ -x "$asar" ] || { echo "configured ASAR tool is missing: $asar" >&2; exit 1; }
"$asar" extract "$contents/resources/app.asar" "$T/app" \
  || { echo "could not extract $contents/resources/app.asar" >&2; exit 1; }
if [ -n "$rev" ] && {
  grep -a -q -F "sourceCommit:\`$rev\`" "$T/app/out/main/index.js" \
    || grep -a -q -F "sourceCommit:\`$rev\`" "$T"/app/out/renderer/assets/use-bundled-skill-install-action-*.js;
}; then
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

# Fleet-wide skill root placement (eval-only, --max-jobs 0): every Orca host
# (desktop Linux + headless orca-serve) gets all skills across the shared,
# Claude, and Antigravity roots, plus Cursor on isCursorHost (utsuho) only;
# non-Orca hosts get none.
all_roots='[ ".agents/skills" ".claude/skills" ".gemini/config/skills" ".cursor/skills" ]'
skills_nix='[ "orca-cli" "orchestration" "orca-computer-use" "orca-per-workspace-env" "orca-linear" "linear-tickets" ]'
for spec in \
  "utsuho|.agents/skills .claude/skills .gemini/config/skills .cursor/skills" \
  "shiori|.agents/skills .claude/skills .gemini/config/skills" \
  "fujiwara|.agents/skills .claude/skills .gemini/config/skills" \
  "chiyari|" \
  "kyuusaku|" \
  "mari|"; do
  h=${spec%%|*}; want_dirs=${spec#*|}
  got=$(nix eval --max-jobs 0 --raw "$FLAKE#homeConfigurations.$h.config.home.file" --apply "
    f: let
      roots = $all_roots;
      skills = $skills_nix;
      present = d: builtins.filter (s: f ? \"\${d}/\${s}\") skills;
      okRoot = d:
        let p = present d; in
        if p == skills then d
        else if p == [] then \"\"
        else \"partial:\${d}\" + \"(\" + toString (builtins.length p) + \")\";
    in builtins.concatStringsSep \" \" (builtins.filter (x: x != \"\") (map okRoot roots))
  " 2>/dev/null) || { fail "$h skill roots" "eval failed"; continue; }
  if [ "$got" = "$want_dirs" ]; then
    pass "$h skill roots: ${got:-none}"
  else
    fail "$h skill roots" "got '${got:-none}', want '${want_dirs:-none}'"
  fi
done

# Fleet-wide orca-serve switch method (eval-only, --max-jobs 0): every host
# defining systemd.user.services.orca-serve must set X-SwitchMethod=keep-old so
# hms/sd-switch does not stop a running Orca server (and kill active sessions or
# itself) when the unit changes.
serve_hosts=0
for h in utsuho shiori fujiwara chiyari kyuusaku mari; do
  method=$(nix eval --max-jobs 0 --raw "$FLAKE#homeConfigurations.$h.config.systemd.user.services" --apply '
    s: if s ? orca-serve then (s.orca-serve.Unit."X-SwitchMethod" or "unset") else "none"
  ' 2>/dev/null) || { fail "$h orca-serve X-SwitchMethod" "eval failed"; continue; }
  case "$method" in
    none) ;;
    keep-old)
      serve_hosts=$((serve_hosts+1))
      pass "$h orca-serve uses X-SwitchMethod=keep-old" ;;
    *)
      serve_hosts=$((serve_hosts+1))
      fail "$h orca-serve X-SwitchMethod" "got '$method', want 'keep-old'" ;;
  esac
done
if [ "$serve_hosts" -gt 0 ]; then
  pass "at least one host defines orca-serve ($serve_hosts)"
else
  fail "orca-serve hosts" "no host defines systemd.user.services.orca-serve"
fi

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
