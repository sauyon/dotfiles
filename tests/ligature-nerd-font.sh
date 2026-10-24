#!/usr/bin/env bash
# Cases for the claim that home.packages carries a Nerd Font that keeps its
# programming ligatures -- JetBrainsMono Nerd Font, from nerd-fonts.jetbrains-mono.
#
# Why a test for a one-line package add. Nerd Fonts ships each family twice:
# "JetBrainsMono Nerd Font" (ligatures kept) and "JetBrainsMonoNL Nerd Font"
# (NL = no ligatures), plus a "Mono" spacing variant of each, all in the same
# package. Picking a face by family name gets you the wrong one silently, and
# the patcher has in the past dropped GSUB tables. So this checks the thing
# actually wanted: a face with Nerd glyphs (U+E0B0, the powerline arrow) whose
# GSUB still has the `calt` feature JetBrains Mono's ligatures live in.
#
#   tests/ligature-nerd-font.sh            evaluate and realise it from this host's home.packages
#   tests/ligature-nerd-font.sh <path>     test a given font package

set -u
set -o pipefail

fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }

FAMILY="JetBrainsMono Nerd Font"

FLAKE=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
PKG="${1:-}"
if [ -z "$PKG" ]; then
  PKG=$(nix eval --max-jobs 0 --raw "$FLAKE#homeConfigurations.$(cat /etc/hostname).config.home.packages" \
    --apply 'ps: let m = builtins.filter (p: (p.pname or "") == "nerd-fonts-jetbrains-mono") ps;
             in if m == [] then "" else (builtins.head m).outPath' 2>/dev/null) \
    || { echo "home.packages did not evaluate" >&2; exit 1; }
  if [ -z "$PKG" ]; then
    fail "home.packages" "no nerd-fonts-jetbrains-mono"
    printf '\n%d/%d passed\n' "$((n-fails))" "$n"; exit 1
  fi
  nix-store --realise "$PKG" >/dev/null || { echo "could not realise $PKG" >&2; exit 1; }
fi
pass "nerd-fonts-jetbrains-mono in home.packages ($PKG)"

face=$(find "$PKG/share/fonts" -name 'JetBrainsMonoNerdFont-Regular.ttf' | head -1)
if [ -z "$face" ]; then
  fail "$FAMILY Regular" "no JetBrainsMonoNerdFont-Regular.ttf under $PKG"
else
  fam=$(fc-query -f '%{family}\n' "$face" | head -1)
  case ",$fam," in *",$FAMILY,"*) pass "$face is family '$FAMILY'" ;;
    *) fail "$face" "family is '$fam', want '$FAMILY'" ;; esac
  if fc-query -f '%{charset}\n' "$face" | grep -qiE '(^| )e0a0-|e0b0'; then
    pass "Nerd glyphs present (powerline U+E0B0)"
  else
    fail "$face" "no powerline glyphs in its charset"
  fi
  # fontTools from this flake's own nixpkgs (a bare `nix shell` adds the library
  # but runs whatever python3 is first on PATH). -I: it only reads a store path.
  py=$(nix build --no-link --print-out-paths --impure --expr \
    "(builtins.getFlake \"$FLAKE\").homeConfigurations.$(cat /etc/hostname).pkgs.python3.withPackages (p: [ p.fonttools ])") \
    || { echo "could not build python3 with fonttools" >&2; exit 1; }
  if "$py/bin/python3" -I -c '
import sys
from fontTools.ttLib import TTFont
f = TTFont(sys.argv[1])
feats = {r.FeatureTag for r in f["GSUB"].table.FeatureList.FeatureRecord} if "GSUB" in f else set()
sys.exit(0 if "calt" in feats else 1)' "$face"; then
    pass "GSUB keeps calt (ligatures)"
  else
    fail "$face" "no calt in GSUB -- ligatures stripped"
  fi
fi

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
