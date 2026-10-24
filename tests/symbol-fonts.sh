#!/usr/bin/env bash
# Cases for the symbol glyphs terminal UIs draw, on every host.
#
# Nothing errors when a glyph has no font: Chromium (Orca, every Electron app)
# and the GTK stack ask fontconfig for any face with the codepoint, get none,
# and draw a box. Claude Code's "⏵⏵ bypass permissions" line was tofu in Orca
# because not one installed face carried U+23F5 -- the Nerd Fonts, the
# google-fonts set and Noto Color Emoji all skip it.
#
# Noto Sans Symbols 2 has it, but whole it also has ⏸⏹⏺🖥⭐ and ~360 more that
# Noto Color Emoji had, and fontconfig ranks it first for them (for sans text
# on fontconfig 2.18, by genericfamily, which no family-order rule outranks),
# so those went monochrome. So home.nix installs it cut down to the codepoints
# no other profile font has (nix/font-gaps.py).
#
# Three claims, each failing silently on its own:
#
#   * the gap font is in every host's home.packages, and every other font
#     package there is one it yields to, and everything it yields to is
#     installed -- a font it yields to but nothing installs is a hole, tofu
#     again (hosts are read out of the flake).
#     Every host is checked by package name, an eval only; on hosts of this
#     machine's system, also by font files: any package already in the local
#     store with a font under share/fonts must be yielded to, whatever its
#     name (jetbrains-mono, inter, iosevka never say "font");
#   * on hosts of this machine's system, the realised face still covers the
#     glyphs below, checked with fc-query;
#   * on those hosts, fallback through a throwaway fontconfig over nixpkgs'
#     conf.d with the gap font and what it yields to -- the profile alone,
#     nothing from the host (orca's own fontconfig, the host's plus DejaVu,
#     is tests/orca-gui-unsandboxed.sh's): ⏵ and 🬀
#     reach the gap font, ⏸🖥⭐ still reach Noto Color Emoji and ␀ a text font
#     -- for a monospace request and for a sans one.
#
#   ./tests/symbol-fonts.sh            # tests this checkout
#   ./tests/symbol-fonts.sh /path/to/flake
set -u
set -o pipefail

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

PKG=noto-sans-symbols-2
# ⏵ media play (Claude Code's mode line), 🬀 legacy-computing sextants (TUI
# graphs), ⬢ hexagon (prompt segments).
CODEPOINTS="23f5 1fb00 2b22"

fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }
glyph() { printf "\\U$(printf %08x "0x$1")"; }

command -v fc-query >/dev/null || { echo "fc-query not on PATH" >&2; exit 1; }

here=$(nix eval --raw --impure --expr builtins.currentSystem) \
  || { echo "could not ask nix for the current system" >&2; exit 1; }

# --max-jobs 0: this is an eval, and must stay one (see tests/hmeval.sh).
systems=$(nix eval --max-jobs 0 --json "$FLAKE#homeConfigurations" \
  --apply 'hs: builtins.mapAttrs (_: h: h.pkgs.stdenv.hostPlatform.system) hs' 2>/dev/null) \
  || { echo "could not list homeConfigurations" >&2; exit 1; }
hosts=$(jq -r 'keys[]' <<<"$systems")
[ -n "$hosts" ] || { echo "this flake has no homeConfigurations" >&2; exit 1; }

# covers <ranges> <codepoint-hex>: does fontconfig's charset string (space-
# separated hex values and lo-hi ranges) include the codepoint?
covers() {
  local d=$((16#$2)) r lo hi
  for r in $1; do
    lo=$((16#${r%-*})); hi=$((16#${r#*-}))
    [ "$d" -ge "$lo" ] && [ "$d" -le "$hi" ] && return 0
  done
  return 1
}

checked=""
for h in $hosts; do
  sys=$(jq -r --arg h "$h" '.[$h]' <<<"$systems")
  # The gap font's drv, then the drvs of what it yields to, one per line; and
  # any font package in home.packages (or the GTK font) it does not yield to,
  # which it would then outrank. runCommand sets name, not pname.
  info=$(nix eval --max-jobs 0 --json "$FLAKE#homeConfigurations.$h.config" --apply "c: let
      m = builtins.filter (p: (p.name or \"\") == \"$PKG\") c.home.packages;
      g = builtins.head m;
      fontish = p: builtins.match \".*font.*\" (p.name or \"\") != null;
      gtkFont = if c.gtk.enable && c.gtk.font != null && c.gtk.font.package != null
        then [ c.gtk.font.package ] else [ ];
      others = builtins.filter (p: fontish p && (p.name or \"\") != \"$PKG\")
        (c.home.packages ++ gtkFont);
      yielded = map (p: p.outPath) (g.yieldTo or [ ]);
      installed = map (p: p.outPath) (c.home.packages ++ gtkFont);
    in if m == [ ] then null else {
      drv = g.drvPath;
      yieldTo = map (p: p.drvPath) (g.yieldTo or [ ]);
      yieldToOut = yielded;
      missing = map (p: p.name) (builtins.filter (p: !(builtins.elem p.outPath yielded)) others);
      phantom = map (p: p.name) (builtins.filter
        (p: !(builtins.elem p.outPath installed)) (g.yieldTo or [ ]));
      all = map (p: { name = p.name or \"\"; out = p.outPath; })
        (builtins.filter (p: (p.name or \"\") != \"$PKG\") (c.home.packages ++ gtkFont));
    }" 2>/dev/null) \
    || { fail "$h" "home.packages did not evaluate"; continue; }
  if [ "$info" = null ]; then
    fail "$h" "$PKG is not in home.packages"
    continue
  fi
  pass "$h: $PKG in home.packages"
  missing=$(jq -r '.missing | join(" ")' <<<"$info")
  if [ -z "$missing" ]; then
    pass "$h: $PKG yields to every other font package"
  else
    fail "$h" "$PKG does not yield to: $missing"
  fi
  # The converse: a font it yields to but the profile does not install leaves
  # a hole where that font's glyphs were -- tofu again, the original bug.
  phantom=$(jq -r '.phantom | join(" ")' <<<"$info")
  if [ -z "$phantom" ]; then
    pass "$h: everything $PKG yields to is installed"
  else
    fail "$h" "$PKG yields to fonts the profile does not install: $phantom"
  fi

  [ "$sys" = "$here" ] || continue
  # By font files: names only catch packages that say "font".
  unyielded=""; skipped=0; looked=0
  while IFS=$'\t' read -r name pout; do
    [ -e "$pout" ] || { skipped=$((skipped+1)); continue; }
    jq -e --arg o "$pout" '.yieldToOut | index($o)' <<<"$info" >/dev/null && continue
    # Counted only once its files are actually searched: the yielded fonts are
    # always in the store, and counting them let this pass with none looked at.
    looked=$((looked+1))
    if [ -n "$(find -L "$pout/share/fonts" -type f \( -iname '*.otf' -o -iname '*.ttf' -o -iname '*.ttc' \) -print -quit 2>/dev/null)" ]; then
      unyielded="$unyielded $name"
    fi
  done < <(jq -r '.all[] | "\(.name)\t\(.out)"' <<<"$info")
  if [ "$looked" -eq 0 ]; then
    fail "$h" "no package of home.packages was in the local store to check"
  elif [ -z "$unyielded" ]; then
    pass "$h: no unyielded package ships fonts ($looked checked, $skipped not in the local store)"
  else
    fail "$h" "ships fonts but is not yielded to:$unyielded"
  fi
  drv=$(jq -r .drv <<<"$info")
  case " $checked " in *" $drv "*) continue ;; esac
  checked="$checked $drv"
  out=$(nix-store --realise "$drv" 2>/dev/null | tail -1) \
    || { fail "$h" "could not realise $drv"; continue; }
  # Every face's charset; a codepoint counts if any face has it.
  ranges=$(find "$out/share/fonts" \( -name '*.otf' -o -name '*.ttf' -o -name '*.ttc' \) \
    -exec fc-query -f '%{charset}\n' {} + 2>/dev/null | tr '\n' ' ')
  for cp in $CODEPOINTS; do
    if covers "$ranges" "$cp"; then
      pass "$h: U+$cp $(glyph $cp) covered"
    else
      fail "$h: U+$cp $(glyph $cp)" "no face in $out covers it"
    fi
  done

  # The fallback the profile produces: the gap font plus everything it yields
  # to, nothing from the host, over nixpkgs' conf.d.
  # Realised by drv, but only the default outputs are used: google-fonts also
  # has an adobeBlank output -- blank glyphs for every codepoint -- which the
  # profile never links and which would otherwise win every fallback.
  # shellcheck disable=SC2046
  nix-store --realise $(jq -r '.yieldTo[]' <<<"$info") >/dev/null 2>&1 \
    || { fail "$h: fallback" "could not realise the fonts it yields to"; continue; }
  dirs=$(jq -r '.yieldToOut[]' <<<"$info")
  fc=$(nix build --no-link --print-out-paths "$FLAKE#homeConfigurations.$h.pkgs.fontconfig.out" 2>/dev/null) \
    || { fail "$h: fallback" "could not realise fontconfig"; continue; }
  T=$(mktemp -d)
  {
    echo '<?xml version="1.0"?>'
    echo '<!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">'
    echo "<fontconfig><include ignore_missing=\"no\">$fc/etc/fonts/conf.d</include>"
    for d in $out $dirs; do echo "<dir>$d/share/fonts</dir>"; done
    echo "<cachedir>$T/cache</cachedir></fontconfig>"
  } > "$T/fonts.conf"
  for req in "JetBrainsMono Nerd Font" "NotoSans Nerd Font"; do
    for want in "23f5=Noto Sans Symbols 2" "1fb00=Noto Sans Symbols 2" \
                "23f8=Noto Color Emoji" "1f5a5=Noto Color Emoji" "2b50=Noto Color Emoji" \
                "2400!Noto Sans Symbols 2"; do
      case $want in
        *=*) cp=${want%%=*}; fam=${want#*=}; neg= ;;
        *) cp=${want%%!*}; fam=${want#*!}; neg=1 ;;
      esac
      got=$(XDG_CONFIG_HOME="$T/none" FONTCONFIG_FILE="$T/fonts.conf" \
        fc-match -f '%{family[0]}' "$req:charset=$cp" 2>/dev/null)
      if [ -z "$neg" ] && [ "$got" = "$fam" ]; then
        pass "$h: $req, U+$cp $(glyph $cp) -> $fam"
      elif [ -n "$neg" ] && [ -n "$got" ] && [ "$got" != "$fam" ]; then
        pass "$h: $req, U+$cp $(glyph $cp) -> $got, not $fam"
      elif [ -z "$neg" ]; then
        fail "$h: $req, U+$cp $(glyph $cp)" "falls back to '$got', want '$fam'"
      else
        fail "$h: $req, U+$cp $(glyph $cp)" "falls back to '$got', which it must not"
      fi
    done
  done
  rm -rf "$T"
done

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
