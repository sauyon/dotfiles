#!/usr/bin/env bash
# Cases for gwscli.nix:
#   * Every host gets official `gwscli` in home.packages (and not community `gws`).
#   * Every host gets the extracted `gwscli` skill in .agents/skills,
#     .claude/skills, and .gemini/config/skills; .cursor/skills only on utsuho
#     (matching isCursorHost / tests/agent-clis.sh).
#   * Building `gwscli` for this host produces a working binary whose reported
#     version matches the derivation and a non-empty SKILL.md.
#
# Usage:
#   ./tests/gwscli.sh
set -u
set -o pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
flake="${1:-$repo}"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

ev() { # ev <host> <attr-path> [apply]
  nix eval --max-jobs 0 --raw "$flake#homeConfigurations.$1.config.$2" ${3:+--apply "$3"}
}

hosts="utsuho fujiwara shiori chiyari kyuusaku mari"

for host in $hosts; do
  pkgs=$(ev "$host" home.packages 'ps: builtins.concatStringsSep " " (map (p: p.pname or p.name or "") ps)') \
    || { echo "could not evaluate $host home.packages" >&2; exit 1; }
  files=$(ev "$host" home.file 'fs: builtins.concatStringsSep "\n" (map (f: f.target) (builtins.attrValues fs))') \
    || { echo "could not evaluate $host home.file" >&2; exit 1; }

  case " $pkgs " in
    *" gwscli "*) ok "$host has gwscli" ;;
    *) bad "$host is missing gwscli" ;;
  esac
  case " $pkgs " in
    *" gws "*) bad "$host still has community gws" ;;
    *) ok "$host does not carry community gws" ;;
  esac

  for dir in .agents/skills .claude/skills .gemini/config/skills; do
    if printf '%s\n' "$files" | grep -Fxq "$dir/gwscli"; then
      ok "$host installs $dir/gwscli"
    else
      bad "$host is missing $dir/gwscli"
    fi
  done

  has_cursor=no
  printf '%s\n' "$files" | grep -Fxq ".cursor/skills/gwscli" && has_cursor=yes
  want_cursor=no; [ "$host" = utsuho ] && want_cursor=yes
  if [ "$has_cursor" = "$want_cursor" ]; then
    ok "$host .cursor/skills/gwscli: $want_cursor"
  else
    bad "$host .cursor/skills/gwscli: want $want_cursor, got $has_cursor"
  fi
done

host=$(cat /etc/hostname 2>/dev/null); host=${host:-$(uname -n)}
drv=$(nix eval --raw "$flake#homeConfigurations.$host.config.home.packages" \
  --apply 'ps: (builtins.head (builtins.filter (p: (p.pname or "") == "gwscli") ps)).drvPath') \
  || { echo "could not locate gwscli drvPath on $host" >&2; exit 1; }
want_version=$(nix eval --raw "$flake#homeConfigurations.$host.config.home.packages" \
  --apply 'ps: (builtins.head (builtins.filter (p: (p.pname or "") == "gwscli") ps)).version') \
  || { echo "could not locate gwscli version on $host" >&2; exit 1; }
out=$(nix-store --realise "$drv" | tail -1) \
  || { echo "could not realise $drv" >&2; exit 1; }

T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
got_version=$(HOME="$T" "$out/bin/gwscli" version 2>&1)
case "$got_version" in
  *"$want_version"*) ok "built gwscli runs and matches version $want_version" ;;
  *) bad "gwscli version: got '$got_version', want '$want_version'" ;;
esac

if [ -s "$out/share/gwscli/skills/gwscli/SKILL.md" ] && grep -q '^name: gwscli$' "$out/share/gwscli/skills/gwscli/SKILL.md"; then
  ok "gwscli ships share/gwscli/skills/gwscli/SKILL.md"
else
  bad "gwscli SKILL.md missing or malformed in $out"
fi

[ "$fails" -eq 0 ]
