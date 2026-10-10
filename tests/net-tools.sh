#!/usr/bin/env bash
# Cases for the network tools home.nix puts on every host's PATH.
#
# The claim is about *commands*, not packages: `route`, `nmap`, `dig` and the
# rest answer on a fresh box without anyone installing anything. That is two
# claims stacked, and each fails silently on its own:
#
#   * the package is in the host's home.packages. The list is gated per
#     platform, so an entry can evaluate fine and still be absent from the host
#     in front of you;
#   * the package ships the command under that name. nixpkgs splits and renames
#     freely -- `dig` lives in bind's dnsutils output, `route` in net-tools,
#     `iperf3` in a package called iperf -- so "nmap is installed" says nothing
#     about whether `ncat` is.
#
# So each row names a command and the package expected to carry it. Every host
# in the flake is asked for the package (read out of the flake, so a new host is
# covered the day it lands); the command is looked for in the realised output on
# hosts of this machine's own system, where realising is a cache download rather
# than a cross build.
#
# The last case is the one that would otherwise only show up as a red CI run:
# home.packages is a buildEnv, and two packages shipping the same bin/ name is a
# build failure there. inetutils is the trap -- it carries its own ifconfig and
# hostname next to net-tools' -- so the set is checked for overlaps here.
#
#   ./tests/net-tools.sh            # tests this checkout
#   ./tests/net-tools.sh /path/to/flake
set -u
set -o pipefail

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

# nix's stderr from the last eval, kept so a host that fails to evaluate says
# why -- an unsupported package on one platform reads the same as a typo
# otherwise.
ERR=$(mktemp) || { echo "mktemp failed" >&2; exit 1; }
trap 'rm -f "$ERR"' EXIT

fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }

# <where> <package> <command>. `linux` rows are the ones macOS already answers
# natively (route, ifconfig, netstat, traceroute, nc) or has no use for.
#
# <package> is the pname, plus `.<output>` when the entry is one named output of
# a split package -- which is how bind appears twice: nixpkgs' `dig` and `host`
# are two outputs of it, and dig's does not carry bin/host.
ROWS='
all   nmap            nmap
all   nmap            ncat
all   bind.dnsutils   dig
all   bind.dnsutils   nslookup
all   bind.host       host
all   mtr             mtr
all   tcpdump         tcpdump
all   whois           whois
all   iperf           iperf3
linux netcat-openbsd  nc
linux net-tools       route
linux net-tools       ifconfig
linux net-tools       netstat
linux net-tools       arp
linux traceroute      traceroute
linux ethtool         ethtool
'

here=$(nix eval --raw --impure --expr builtins.currentSystem) \
  || { echo "could not ask nix for the current system" >&2; exit 1; }

# --max-jobs 0: this is an eval, and must stay one (see tests/hmeval.sh).
systems=$(nix eval --max-jobs 0 --json "$FLAKE#homeConfigurations" \
  --apply 'hs: builtins.mapAttrs (_: h: h.pkgs.stdenv.hostPlatform.system) hs' 2>/dev/null) \
  || { echo "could not list homeConfigurations" >&2; exit 1; }
hosts=$(jq -r 'keys[]' <<<"$systems")
[ -n "$hosts" ] || { echo "this flake has no homeConfigurations" >&2; exit 1; }

wanted=$(awk 'NF { print $2 }' <<<"$ROWS" | sort -u | jq -Rn '[inputs]')

declare -A bins=()   # out path -> newline-separated bin/ names, realised once
for h in $hosts; do
  sys=$(jq -r --arg h "$h" '.[$h]' <<<"$systems")
  # package -> out path, for the wanted packages only: forcing outPath on the
  # whole list would instantiate every derivation in the profile.
  have=$(nix eval --max-jobs 0 --json "$FLAKE#homeConfigurations.$h.config.home.packages" \
    --apply "ps: let
        key = p: (p.pname or \"\") + (if (p.outputName or \"out\") == \"out\" then \"\" else \".\" + p.outputName);
      in builtins.listToAttrs (map (p: { name = key p; value = p.outPath; })
        (builtins.filter (p: builtins.elem (key p) (builtins.fromJSON ''$wanted'')) ps))" \
    2>"$ERR") || { fail "$h" "home.packages did not evaluate: $(grep -m1 'error:' "$ERR")"; continue; }

  while read -r where pkg cmd; do
    [ -n "$where" ] || continue
    case "$where:$sys" in linux:*-darwin) continue ;; esac
    out=$(jq -r --arg p "$pkg" '.[$p] // empty' <<<"$have")
    if [ -z "$out" ]; then
      fail "$h: $cmd" "$pkg is not in home.packages"
      continue
    fi
    if [ "$sys" != "$here" ]; then
      pass "$h: $cmd ($pkg present; $sys not realised here)"
      continue
    fi
    if [ -z "${bins[$out]+x}" ]; then
      nix-store --realise "$out" >/dev/null 2>&1 \
        || { fail "$h: $cmd" "could not realise $out"; continue; }
      # Trailing slashes: sbin is usually a symlink to bin, and without one
      # find reports the link itself instead of looking inside it.
      bins[$out]=$(find "$out/bin/" "$out/sbin/" -mindepth 1 -maxdepth 1 \
        \( -type f -o -type l \) -printf '%f\n' 2>/dev/null | sort -u)
    fi
    if grep -qx -- "$cmd" <<<"${bins[$out]}"; then
      pass "$h: $cmd"
    else
      fail "$h: $cmd" "$pkg has no bin/$cmd"
    fi
  done <<<"$ROWS"
done

if [ ${#bins[@]} -gt 0 ]; then
  dups=$(printf '%s\n' "${bins[@]}" | sed '/^$/d' | sort | uniq -d | tr '\n' ' ')
  if [ -z "$dups" ]; then
    pass "no bin/ name is shipped by two of these packages"
  else
    fail "bin/ collision" "shipped twice: $dups"
  fi
else
  fail "bin/ collision" "nothing was realised, so nothing was compared"
fi

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
