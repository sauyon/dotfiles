#!/usr/bin/env bash
# Cases for the NUR pin in home.nix's overlay, and for what it ships to Zen.
#
# Why this needs a test. NUR has two branches that both look alive: `main`, the
# default, where the bot commits every repo's lock several times an hour; and
# `master`, which still takes human-merged PRs (repo registrations) but whose
# repos.json.lock the bot stopped updating on 2025-03-06. A `master` commit dated
# yesterday therefore carries a lock nineteen months old. The 2026-10-05 bump
# (3d5ab564) pinned such a commit, and 7c01a312 bumped to another one, so rycee's
# firefox-addons went back from an Apr 2026 rev to a Mar 2025 one: the Bitwarden
# extension in Zen silently downgraded 2026.2.0 -> 2025.2.0 while bitwarden-desktop
# kept moving forward. The old extension could not read the account state the
# newer one had written and refused to unlock with "No encrypted user key found."
# Nothing failed at eval, build or switch time -- an older version is still a
# version.
#
# Two cases:
#
#   The pinned rev is on NUR's default branch. Asked of GitHub's compare API, so
#   the default branch is read rather than written down here. Needs network; with
#   none it fails, rather than passing on a check it never ran.
#
#   On every host that installs it, the Bitwarden extension is within six months
#   of bitwarden-desktop. They share the vault's local state through native
#   messaging and track the same server, so a wide gap either way is the bug above
#   coming back, whatever pin or branch caused it.
#
#   ./tests/nur-pin.sh            # tests this checkout
#   ./tests/nur-pin.sh /path/to/flake
set -u
set -o pipefail

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

D=$(mktemp -d) || { echo "mktemp failed" >&2; exit 1; }
trap 'rm -rf "$D"' EXIT
fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }

# --- the pin is on NUR's default branch --------------------------------------
# NUR_PIN_REV stands in for the rev read from home.nix, so the branch case can be
# shown to accept an older ancestor of main and refuse a master commit.
revs=${NUR_PIN_REV:-$(grep -oE 'nix-community/NUR/archive/[0-9a-f]{40}' "$FLAKE/home.nix" | grep -oE '[0-9a-f]{40}$' | sort -u)}
if [ "$(printf '%s\n' "$revs" | grep -c .)" -ne 1 ]; then
  fail "home.nix pins exactly one NUR rev" "found: $(echo $revs)"
else
  rev=$revs
  if ! curl -fsS --max-time 30 https://api.github.com/repos/nix-community/NUR >"$D/repo" 2>"$D/err"; then
    fail "NUR $rev is on the default branch" "could not reach GitHub: $(cat "$D/err")"
  else
    branch=$(jq -r .default_branch <"$D/repo")
    # $branch as base, the pin as head: status is the pin relative to $branch.
    # per_page=1 because a master pin diverges by tens of thousands of commits.
    if ! curl -fsS --max-time 30 "https://api.github.com/repos/nix-community/NUR/compare/$branch...$rev?per_page=1" >"$D/cmp" 2>"$D/err"; then
      fail "NUR $rev is on $branch" "compare failed: $(cat "$D/err")"
    else
      status=$(jq -r .status <"$D/cmp")
      # behind: the pin is an ancestor $branch has moved past. identical: its tip.
      case "$status" in
        behind|identical) pass "NUR $rev is on $branch ($status)" ;;
        *) fail "NUR $rev is on $branch" \
                "compare says '$status'; the pin is not an ancestor of $branch, so its repos.json.lock is not the one NUR is updating" ;;
      esac
    fi
  fi
fi

# --- the extension keeps up with the desktop app -----------------------------
if ! nix eval --json "$FLAKE#homeConfigurations" --apply builtins.attrNames >"$D/hosts" 2>"$D/err"; then
  echo "could not list homeConfigurations:" >&2; cat "$D/err" >&2; exit 1
fi
mapfile -t hosts < <(jq -r '.[]' <"$D/hosts")
[ "${#hosts[@]}" -gt 0 ] || { echo "flake defines no homeConfigurations" >&2; exit 1; }

months() { # months <YYYY.M...> -> YYYY*12+M; empty for anything else
  [[ $1 =~ ^([0-9]{4})\.([0-9]{1,2})(\.|$) ]] || return 1
  echo $(( 10#${BASH_REMATCH[1]} * 12 + 10#${BASH_REMATCH[2]} ))
}

checked=0
for host in "${hosts[@]}"; do
  if ! nix eval --json "$FLAKE#homeConfigurations.$host" --apply '
      c: let
        exts = builtins.concatMap (p: p.extensions.packages or [ ])
          (builtins.attrValues (c.config.programs.zen-browser.profiles or { }));
        # Parsed from the name: older rycee revs build addons without
        # pname/version attrs, and those are the revs this has to catch.
        bw = builtins.filter (d: d.name == "bitwarden")
          (map (p: builtins.parseDrvName p.name) exts);
      in {
        ext = map (d: d.version) bw;
        desktop = c.pkgs.bitwarden-desktop.version;
      }' >"$D/v" 2>"$D/err"; then
    fail "$host evaluates" "$(grep -E 'error:' "$D/err" | head -2 | tr '\n' ' ')"
    continue
  fi
  desktop=$(jq -r .desktop <"$D/v")
  mapfile -t exts < <(jq -r '.ext[]' <"$D/v")
  for ext in ${exts+"${exts[@]}"}; do
    checked=$((checked+1))
    if ! e=$(months "$ext") || ! d=$(months "$desktop"); then
      fail "$host: zen bitwarden $ext vs desktop $desktop" "not both YYYY.M versions"
      continue
    fi
    gap=$(( e - d ))
    if [ "${gap#-}" -le 6 ]; then
      pass "$host: zen bitwarden $ext is within 6 months of desktop $desktop"
    else
      fail "$host: zen bitwarden $ext vs desktop $desktop" \
           "$gap months apart; check which NUR/rycee rev the pin resolves to"
    fi
  done
done
# Every case above is skipped if the extension attr path changes under us; that
# must not read as green.
[ "$checked" -gt 0 ] || fail "some host installs the bitwarden extension" "found none; the version cases checked nothing"

echo
if [ "$fails" -eq 0 ]; then echo "all $n passed"; else echo "$fails of $n failed"; fi
exit $(( fails > 0 ))
