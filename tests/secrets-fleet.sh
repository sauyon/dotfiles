#!/usr/bin/env bash
# Cases for which hosts are in the dotfiles secrets fleet (report Part A).
#
# kyuusaku is a work box whose distribution we do not own. It is deliberately
# never enrolled with a device identity, so it has no way to decrypt
# secrets.yaml once the cluster-domain key leaves the fleet -- and it does not
# need any of the five secrets in the first place. So it is out of the fleet
# entirely: no sops.secrets, no sops environment, no sops-nix unit, and no
# consumer that would read a secret file that is never going to exist.
# setsuna is out for the same effect and a different reason: it is being
# decommissioned, and enrolling it would only mint a device key to revoke.
#
# What is protected here:
#
#   Out means all the way out. sops-nix's home-manager module turns itself off
#   when sops.secrets is empty -- no sops-install-secrets, no manifest, no
#   sops-nix.service, no activation step. That is what keeps an unenrolled host
#   from failing every activation on a decrypt it cannot do. A single secret
#   left declared for an out host (a new one added without the gate) would switch
#   the whole module back on, so the check is "the set is empty", not "these
#   five are gone".
#
#   In means unchanged. Every other host still declares every secret, and the
#   Linux ones still run the sops-nix unit. The expected set is read off a
#   member host rather than written down here, so adding a secret does not need
#   a test edit -- only adding it outside the gate does.
#
#   The one consumer that does not degrade. Every activation-time consumer
#   (opencode.nix, pi.nix, mcode.nix) guards its key file with [ -r ]; the
#   unifi MCP server instead `cat`s its key at launch, so on a host with no
#   secret it is a server that fails every start. It is registered only where
#   the secret exists.
#
#   Within the fleet, who is enrolled. A member decrypts either through its
#   device identity (wifHosts: the credential config is the store-built
#   external_account file) or through the cluster-domain gcp-key.json. Listing a
#   host in wifHosts before its JWK is published 401s it at the next activation,
#   and dropping one silently moves it back onto the key the migration is
#   retiring, so the split is pinned. Where the WIF key lives (TPM or file) is
#   tests/wif-tpm.sh's job, on the device.
#
#   ./tests/secrets-fleet.sh            # tests this checkout
#   ./tests/secrets-fleet.sh /path/to/flake
set -u

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

IN=(utsuho shiori fujiwara mari)
OUT=(kyuusaku setsuna)
LINUX_IN=(utsuho shiori fujiwara)
WIF=(shiori fujiwara utsuho)
GCP_KEY=(mari)

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

# nix eval's stderr is noise on a healthy box and the whole story on a broken
# one: held back, printed only on failure.
eval_json() { # eval_json <host> <nix apply fn body over `c` (the host config)>
  if ! nix eval --max-jobs 0 --json "$FLAKE#homeConfigurations.$1.config" \
       --apply "c: $2" 2>"$D/err"; then
    echo >&2; echo "nix eval failed for $1 ($2):" >&2; cat "$D/err" >&2
    exit 1
  fi
}

# One eval per host: everything below reads out of this.
facts() { # facts <host>
  [ -f "$D/$1.json" ] || eval_json "$1" '{
    secrets = builtins.attrNames c.sops.secrets;
    env = c.sops.environment;
    unit = c.systemd.user.services ? sops-nix;
    activation = c.home.activation ? sops-nix;
    ageFile = c.home.file ? ".config/sops/age-unused.txt";
    unifi = (c.programs.claude-code.settings.mcpServers or {}) ? unifi;
  }' > "$D/$1.json"
}
fact() { jq -c "$2" < "$D/$1.json"; }

check() { # check <what> <expected> <actual>
  n=$((n+1))
  if [ "$2" = "$3" ]; then
    echo "ok   $1"
  else
    echo "FAIL $1: expected '$2', got '$3'"; fails=$((fails+1))
  fi
}

for h in "${IN[@]}" "${OUT[@]}"; do facts "$h"; done

# --- out ---------------------------------------------------------------------
echo
for h in "${OUT[@]}"; do
  check "$h declares no sops secrets"           '[]'    "$(fact "$h" .secrets)"
  check "$h sets no sops environment"           '{}'    "$(fact "$h" .env)"
  check "$h has no sops-nix user unit"          false   "$(fact "$h" .unit)"
  check "$h has no sops-nix activation step"    false   "$(fact "$h" .activation)"
  check "$h writes no sops age placeholder"     false   "$(fact "$h" .ageFile)"
  check "$h registers no unifi MCP server"      false   "$(fact "$h" .unifi)"
done

# --- in ----------------------------------------------------------------------
echo
want=$(fact shiori .secrets)
for h in "${IN[@]}"; do
  check "$h declares the full secret set"       "$want" "$(fact "$h" .secrets)"
  check "$h points sops at a credential"        true \
    "$(fact "$h" '.env | has("GOOGLE_APPLICATION_CREDENTIALS")')"
  check "$h registers the unifi MCP server"     true    "$(fact "$h" .unifi)"
done
for h in "${LINUX_IN[@]}"; do
  check "$h runs the sops-nix user unit"        true    "$(fact "$h" .unit)"
done

# --- enrolled ----------------------------------------------------------------
echo
cred() { fact "$1" '.env.GOOGLE_APPLICATION_CREDENTIALS // "" | if endswith("-wif-hosts.json") then "device identity" elif endswith("/.config/sops/gcp-key.json") then "gcp-key.json" else . end'; }
for h in "${WIF[@]}"; do
  check "$h decrypts through its device identity" '"device identity"' "$(cred "$h")"
done
for h in "${GCP_KEY[@]}"; do
  check "$h still decrypts through gcp-key.json"  '"gcp-key.json"'    "$(cred "$h")"
done

# --- the teeth ---------------------------------------------------------------
# "full secret set" is read off shiori; if shiori lost its secrets every member
# would agree on [] and the cases above would pass having checked nothing.
echo
n=$((n+1))
if [ "$(jq length <<<"$want")" -gt 0 ]; then
  echo "ok   the reference host still declares $(jq length <<<"$want") secret(s)"
else
  echo "FAIL shiori declares no secrets: every 'in' case above is now vacuous"
  fails=$((fails+1))
fi

echo
if [ "$fails" -eq 0 ]; then echo "all $n passed"; else echo "$fails of $n failed"; fi
exit $(( fails > 0 ))
