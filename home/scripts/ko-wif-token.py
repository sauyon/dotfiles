#!/usr/bin/env python3
"""ko-wif-token: minimal ES256 JWT signer / JWK printer, python3 stdlib + openssl only.

Device-side half of the dotfiles trust root (reports/Homelab secrets bootstrap trust
root.md, Part A). Wrapped by home.nix as `ko-wif-token` and invoked by Google's auth
library as an executable-sourced credential from sops-nix's PATH="" unit.

  ko-wif-token --key wif.pem --jwk                                 -> public JWK (RFC 7638 kid)
  ko-wif-token --key wif.pem --iss I --sub S --aud A [--exp 300]   -> compact JWT
  ko-wif-token --key wif.pem --iss I --sub S --aud A --adc         -> GCP executable-credential JSON

The key may be a plain P-256 PEM or a TPM-resident "TSS2 PRIVATE KEY" blob from
install/wif/tpm-keygen.sh; see key_args for how the two are told apart and what
the TPM one needs in the environment.

Refuses to sign when the clock is not confirmed synced (see clock_ok).
"""
import argparse, base64, hashlib, json, os, subprocess, sys, time

# Absolute tool paths for use from a systemd unit with PATH="" (sops-nix sets PATH="").
OPENSSL = os.environ.get("KO_OPENSSL", "openssl")
TIMEDATECTL = os.environ.get("KO_TIMEDATECTL", "timedatectl")

def b64u(b: bytes) -> str:
    return base64.urlsafe_b64encode(b).rstrip(b"=").decode()

def key_args(key):
    """Extra openssl arguments this key needs, decided by the file itself.

    A TPM-resident key (install/wif/tpm-keygen.sh) is a "TSS2 PRIVATE KEY": a
    blob wrapped to one TPM, which openssl can only load through the tpm2
    provider. Naming any provider on the command line switches OFF the implicit
    default one, so `default` has to be named again or the digest, the encoders
    and the verify path all disappear with it.

    Sniffing the PEM header rather than taking a flag keeps the callers honest:
    the credential config in home.nix, device-test.sh and an interactive run all
    pass a path and nothing else, and a key that moved into the TPM must not
    need every one of them edited in the same commit to keep working.
    """
    try:
        with open(key, "rb") as f:
            head = f.readline()
    except OSError as e:
        raise SystemExit(f"ko-wif-token: {key}: {e.strerror}")
    # The provider is found via OPENSSL_MODULES, which the home.nix wrapper sets;
    # a TPM key with no provider fails in openssl, and loudly.
    return ["-provider", "tpm2", "-provider", "default"] if b"TSS2 PRIVATE KEY" in head else []

def pub_xy(key):
    args = key_args(key)
    try:
        # `pkey`, not `ec`: `ec` is the legacy EC-only command, and the TPM key's
        # public half comes out of the provider's generic store loader.
        der = subprocess.check_output([OPENSSL, "pkey", *args, "-in", key, "-pubout", "-outform", "DER"],
                                      stderr=subprocess.DEVNULL)
    except subprocess.CalledProcessError as e:
        hint = ("the tpm2 openssl provider could not load it: check OPENSSL_MODULES points at "
                "tpm2-openssl's ossl-modules, TPM2OPENSSL_TCTI at this host's TPM, and that you are in the tss group"
                if args else
                "generate one with: openssl ecparam -name prime256v1 -genkey -noout -out wif.pem")
        raise SystemExit(f"ko-wif-token: {key}: not a readable EC private key (openssl pkey exit {e.returncode}); {hint}")
    # Length first, then the point. `pkey` accepts any key type, so a P-384 key
    # reaches here with a longer SPKI whose last 65 bytes start with 0x04 about
    # one time in 256 -- and would then be split 32/32 into a silently wrong JWK
    # whose kid nothing local disagrees with. P-256 SPKI is exactly 91 bytes.
    if len(der) != 91:
        raise SystemExit(f"ko-wif-token: {key}: expected a 91-byte P-256 SubjectPublicKeyInfo, "
                         f"got {len(der)} bytes; is this a prime256v1 key?")
    pt = der[-65:]
    if pt[0] != 4:
        raise SystemExit(f"ko-wif-token: {key}: expected an uncompressed P-256 public point "
                         "(65 bytes, 0x04 prefix); is this a prime256v1 key?")
    return pt[1:33], pt[33:65]

def jwk(key):
    x, y = pub_xy(key)
    j = {"crv": "P-256", "kty": "EC", "x": b64u(x), "y": b64u(y)}
    thumb = hashlib.sha256(json.dumps(j, separators=(",", ":"), sort_keys=True).encode()).digest()
    j.update({"kid": b64u(thumb), "alg": "ES256", "use": "sig"})
    return j

def der_sig_to_raw(der: bytes) -> bytes:
    # SEQUENCE { INTEGER r, INTEGER s } -> raw r||s (JWS ES256 form). openssl emits
    # a well-formed DER signature; anything else here is a broken key or tool.
    if not der or der[0] != 0x30:
        raise ValueError("openssl did not return a DER SEQUENCE for the ECDSA signature")
    i = 2 if der[1] < 0x80 else 2 + (der[1] & 0x7f)
    out = b""
    for name in ("r", "s"):
        if i >= len(der) or der[i] != 0x02:
            raise ValueError(f"malformed ECDSA signature: expected INTEGER for {name}")
        i += 1
        ln = der[i]; i += 1
        v = der[i:i+ln]; i += ln
        v = v.lstrip(b"\x00")
        if len(v) > 32:
            raise ValueError(f"malformed ECDSA signature: {name} longer than 32 bytes")
        out += v.rjust(32, b"\x00")
    return out

def sign(key, iss, sub, aud, exp_s):
    now = int(time.time())
    hdr = {"alg": "ES256", "typ": "JWT", "kid": jwk(key)["kid"]}
    pl = {"iss": iss, "sub": sub, "aud": aud, "iat": now - 60, "nbf": now - 60, "exp": now + exp_s}
    msg = b64u(json.dumps(hdr, separators=(",", ":")).encode()) + "." + \
          b64u(json.dumps(pl, separators=(",", ":")).encode())
    der = subprocess.check_output([OPENSSL, "dgst", "-sha256", "-sign", key, *key_args(key)],
                                  input=msg.encode())
    return msg + "." + b64u(der_sig_to_raw(der)), pl["exp"]

def clock_ok(host="storage.googleapis.com"):
    """Refuse to sign if we cannot confirm sync or skew > 60s (report A3 clock guard)."""
    try:
        r = subprocess.run([TIMEDATECTL, "show", "-p", "NTPSynchronized", "--value"],
                           capture_output=True, text=True, timeout=5)
        if r.returncode == 0 and r.stdout.strip() == "yes":
            return True
    except Exception:
        pass
    try:
        import http.client, email.utils
        c = http.client.HTTPSConnection(host, timeout=5); c.request("HEAD", "/"); hdr = c.getresponse().getheader("Date")
        remote = email.utils.parsedate_to_datetime(hdr).timestamp()
        return abs(remote - time.time()) <= 60
    except Exception:
        return False

if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--key", required=True); ap.add_argument("--jwk", action="store_true")
    ap.add_argument("--iss"); ap.add_argument("--sub"); ap.add_argument("--aud")
    ap.add_argument("--exp", type=int, default=300); ap.add_argument("--adc", action="store_true")
    ap.add_argument("--skip-clock-check", action="store_true")
    a = ap.parse_args()
    if a.jwk:
        print(json.dumps(jwk(a.key), indent=1)); sys.exit(0)
    if not (a.iss and a.sub and a.aud):
        ap.error("--iss/--sub/--aud required")
    if not a.skip_clock_check and not clock_ok():
        msg = "ko-jwt: refusing to sign: clock not confirmed synced (NTPSynchronized!=yes and skew>60s vs storage.googleapis.com). Fix time sync first."
        if a.adc:
            print(json.dumps({"version": 1, "success": False, "code": "clock_skew", "message": msg}))
        else:
            print(msg, file=sys.stderr)
        sys.exit(1)
    tok, exp = sign(a.key, a.iss, a.sub, a.aud, a.exp)
    if a.adc:
        print(json.dumps({"version": 1, "success": True, "token_type": "urn:ietf:params:oauth:token-type:jwt",
                          "id_token": tok, "expiration_time": exp}))
    else:
        print(tok)
