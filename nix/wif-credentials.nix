# The credential a dotfiles host decrypts secrets.yaml with: a Google
# external-account ("workload identity federation") config that shells out to a
# local signer, plus the signer itself. No decryption key is on disk -- the host
# proves possession of a P-256 key, Google STS federates the resulting JWT, and
# KMS host-key does the unwrap. Design: reports/…trust root.md, Part A.
#
# This is its own file because TWO configurations need the identical credential
# for the same host: home.nix, for the user's sops-nix, and
# darwinConfigurations.mari, for nix-darwin's -- the latter runs as root and so
# cannot reach home.nix's `let`. A device identity IS the tuple (iss, sub, aud)
# plus the key that signs for it, and the published JWKS authorises exactly that
# tuple; a second copy of this expression is a fourfold chance for one of them
# to drift, and the failure mode of a drifted one is an STS 401 at activation on
# a host whose secrets are, by then, encrypted to nothing else.
{
  pkgs,
  lib,
  # Signed into `sub` as `device:<hostname>`. The trust root maps (iss, sub) to
  # a tier, and `sub` is attacker-chosen, so the tier is the whole authorization
  # decision -- see the report's A3.
  hostname,
  # Absolute path to the PEM this host signs with. Read at activation time by
  # the signer, never at build time: it must not reach the store.
  keyFile,
  # true when keyFile is a TSS2 handle rather than a plain P-256 key, which
  # openssl can load only through the tpm2 provider. Darwin has no TPM, so a
  # darwin caller passes false.
  useTpm,
  # From the dotfiles-private flake input. Naming the provider resource in a
  # public repo would publish the one component of the audience that is not
  # already derivable from the bucket.
  audience,
  # Where the authorised kids are published. Defaulted rather than required:
  # every caller wants the production bucket, and a second spelling of it is a
  # quiet way to sign for an issuer nobody validates.
  issuer ? "https://storage.googleapis.com/ko-keys-sauyon/hosts",
}:
let
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  # sops-nix runs sops-install-secrets with PATH="" — every path here is absolute.
  koWifToken = pkgs.writeShellScriptBin "ko-wif-token" ''
    export KO_OPENSSL=${pkgs.openssl}/bin/openssl
    ${lib.optionalString (!isDarwin) "export KO_TIMEDATECTL=/usr/bin/timedatectl"}
    ${lib.optionalString useTpm ''
      # openssl loads the TPM key only through this provider, and finds providers
      # by OPENSSL_MODULES. Both come from the same `pkgs`, which is the point —
      # but note nothing enforces that at runtime: tpm2.so's RUNPATH holds no
      # openssl at all, so it resolves libcrypto from the loading process and any
      # ABI-compatible OpenSSL 3.x would load it. The pairing is a build-time
      # header dependency, kept honest here by both names coming from one pkgs.
      export OPENSSL_MODULES=${pkgs.tpm2-openssl}/lib/ossl-modules
      # Same reason as gnome-keyring-tpm in home.nix: the nixpkgs TSS defaults to
      # tcti-abrmd, a resource-manager daemon this host does not run. /dev/tpmrm0
      # is the kernel's own resource manager and needs only the tss group.
      export TPM2OPENSSL_TCTI=device:/dev/tpmrm0
    ''}
    exec ${pkgs.python3}/bin/python3 ${../home/scripts/ko-wif-token.py} "$@"
  '';
in
# Non-secret by construction (GCP documents credential configs as safe to commit).
pkgs.writeText "wif-hosts.json" (builtins.toJSON {
  type = "external_account";
  inherit audience;
  subject_token_type = "urn:ietf:params:oauth:token-type:jwt";
  token_url = "https://sts.googleapis.com/v1/token";
  credential_source.executable = {
    command = lib.concatStringsSep " " [
      "${koWifToken}/bin/ko-wif-token"
      "--key" keyFile
      "--iss" issuer
      "--sub" "device:${hostname}"
      "--aud" audience
      "--adc"
    ];
    timeout_millis = 10000;
  };
})
