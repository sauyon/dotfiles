# Remote darwin builder (mari)

The Linux boxes (utsuho, setsuna, fujiwara) delegate **aarch64-darwin** builds to
**mari-1** over the **LAN** — never over Tailscale, never over Kon WireGuard,
never over the public internet. CI is x86_64-linux only, and you can't
cross-build darwin from Linux, so this is the *only* path; but it requires
mari-1 to be on the same LAN as the Linux clients at the time of the build.

This is intentional, not a TODO. The earlier incarnation routed builders
through mari-1's Tailscale IP (`100.106.204.103`, CGNAT), which forced every
Linux client to be on the work tailnet (`tail1beac.ts.net`) just to push a
darwin build — a much wider surface than the build path itself needs. The LAN
wire does the same job with a single address, no tailnet dependency, and no
nix daemon-as-root crossing CGNAT egress you don't control. mari is also
**not** on Kon WireGuard (`10.9.0.0/24`), per the kube repo's
`docs/network-ip-map.md` inventory — `nix.conf`'s old builders line is the
"latent breakage for any WG-only host" called out in
`kube/tf/overlay-dns.tf`. The kube doc itself still describes mari as
"Tailscale-only"; that is now stale and should be rewritten next time the
kube repo is touched.

## Topology / naming gotcha

- The live Mac is **`mari-1`**. The dotfiles config is `darwinConfigurations.mari`
  (`hostname = mari`); that's a config label, not a target. The *builder
  target* is mari-1's current **LAN IP** — which can change if it leases a
  different DHCP address on the home network, so the `builders` line is the
  one place that needs re-checking when builds stop working. Currently pinned
  to **`10.0.10.70`**; pin a static DHCP reservation on the UCG if that
  address keeps drifting.
- The Tailscale node literally named `mari` (`100.68.16.116`) has been
  offline for months — ignore it.
- The `builders` line currently points at mari-1's LAN IP. We **do not** fall
  back to the old Tailscale IP and we do not advertise mari-1 as a Tailscale
  node for the build path; if mari-1's tailscaled is logged in (it shouldn't
  be by design), that is incidental.

## How it's wired

**Client side (Linux) — `system/etc/nix/nix.custom.conf` (rendered by
`system/deploy`):**
```
builders = ssh-ng://nixremote@10.0.10.70 aarch64-darwin /etc/nix/mari-builder-key 4 1 - - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUJ4am9mQVZnbHZoZXRlZzdxdjljNEVUbHNpRVI3azhQSmNkN3lNTDVPWTI=
builders-use-substitutes = true
```
The nix daemon runs as root, so it is root that SSHes to mari-1, using a
dedicated key (`sops:mariBuilderKey` rendered 0600 root by `system/deploy`).
The last field pins mari-1's LAN SSH host key (`ed25519`); that key is the
same one mari-1 presents on its Tailscale address — we did not need a new
TOFU cycle to confirm it on the LAN.

`nixremote` (NOT sauyon) is the SSH user: the OIDC `ForceCommand` would hijack
a sauyon-user non-interactive build, and the `nixremote` Match carve-out
bypasses it.

**Builder side (mari), in `flake.nix` `darwinConfigurations.mari`:**
- `nix.settings.trusted-users = [ "sauyon" ]` — the SSH build user must be a nix
  trusted-user to be allowed to run builds (nix-darwin keeps `root` too).
- The builder's **public** key is authorized via sops
  `ssh-authorized-keys-sauyon` (consumed by `services.openssh`
  → `AuthorizedKeysFile`). That secret was **empty** before this — so
  mari-1 had no key-based SSH access at all; it now authorizes the builder
  key. Add your personal pubkey to that secret too if you want
  interactive SSH to mari-1.

## Bringing it up

1. mari-1: `darwin-rebuild switch --flake .#mari` — authorizes the builder
   key and trusts `sauyon`. Until that runs, the daemon reaches mari-1 but
   gets `Permission denied (publickey)`.
2. Linux clients: `./system/deploy` (renders the key, deploys the conf).
3. Verify from a Linux box *while mari-1 is on the same LAN*:
   ```sh
   sudo ssh -i /etc/nix/mari-builder-key sauyon@10.0.10.70 true   # should succeed
   nix build --impure --expr '(builtins.getFlake (toString ./.)).darwinConfigurations.mari.config.system.build.toplevel' \
     --max-jobs 0   # forces remote build; should land on mari-1
   ```

## Caveats

- mari-1 is a laptop: if it's asleep, offline, or off the home network, the
  builders line can't reach it and aarch64-darwin builds on Linux clients
  fail. That is the new expected failure mode — it is no longer masked by
  Tailscale punching through CGNAT. Make mari reachability part of the
  build-system story (UCG wake-on-LAN, static DHCP reservation, etc.) if the
  backgrounded-laptop reality is a problem.
- This is a **builder only** — mari does not push to the attic cache
  (deliberate; no push token). See `nix-binary-cache.md`.
