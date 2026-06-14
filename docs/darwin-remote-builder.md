# Remote darwin builder (mari)

The Linux boxes (utsuho, setsuna, fujiwara) delegate **aarch64-darwin** builds to
**mari** over Tailscale, since nothing else can build darwin (CI is x86_64-linux
only, and you can't cross-build darwin from Linux).

## Topology / naming gotcha

- The live Mac is the Tailscale node **`mari-1`** (`100.106.204.103`, MagicDNS
  `mari-1.tail1beac.ts.net`). The node literally named `mari` (`100.68.16.116`)
  has been **offline for months** — ignore it.
- The dotfiles config is still `darwinConfigurations.mari` (hostname `mari`);
  that's just the config name. The *builder target* is `mari-1`'s Tailscale IP.
- We pin the **Tailscale IP**, not the MagicDNS name, in `builders`: root's
  resolver on the Linux boxes has no `*.ts.net` (MagicDNS/accept-dns off), and
  the Tailscale IP is stable per node.

## How it's wired

**Client side (Linux), in `system/etc/nix/nix.custom.conf` (→ `system/deploy`):**
```
builders = ssh-ng://sauyon@100.106.204.103 aarch64-darwin /etc/nix/mari-builder-key 4 1 - - <base64 mari-1 host key>
builders-use-substitutes = true
```
The nix **daemon runs as root**, so it's root that SSHes to mari-1, using a
dedicated key at `/etc/nix/mari-builder-key` (sops `mariBuilderKey`, rendered
0600 root by `system/deploy`). The last field pins mari-1's SSH host key.

**Builder side (mari), in `flake.nix` `darwinConfigurations.mari`:**
- `nix.settings.trusted-users = [ "sauyon" ]` — the SSH build user must be a nix
  trusted-user to be allowed to run builds (nix-darwin keeps `root` too).
- The builder's **public** key is authorized via sops `ssh-authorized-keys-sauyon`
  (consumed by `services.openssh` → `AuthorizedKeysFile`). That secret was
  **empty** before this — so mari-1 had no key-based SSH access at all; it now
  authorizes the builder key. You probably want to add your personal pubkey to
  that secret too.

## Bringing it up

1. Linux clients: `./system/deploy` (renders the key, sets `builders`).
2. **mari-1: `darwin-rebuild switch --flake .#mari`** — this is the step that
   authorizes the builder key and trusts `sauyon`. Until then, the daemon reaches
   mari-1 but gets `Permission denied (publickey)`.
3. Verify from a Linux box:
   ```sh
   sudo ssh -i /etc/nix/mari-builder-key sauyon@100.106.204.103 true   # should succeed
   nix build --impure --expr '(builtins.getFlake (toString ./.)).darwinConfigurations.mari.config.system.build.toplevel' \
     --max-jobs 0   # forces remote build; should land on mari-1
   ```

## Caveats

- mari-1 is a laptop: if it's asleep/offline, aarch64-darwin builds fail (no
  builder). That's inherent — there's no other darwin machine.
- This is a **builder only** — mari does not push to the attic cache (deliberate;
  no push token). See `nix-binary-cache.md`.
