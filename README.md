# dotfiles

Home Manager configuration for `sauyon`.

## New machine setup

```bash
# 1. Clone into ~/devel/dotfiles
mkdir -p ~/devel
git clone https://github.com/sauyon/dotfiles ~/devel/dotfiles

# 2. Write machine-local config (not committed)
cat > ~/devel/dotfiles/machine.nix << 'EOF'
{
  hostname = "mymachine";
  gui = true;  # false for headless servers
  # gpu = "amd";  # or "nvidia"; omit for Intel/none
  # mirrorOutput = "eDP-1";  # physical output the wayvnc headless display mirrors
}
EOF

# 3. Install Nix
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix \
  | sudo sh -s -- install --no-confirm

# 4. Source Nix and trust mise
. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
mise trust ~/devel/dotfiles/mise.toml

# 5. Apply
nix run github:nix-community/home-manager -- switch --flake ~/devel/dotfiles#sauyon
```

After the first switch, `home-manager switch` is available directly.

## System config

Files under `system/` mirror `/` and require root to deploy:

```bash
system/deploy
```

## Storage tuning

One-shot tuning for the btrfs-on-LUKS-on-loop-on-ext4 home stack: sets LUKS
workqueue-bypass flags, `noatime` on host `/home`, grows btrfs into LUKS
device slack. Idempotent. The LUKS step prompts for the passphrase.

```bash
system/storage-tuning.sh
```

## Secrets (sops-nix)

Secrets are decrypted using `~/.ssh/id_ed25519` as an age identity. On a new machine, either copy your existing key or generate one and add it as a sops recipient:

```bash
# Add new machine key as recipient
cd ~/devel/dotfiles
ssh-keygen -t ed25519 -f ~/.ssh/id_ed25519
mise run sops -- updatekeys secrets.yaml
```
