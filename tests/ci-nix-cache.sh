#!/usr/bin/env bash
# Cases for nix-home.yml's node-local signed nix cache (/nix-cache).
#
# Why it exists: a typical run (job 826, d2e0ac38) spent ~3.5 of its ~5 minutes
# re-pulling ~1,800 unchanged closure paths into an empty store. The runner now
# mounts a file:// cache (kube repo, charts/forgejo-runner `nixCache`); this
# workflow reads it first and feeds it after a green run.
#
# The contract pinned here:
#   1. The container mounts /nix-cache, and NIX_CONFIG lists it as the first
#      substituter with priority=10 (a file:// cache defaults to 50, behind
#      cache.nixos.org's 40) and without trusted=1 (require-sigs is the whole
#      trust argument for sharing it).
#   2. A mount preflight runs before the first step that invokes nix-fetching
#      work (the build).
#   3. The save step is best-effort (continue-on-error) and, given path-info
#      output, copies exactly the signed paths -- an unsigned locally built path
#      (where the private input ends up) never goes in.
#
#   ./tests/ci-nix-cache.sh            # against the working tree
#   WF=/path/to/nix-home.yml ./tests/ci-nix-cache.sh
set -u

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
WF="${WF:-$ROOT/.forgejo/workflows/nix-home.yml}"

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0
ok()   { n=$((n+1)); echo "ok $n - $1"; }
fail() { n=$((n+1)); fails=$((fails+1)); echo "not ok $n - $1"; }
check() { if eval "$2"; then ok "$1"; else fail "$1"; fi; }

YQ=(nix run nixpkgs#yq-go --)
job='.jobs.build-and-push'

vols=$("${YQ[@]}" "$job.container.volumes[]" "$WF" 2>/dev/null)
check "container mounts /nix-cache" '[[ "$vols" == *"/nix-cache:/nix-cache"* ]]'

conf=$("${YQ[@]}" "$job.env.NIX_CONFIG" "$WF")
subs=$(printf '%s\n' "$conf" | while IFS= read -r l; do
  case $l in extra-substituters*) printf '%s' "${l#*= }";; esac; done)
check "file:///nix-cache is the first substituter" \
  '[[ "${subs%% *}" == file:///nix-cache\?* ]]'
check "it carries priority=10" '[[ "${subs%% *}" == *priority=10* ]]'
check "it is not trusted=1" '[[ "$subs" != *trusted=1* ]]'

names=$("${YQ[@]}" "$job.steps[].name" "$WF")
pre=-1; build=-1; i=0
while IFS= read -r s; do
  case $s in
    *"local nix cache must be mounted"*) pre=$i ;;
    "build home closures") build=$i ;;
  esac
  i=$((i+1))
done <<< "$names"
check "mount preflight exists and runs before the build" '[ "$pre" -ge 0 ] && [ "$pre" -lt "$build" ]'

save='.steps[] | select(.name == "save closure to the local nix cache")'
coe=$("${YQ[@]}" "$job | $save | .continue-on-error" "$WF")
check "save step is continue-on-error" '[ "$coe" = true ]'

# Run the save step's real script with nix/du stubbed: path-info returns a
# fixture with one unsigned (locally built) path, copy records its arguments.
"${YQ[@]}" "$job | $save | .run" "$WF" > "$D/save.sh"
mkdir -p "$D/bin"
cat > "$D/bin/nix" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  path-info)
    printf '%s\t%s\n' \
      /nix/store/aaa-glibc 'cache.nixos.org-1:sigA' \
      /nix/store/bbb-home-manager-path '' \
      /nix/store/ccc-zen 'kube:sigB cache.nixos.org-1:sigC' ;;
  copy) shift; printf '%s\n' "$@" > "$STUB_OUT" ;;
esac
STUB
printf '#!/usr/bin/env bash\nprintf "12\\t/nix-cache\\n"\n' > "$D/bin/du"
chmod +x "$D/bin/nix" "$D/bin/du"
printf '/nix/store/aaa-glibc\n' > "$D/out-paths"
# The script reads /tmp/out-paths; point it at the fixture.
script=$(< "$D/save.sh"); script=${script//\/tmp\/out-paths/$D\/out-paths}
STUB_OUT="$D/copied" PATH="$D/bin:$PATH" bash -c "$script" > "$D/save.log" 2>&1
copied=$(cat "$D/copied" 2>/dev/null)
check "save copies the signed paths" \
  '[[ "$copied" == *aaa-glibc* && "$copied" == *ccc-zen* ]]'
check "save skips the unsigned locally built path" '[[ "$copied" != *bbb-home-manager-path* ]]'
check "save targets the file:// cache with zstd" '[[ "$copied" == *"file:///nix-cache?compression=zstd"* ]]'

echo "$((n - fails))/$n passed"
[ "$fails" -eq 0 ]
