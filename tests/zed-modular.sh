#!/usr/bin/env bash
# Cases for zed.nix: Zed on GLM 5.3 via Modular.
#
# The wiring spans three places that must agree and fail silently when they
# don't -- Zed just shows the provider as unauthenticated, or the agent panel
# falls back to no model:
#
#   * agent.default_model must name a provider and model that
#     language_models.openai_compatible actually defines.
#   * Zed reads that provider's key from `<ID>_API_KEY`, the provider id
#     upper-snake-cased (crates/language_models/src/provider/api_compatible.rs),
#     so the wrapper must export exactly that name. Renaming the provider
#     without the wrapper is the regression.
#   * The wrapper must read the key at launch (not embed it), and run Zed under
#     both nixVulkan and nixGL -- Zed renders through Vulkan, and without the
#     Vulkan half it falls back to llvmpipe on Arch.
#
#   ./tests/zed-modular.sh            # this host
#   ./tests/zed-modular.sh utsuho     # another desktop host
set -u
cd "$(dirname "$0")/.."

host="${1:-$(cat /etc/hostname)}"
cfg=".#homeConfigurations.$host.config.programs.zed-editor"
fail=0
check() { if eval "$2"; then echo "ok   $1"; else echo "FAIL $1"; fail=1; fi; }

settings=$(nix eval --json "$cfg.userSettings" 2>/dev/null) \
  || { echo "could not evaluate zed settings for $host" >&2; exit 1; }
provider=$(jq -r '.agent.default_model.provider' <<<"$settings")
model=$(jq -r '.agent.default_model.model' <<<"$settings")

check "default provider '$provider' is defined" \
  "jq -e --arg p '$provider' '.language_models.openai_compatible[\$p]' <<<'$settings' >/dev/null"
check "default model '$model' is listed under it" \
  "jq -e --arg p '$provider' --arg m '$model' '.language_models.openai_compatible[\$p].available_models | any(.name == \$m)' <<<'$settings' >/dev/null"
check "default model is GLM 5.3" "[ '$model' = zai-org/glm-5.3 ]"

pkg=$(nix build --no-link --print-out-paths "$cfg.package" 2>/dev/null) \
  || { echo "could not build zed package for $host" >&2; exit 1; }
wrapper="$pkg/bin/zeditor"
envvar="$(tr '[:lower:]-' '[:upper:]_' <<<"$provider")_API_KEY"

check "wrapper exports $envvar" "grep -q 'export $envvar' '$wrapper'"
check "wrapper reads the key at launch" "grep -q '$envvar=\"\$(cat ' '$wrapper'"
check "wrapper runs under nixVulkan" "grep -q '/bin/nixVulkan' '$wrapper'"
check "wrapper runs under nixGL" "grep -q '/bin/nixGL' '$wrapper'"

exit $fail
