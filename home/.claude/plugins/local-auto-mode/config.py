import os
from pathlib import Path

# LAN address of the in-cluster litellm router (Cilium LoadBalancer). ai.ko.ag
# is gone -- it fronted this same router via a Workers façade and CF AI Gateway
# and 524'd structurally, because litellm sends no bytes until its upstream
# produces a first token and Cloudflare reads silence as a dead origin. See the
# kube repo's docs/litellm-access.md.
ENDPOINT = os.environ.get("LOCAL_CLASSIFIER_URL", "http://10.0.7.240:4000/v1")
# litellm's model-group name, not a lemonade id: it fails over lemonade -> Z.ai
# -> OpenRouter, so a cold or dead local model degrades instead of erroring.
MODEL = os.environ.get("LOCAL_CLASSIFIER_MODEL", "glm")
# The Qwen3.6-35B behind `glm` runs at ~60 tok/s and pays a multi-second prefill
# on the ~7k-token system prompt, so stage 2 lands around 6-12s. Give it
# headroom. This is now the ONLY deadline in the path -- the ~100s Cloudflare
# edge cap that used to sit in front of it is gone.
TIMEOUT = int(os.environ.get("LOCAL_CLASSIFIER_TIMEOUT", "25"))

_KEY_FILE = Path(os.environ.get(
    "LOCAL_CLASSIFIER_API_KEY_FILE",
    os.path.expanduser("~/.config/opencode/ko-ag-key"),
))


def get_api_key() -> str:
    env = os.environ.get("LOCAL_CLASSIFIER_API_KEY")
    if env:
        return env
    try:
        return _KEY_FILE.read_text().strip()
    except FileNotFoundError:
        raise RuntimeError(f"No API key: set LOCAL_CLASSIFIER_API_KEY or create {_KEY_FILE}")
