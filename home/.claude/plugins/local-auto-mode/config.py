import os
from pathlib import Path

ENDPOINT = os.environ.get("LOCAL_CLASSIFIER_URL", "https://ai.ko.ag/v1")
MODEL = os.environ.get("LOCAL_CLASSIFIER_MODEL", "Huihui-Qwen3.6-35B-A3B-abliterated-Q4_K")
# ai.ko.ag's Qwen3.6-35B runs at ~60 tok/s and pays a multi-second prefill on
# the ~7k-token system prompt, so stage 2 lands around 6-12s. Give it headroom.
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
