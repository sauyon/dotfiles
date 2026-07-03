import os
from pathlib import Path

ENDPOINT = os.environ.get("LOCAL_CLASSIFIER_URL", "https://model.api.modular.com/v1")
MODEL = os.environ.get("LOCAL_CLASSIFIER_MODEL", "google/gemma-4-31b-it")
TIMEOUT = int(os.environ.get("LOCAL_CLASSIFIER_TIMEOUT", "10"))

_KEY_FILE = Path(os.environ.get(
    "LOCAL_CLASSIFIER_API_KEY_FILE",
    os.path.expanduser("~/.config/local-auto-mode/api-key"),
))


def get_api_key() -> str:
    env = os.environ.get("LOCAL_CLASSIFIER_API_KEY")
    if env:
        return env
    try:
        return _KEY_FILE.read_text().strip()
    except FileNotFoundError:
        raise RuntimeError(f"No API key: set LOCAL_CLASSIFIER_API_KEY or create {_KEY_FILE}")
