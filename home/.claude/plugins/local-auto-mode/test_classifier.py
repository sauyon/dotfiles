#!/usr/bin/env python3

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

import classifier


class ClassifierTests(unittest.TestCase):
    def with_settings(self, text: str):
        tmp = tempfile.TemporaryDirectory()
        path = Path(tmp.name) / "settings.json"
        path.write_text(text)
        self.addCleanup(tmp.cleanup)
        old_paths = classifier.SETTINGS_PATHS
        classifier.SETTINGS_PATHS = [path]
        self.addCleanup(lambda: setattr(classifier, "SETTINGS_PATHS", old_paths))
        old_codex_paths = getattr(classifier, "CODEX_CONFIG_PATHS", None)
        classifier.CODEX_CONFIG_PATHS = []
        self.addCleanup(lambda: setattr(classifier, "CODEX_CONFIG_PATHS", old_codex_paths))

    def test_default_read_tool_is_fast_allowed(self):
        self.with_settings("{}")

        rules = classifier.load_auto_mode_config()

        self.assertEqual(
            classifier.fast_path("Read", {"file_path": "/tmp/example"}, rules),
            ("allow", "fast-path default read-only tool"),
        )

    def test_configured_bash_prefix_can_be_fast_allowed(self):
        self.with_settings(json.dumps({
            "autoMode": {
                "fast_path": [
                    {
                        "tool": "Bash",
                        "command_prefix": "mise run quitesh:test",
                        "decision": "allow",
                        "reason": "trusted project test task",
                    }
                ]
            }
        }))

        rules = classifier.load_auto_mode_config()

        self.assertEqual(
            classifier.fast_path("Bash", {"command": "mise run quitesh:test --unit"}, rules),
            ("allow", "trusted project test task"),
        )

    def test_configured_regex_can_force_ask_without_llm(self):
        self.with_settings(json.dumps({
            "autoMode": {
                "fast_path": [
                    {
                        "tool": "Bash",
                        "command_regex": r"^git push\b.*\b(master|main)\b",
                        "decision": "ask",
                    }
                ]
            }
        }))

        rules = classifier.load_auto_mode_config()

        self.assertEqual(
            classifier.fast_path("Bash", {"command": "git push origin master"}, rules),
            ("ask", "fast-path configured rule"),
        )

    def test_configured_input_contains_can_deny(self):
        self.with_settings(json.dumps({
            "autoMode": {
                "fast_path": [
                    {
                        "tool": "Write",
                        "input_contains": {"file_path": "/.ssh/authorized_keys"},
                        "decision": "deny",
                        "reason": "do not persist ssh access",
                    }
                ]
            }
        }))

        rules = classifier.load_auto_mode_config()

        self.assertEqual(
            classifier.fast_path("Write", {"file_path": "/Users/me/.ssh/authorized_keys"}, rules),
            ("deny", "do not persist ssh access"),
        )

    def test_codex_toml_fast_path_is_loaded(self):
        tmp = tempfile.TemporaryDirectory()
        path = Path(tmp.name) / "config.toml"
        path.write_text("""
[autoMode]
[[autoMode.fast_path]]
tool = "Bash"
command_prefix = "cargo test"
decision = "allow"
reason = "trusted cargo tests"
""")
        self.addCleanup(tmp.cleanup)
        old_paths = classifier.SETTINGS_PATHS
        old_codex_paths = classifier.CODEX_CONFIG_PATHS
        classifier.SETTINGS_PATHS = []
        classifier.CODEX_CONFIG_PATHS = [path]
        self.addCleanup(lambda: setattr(classifier, "SETTINGS_PATHS", old_paths))
        self.addCleanup(lambda: setattr(classifier, "CODEX_CONFIG_PATHS", old_codex_paths))

        rules = classifier.load_auto_mode_config()

        self.assertEqual(
            classifier.fast_path("Bash", {"command": "cargo test --workspace"}, rules),
            ("allow", "trusted cargo tests"),
        )

    def test_claude_hook_input_normalizes(self):
        event = classifier.normalize_hook_input({
            "permission_mode": "default",
            "tool_name": "Bash",
            "tool_input": {"command": "git status"},
            "cwd": "/repo",
            "transcript_path": "/tmp/transcript.jsonl",
        })

        self.assertEqual(event.tool_name, "Bash")
        self.assertEqual(event.tool_input, {"command": "git status"})
        self.assertEqual(event.cwd, "/repo")
        self.assertEqual(event.transcript_path, "/tmp/transcript.jsonl")
        self.assertEqual(event.permission_mode, "default")

    def test_nested_codex_hook_input_normalizes(self):
        event = classifier.normalize_hook_input({
            "approval_policy": "on-request",
            "tool": {
                "name": "Bash",
                "input": {"command": "git status"},
            },
            "cwd": "/repo",
            "transcript": {"path": "/tmp/codex.jsonl"},
        })

        self.assertEqual(event.tool_name, "Bash")
        self.assertEqual(event.tool_input, {"command": "git status"})
        self.assertEqual(event.cwd, "/repo")
        self.assertEqual(event.transcript_path, "/tmp/codex.jsonl")
        self.assertEqual(event.permission_mode, "")


if __name__ == "__main__":
    unittest.main()
