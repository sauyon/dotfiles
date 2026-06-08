# Local Auto Mode Codex Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the local auto-mode classifier work as a Codex-managed hook and add configurable fast-path rules.

**Architecture:** Keep the existing classifier entrypoint and prompt flow, but add a small normalization layer for hook inputs and a small rule engine for `autoMode.fast_path`. Home Manager installs the same plugin code into both Claude and Codex paths, and writes Codex's hook file to point at the Codex path.

**Tech Stack:** Python standard library, JSON/TOML settings, Home Manager Nix, direct Python script tests.

---

### Task 1: Classifier Fast-Path Rules

**Files:**
- Modify: `home/.claude/plugins/local-auto-mode/classifier.py`
- Create: `home/.claude/plugins/local-auto-mode/test_classifier.py`

- [ ] **Step 1: Write failing tests**

Add tests for default read allow, configured Bash prefix allow, configured regex ask, and configured deny.

- [ ] **Step 2: Run tests to verify they fail**

Run: `python3 home/.claude/plugins/local-auto-mode/test_classifier.py`

- [ ] **Step 3: Implement rule loading and matching**

Add `autoMode.fast_path` loading from Claude and Codex settings/config files. Support `tool`, `command`, `command_prefix`, `command_regex`, `input_equals`, `input_contains`, and decisions `allow`, `ask`, `deny`.

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 home/.claude/plugins/local-auto-mode/test_classifier.py`

### Task 2: Hook Input Normalization

**Files:**
- Modify: `home/.claude/plugins/local-auto-mode/classifier.py`
- Modify: `home/.claude/plugins/local-auto-mode/test_classifier.py`

- [ ] **Step 1: Write failing tests**

Add tests for Claude-style input and likely Codex-style nested tool input both normalizing to `(tool_name, tool_input, cwd, transcript_path, permission_mode)`.

- [ ] **Step 2: Run tests to verify they fail**

Run: `python3 home/.claude/plugins/local-auto-mode/test_classifier.py`

- [ ] **Step 3: Implement normalization**

Add `normalize_hook_input` and use it in `main`.

- [ ] **Step 4: Run tests to verify they pass**

Run: `python3 home/.claude/plugins/local-auto-mode/test_classifier.py`

### Task 3: Codex Installation

**Files:**
- Modify: `home.nix`

- [ ] **Step 1: Add Codex-managed files**

Install the local-auto-mode files under `~/.codex/plugins/local-auto-mode`.

- [ ] **Step 2: Add Codex hook file activation**

Write `~/.codex/hooks.json` with the local classifier and PR-create guard pointing to Codex paths.

- [ ] **Step 3: Validate syntax**

Run: `nixfmt --check home.nix`

### Task 4: End-to-End Verification

**Files:**
- Use existing files only.

- [ ] **Step 1: Simulate default allow**

Run classifier with a read-only tool and expect `permissionDecision` of `allow`.

- [ ] **Step 2: Simulate configured fast path**

Run classifier with a temporary settings file and expect configured allow/ask/deny behavior without network.

- [ ] **Step 3: Run repository checks**

Run the available format/test commands that do not require unavailable network access.
