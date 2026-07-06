# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview
`term-mux` is an Emacs Lisp package that provides a terminal multiplexer within Emacs. It groups terminal buffers into "sessions", typically corresponding to projects.

## Dependencies
- **Hard requirement**: Emacs 25.1+
- **Project detection** (auto-detected, at least one needed): `project.el` (built-in) or `projectile` (optional)
- **Terminal backends** (auto-detected, at least one needed): `ghostel`, `vterm`, or `eshell` (built-in)

## Development Commands
Since this is a simple Emacs Lisp package without a dedicated build system (like Cask or Eldev), use `emacs` in batch mode for verification.

- **Byte Compile** (Verify syntax/warnings):
  ```bash
  emacs -Q -batch -L . -f batch-byte-compile term-mux.el
  ```
- **Run Tests**:
  ```bash
  emacs -Q -batch -L . -l term-mux-test.el -f ert-run-tests-batch-and-exit
  ```

## Architecture
- **Session Model**: Terminals are grouped by "session". A session name is derived from: customizable `term-mux-session-name` → projectile project name → `project.el` project name → `"global"`.
- **State Management**:
  - `term-mux--buffer-table`: A global hash table mapping session names to a list of terminal buffers.
  - `term-mux--last-visited-table`: Tracks the most recently used buffer for each session.
- **Buffer Local Variables**:
  - `term-mux--buffer-session`: The session a terminal belongs to.
  - `term-mux--buffer-slot`: An integer index used to order terminals within a session.
- **Terminal Backends**: `term-mux--detect-terminal` auto-detects the first available backend (ghostel → vterm → eshell). Each backend has a `--setup-X` function and a `term-mux-create-X` command.
- **Integration**:
  - `project.el` (built-in) for project detection, with `projectile` as optional override.
  - `term-mux-mode`: A minor mode active in managed terminal buffers to handle cleanup (hooks).

## Key Functions
- `term-mux-create`: Creates a new terminal in the current session.
- `term-mux-toggle`: Toggles visibility of the terminal window or creates one if missing.
- `term-mux--handle-kill-buffer`: Hook that ensures internal state is updated when a terminal is closed.
- `term-mux--detect-terminal`: Auto-detects and activates the first available terminal backend.
