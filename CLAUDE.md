# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview
`term-mux` is an Emacs Lisp package that provides a terminal multiplexer functionality within Emacs. It groups terminal buffers (using `vterm` or `eshell`) into "sessions", typically corresponding to Projectile projects.

## Development Commands
Since this is a simple Emacs Lisp package without a dedicated build system (like Cask or Eldev), use `emacs` in batch mode for verification.

- **Byte Compile** (Verify syntax/warnings):
  ```bash
  emacs -Q -batch -L . -f batch-byte-compile term-mux.el
  ```
- **Linting**:
  Standard Emacs `checkdoc` is recommended.

## Architecture
- **Session Model**: Terminals are grouped by "session". A session is automatically derived from the current Projectile project name, or defaults to "global" if not in a project.
- **State Management**:
  - `term-mux--buffer-table`: A global hash table mapping session names to a list of terminal buffers.
  - `term-mux--last-visited-table`: Tracks the most recently used buffer for each session.
- **Buffer Local Variables**:
  - `term-mux--buffer-session`: The session a terminal belongs to.
  - `term-mux--buffer-slot`: An integer index used to order terminals within a session.
- **Integration**:
  - Relies on `projectile` for project detection.
  - Uses `vterm` (default) or `eshell` for the actual terminal implementation.
  - `term-mux-mode`: A minor mode active in managed terminal buffers to handle cleanup (hooks).

## Key Functions
- `term-mux-create`: Creates a new terminal in the current session.
- `term-mux-toggle`: Toggles visibility of the terminal window or creates one if missing.
- `term-mux--handle-kill-buffer`: Hook that ensures internal state is updated when a terminal is closed.
