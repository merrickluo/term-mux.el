# term-mux

A Terminal Multiplexer for Emacs, inspired by tmux.

`term-mux` groups terminal buffers (using `vterm` or `eshell`) into "sessions", typically corresponding to [Projectile](https://github.com/bbatsov/projectile) projects. It provides a convenient way to manage multiple terminal instances associated with different working contexts.

## Features

- **Session Management**: Automatically groups terminals by project (via Projectile) or defaults to a "global" session.
- **Terminal Backend Support**: Supports `vterm` (default) and `eshell`.
- **Session Persistence**: Remembers the last visited terminal for each session.
- **Frame Integration**: Optional `term-mux-frame` extension to open sessions in dedicated frames.

## Installation

### Dependencies

- Emacs 25.1+
- [projectile](https://github.com/bbatsov/projectile)
- [vterm](https://github.com/akermu/emacs-libvterm) (optional, but recommended default)

### Manual Installation

Clone the repository and add it to your load path:

```elisp
(add-to-list 'load-path "/path/to/term-mux")
(require 'term-mux)
```

## Usage

### Basic Commands

- `M-x term-mux-create`: Create a new terminal in the current session.
- `M-x term-mux-toggle`: Toggle the terminal window. If a terminal exists for the session, it is shown; otherwise, a new one is created.
- `M-x term-mux-switch-to`: Switch to a specific terminal buffer in the current session.
- `M-x term-mux-next`: Switch to the next terminal in the current session.
- `M-x term-mux-prev`: Switch to the previous terminal in the current session.

### Keybindings

You can set up a keymap prefix for convenient access:

```elisp
(global-set-key (kbd "C-c t") term-mux-command-map)
```

This binds:
- `C-c t c`: Create new terminal
- `C-c t s`: Switch terminal
- `C-c t n`: Next terminal
- `C-c t p`: Previous terminal
- `C-c t v`: Create vterm
- `C-c t e`: Create eshell

### Customization

- `term-mux-default-terminal-setup-fn`: Function to setup the terminal (default: `#'term-mux--setup-vterm`).
- `term-mux-buffer-prefix`: Prefix for terminal buffer names (default: `"*term-mux-"`).

## Extensions

### term-mux-frame

Provides functionality to open a term-mux session in a new frame.

```elisp
(require 'term-mux-frame)
```

- `M-x term-mux-frame`: Open a new frame with a dedicated term-mux session.
