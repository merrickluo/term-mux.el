;;; term-mux-frame.el --- Terminal multiplexer on new frame. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Merrick Luo
;;
;; Author: Merrick Luo <merrick@luois.me>
;; Maintainer: Merrick Luo <merrick@luois.me>
;; Created: August 19, 2024
;; Modified: February 11, 2026
;; Version: 0.0.2
;; Keywords: terminals processes
;; Homepage: https://github.com/merrickluo/term-mux
;; Package-Requires: ((emacs "24.3") (term-mux "0.0.2") (uuid "0.0.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Purpose of this package is to allow user to create a new frame
;; that starts a term-mux session, same as starting a new tmux session
;; with new terminal windows.
;;
;;
;;; Code:

(require 'term-mux)
(require 'uuid)
(require 'vterm nil t)
(require 'ghostel nil t)

(defgroup term-mux-frame nil
  "Terminal multiplexer for Frame."
  :group 'term-mux)

(defcustom term-mux-frame-session-name-fn #'uuid-string
  "Function to generate new session name."
  :group 'term-mux-frame
  :type 'function)

(defcustom term-mux-frame-name "Terminal"
  "Name for the new term mux frames."
  :group 'term-mux-frame
  :type 'string)

;;;###autoload
(defun term-mux-frame()
  "Setup term-mux in selected frame, assuming it's called from emacsclient."
  (let ((term-mux-session-name (funcall term-mux-frame-session-name-fn)))
    (set-frame-parameter (selected-frame) 'term-mux-frame t)
    (set-frame-name term-mux-frame-name)
    (switch-to-buffer (term-mux-buffer))))

(defun term-mux-frame--exit-fn (&optional buffer _event)
  "Hook to run after a terminal BUFFER process exits for term-mux frame.
Delete the frame if it's the last buffer in session."
  (let* ((buffer (or buffer (current-buffer)))
         (window (get-buffer-window buffer))
         (frame (if window (window-frame window) nil))
         (session (term-mux--buffer-session buffer)))

    (when (and frame (frame-parameter frame 'term-mux-frame))
      ;; Kill the buffer if the terminal process exited
      (with-current-buffer buffer
        (when (or (derived-mode-p 'vterm-mode)
                  (derived-mode-p 'ghostel-mode)
                  (derived-mode-p 'eshell-mode))
          (kill-buffer buffer)))

      (when (term-mux-session-empty-p session)
        (delete-frame frame)))))

(with-eval-after-load 'vterm
  (add-hook 'vterm-exit-functions #'term-mux-frame--exit-fn))
(with-eval-after-load 'ghostel
  (add-hook 'ghostel-exit-functions #'term-mux-frame--exit-fn))
(with-eval-after-load 'eshell
  (add-hook 'eshell-exit-hook #'term-mux-frame--exit-fn))

(provide 'term-mux-frame)
;;; term-mux-frame.el ends here
