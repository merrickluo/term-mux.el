;;; term-mux-frame.el --- Terminal multiplexer on new frame. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Merrick Luo
;;
;; Author: Merrick Luo <merrick@luois.me>
;; Maintainer: Merrick Luo <merrick@luois.me>
;; Created: August 19, 2024
;; Modified: August 19, 2024
;; Version: 0.0.1
;; Keywords: terminals processes
;; Homepage: https://github.com/merrickluo/term-mux
;; Package-Requires: ((emacs "24.3") (term-mux "0.0.1") (uuid "0.0.3"))
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
(require 'vterm nil)

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
  "Hook to run after a vterm BUFFER killed for term-mux frame.
Delete the frame if it's the last buffer in session."
  (let* ((buffer (or buffer (current-buffer)))
         (frame (window-frame (get-buffer-window buffer)))
         (session (term-mux--buffer-session buffer)))

    (when (frame-parameter frame 'term-mux-frame)
      ;; vterm requires us to kill the buffer
      (with-current-buffer buffer
        (when (provided-mode-derived-p major-mode 'vterm-mode)
          (kill-buffer buffer)))

      (when (term-mux-session-empty-p session)
        (delete-frame frame)))))

(add-hook 'vterm-exit-functions #'term-mux-frame--exit-fn)
(add-hook 'eshell-exit-hook #'term-mux-frame--exit-fn)

(provide 'term-mux-frame)
;;; term-mux-frame.el ends here
