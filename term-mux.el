;;; term-mux.el --- Terminal multiplexer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Merrick Luo
;;
;; Author: Merrick Luo <merrick@luois.me>
;; Maintainer: Merrick Luo <merrick@luois.me>
;; Created: November 27, 2022
;; Modified: December 20, 2022
;; Version: 0.0.1
;; Keywords: terminals processes
;; Homepage: https://github.com/merrickluo/term-mux
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'projectile nil)
(require 'project nil)
(require 'subr-x)
(require 'vterm)


(defgroup term-mux nil
  "Terminal multiplexer."
  :group 'terminals)

(defcustom term-mux-buffer-prefix "*term-mux-"
  "The buffer prefix for term mux buffers."
  :group 'term-mux
  :type 'string)

(defcustom term-mux-default-terminal-setup-fn #'term-mux--setup-vterm
  "The default terminal buffer setup function."
  :group 'term-mux
  :type 'function)


(defvar term-mux--buffer-table (make-hash-table :test 'equal)
  "Currently opened term buffers grouped by session.")

(defvar-local term-mux-session-name nil
  "Can be used to customize session name.")

(defvar-local term-mux-session-root nil
  "Can be used to customize session root.")

(defvar-local term-mux--last-visited-table (make-hash-table :test 'equal)
  "Last visited term buffer grouped by session.")

(defvar-local term-mux--buffer-session nil
  "Buffer local variable to store current buffer's session.")

(defvar-local term-mux--buffer-slot nil
  "Buffer local variable to store current buffer's slot.")


(defun term-mux--add-to-buffer-table (session buffer)
  "Add new BUFFER to the buffer list under SESSION key."
  (if-let ((buffers (gethash session term-mux--buffer-table)))
      (let ((new-buffers (cl-pushnew buffer buffers)))
        (cl-sort new-buffers #'< :key #'term-mux--buffer-slot)
        (puthash session new-buffers term-mux--buffer-table))
    (puthash session (list buffer) term-mux--buffer-table)))

(defun term-mux--handle-kill-buffer ()
  "Hook to run on term mux buffer get killed.

- Remove it from `term-mux--last-visited-table'
- Switch to other buffer in the same session if exists."
  (when (bound-and-true-p term-mux-mode)
    (let* ((session (term-mux--buffer-session)))
      (remhash session term-mux--last-visited-table)
      (when-let ((buffers (gethash session term-mux--buffer-table)))
        (let* ((others (delq (current-buffer) buffers)))
          (puthash session others term-mux--buffer-table)
          (if others
              (term-mux--show-buffer (car others))))))))

(defun term-mux--current-window ()
  "Find the window currently displaying the term mux buffer."
  (let ((window nil))
    (maphash (lambda (_session buffers)
               (mapcar (lambda (buffer)
                         (if-let ((win (get-buffer-window buffer)))
                             (setq window win)))
                       buffers))
             term-mux--buffer-table)
    window))

(defun term-mux--session-name ()
  (or term-mux-session-name
      (if (projectile-project-p)
          (projectile-project-name)
        "global")))

(defun term-mux--session-root ()
  (or term-mux-session-root
      (if (projectile-project-p)
          (projectile-project-root)
        default-directory)))

(defun term-mux--new-buffer (session terminal-setup-fn slot)
  (let* ((default-directory (term-mux--session-root))
         (slot (or slot 0))
         (buffer (get-buffer-create (format "%s-%s-%d*" term-mux-buffer-prefix session slot))))
    (with-current-buffer buffer
      (funcall (or terminal-setup-fn term-mux-default-terminal-setup-fn))

      (setq-local term-mux--buffer-session session)
      (setq-local term-mux--buffer-slot slot)
      (term-mux--add-to-buffer-table session buffer))
    buffer))

;; TODO: add keyword parameter to determine pop up is needed
(defun term-mux--show-buffer (buffer-or-name &optional session)
  "Show the BUFFER-OR-NAME in existing window of pop up a new window.
Show only buffers in SESSION if given."
  (let* ((buffer (get-buffer buffer-or-name))
         (session (or session (term-mux--buffer-session buffer))))
    (puthash session buffer term-mux--last-visited-table)
    (if-let ((window (term-mux--current-window)))
        (progn
          (with-selected-window window
            (display-buffer buffer 'display-buffer-same-window))
          (select-window window))
      (pop-to-buffer buffer))))

(defun term-mux--current-session ()
  (or (term-mux--buffer-session)
      (term-mux--session-name)))

(defun term-mux--list-buffers (&optional session)
  (let ((session (or session (term-mux--session-name))))
    (mapcar #'buffer-name (gethash session term-mux--buffer-table))))

(defun term-mux--setup-vterm ()
  "Setup buffer for vterm."
  (unless (eq major-mode 'vterm-mode)
    (vterm-mode))
  (term-mux-mode))

(defun term-mux--setup-eshell ()
  "Setup buffer for eshell."
  (unless (eq major-mode 'eshell-mode)
    (eshell-mode))
  (term-mux-mode))

(defun term-mux--buffer-slot (&optional buffer)
  (buffer-local-value 'term-mux--buffer-slot (or buffer (current-buffer))))

(defun term-mux--buffer-session (&optional buffer)
  (buffer-local-value 'term-mux--buffer-session (or buffer (current-buffer))))

(defun term-mux--find-empty-slot (session)
  "Find a empty slot in SESSION."
  (let ((buffers (gethash session term-mux--buffer-table))
        (next-fn (lambda (next-fn buffers slot)
                   (if (and (car buffers)
                            (= (term-mux--buffer-slot (car buffers)) slot))
                       (funcall next-fn next-fn (cdr buffers) (+ slot 1))
                     slot))))
    (funcall next-fn next-fn buffers 0)))

(defun term-mux--next-buffer (session slot direction)
  (let ((buffers (gethash session term-mux--buffer-table))
        (next-fn (lambda (next-fn buffers slot)
                   (if (and (car buffers) (= (term-mux--buffer-slot (car buffers)) slot))
                       (cadr buffers)
                     (funcall next-fn next-fn (cdr buffers) slot)))))
    (if (< direction 0)
        (funcall next-fn next-fn (reverse buffers) slot)
      (funcall next-fn next-fn buffers slot))))


;;;###autoload
(defun term-mux-toggle ()
  "Toggle term mux window.

If there is a window displaying a term mux buffer, switch to it.
Otherwise create or find the latest term mux buffer and pop up."
  (interactive)
  (if-let ((window (term-mux--current-window)))
      (if (eq window (selected-window))
          (delete-window window)
        (select-window window))
    (let* ((session (term-mux--current-session))
           (buffer (or (gethash session term-mux--last-visited-table))))
      (if buffer
          (term-mux--show-buffer buffer session)
        (term-mux-create term-mux-default-terminal-setup-fn session)))))

;;;###autoload
(defun term-mux-create (&optional terminal-setup-fn session)
  "Create a new term mux buffer in SESSION with type SHELL.

SESSION defaults to current project name,
or session associated with current buffer.
TERMINAL-SETUP-FN defaults to `term-mux-default-terminal-setup-fn'"
  (interactive)
  (let* ((session (or session (term-mux--current-session)))
         (buffer (term-mux--new-buffer session
                                       terminal-setup-fn
                                       (term-mux--find-empty-slot session))))
    (term-mux--show-buffer buffer session)))

;;;###autoload
(defun term-mux-create-eshell (&optional session)
  "Create a eshell buffer and attach it to current term mux SESSION."
  (interactive)
  (term-mux-create #'term-mux--setup-eshell session))

(defun term-mux-create-vterm (&optional session)
  "Create a eshell buffer and attach it to current term mux SESSION."
  (interactive)
  (term-mux-create #'term-mux--setup-vterm session))

(defun term-mux-switch-to (buffer-or-name)
  "Switch to specific term buffer BUFFER-OR-NAME."
  (interactive
   (list (completing-read "Switch to:" (term-mux--list-buffers))))
  (term-mux--show-buffer buffer-or-name))

(defun term-mux-next (&optional direction)
  "Find or create next term mux buffer in DIRECTION."
  (interactive)
  (let ((session (term-mux--buffer-session))
        (slot (term-mux--buffer-slot)))
    (if (and session slot)
        (if-let ((buffer (term-mux--next-buffer session slot (or direction 1))))
            (term-mux--show-buffer buffer)
          (error "no next term in session"))
      (error "this function only works in term-mux buffers."))))

(defun term-mux-prev ()
  "Shortcut for `(term-mux-next -1)'"
  (interactive)
  (term-mux-next -1))

(defvar term-mux-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'term-mux-next)
    (define-key map "p" #'term-mux-prev)
    (define-key map "s" #'term-mux-switch-to)
    (define-key map "c" #'term-mux-create)
    (define-key map "e" #'term-mux-create-eshell)
    (define-key map "v" #'term-mux-create-vterm)
    map))

(fset 'term-mux-command-map term-mux-command-map)


(define-minor-mode term-mux-mode
  "Adds term mux utilities."
  :lighter "mux"
  :keymap '()
  (if term-mux-mode
      (add-hook 'kill-buffer-hook #'term-mux--handle-kill-buffer)
    (remove-hook 'kill-buffer-hook #'term-mux--handle-kill-buffer)))

(provide 'term-mux)
;;; term-mux.el ends here
