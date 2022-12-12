;;; term-mux.el --- terminal multiplexer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Merrick Luo
;;
;; Author: Merrick Luo <merrick@luois.me>
;; Maintainer: Merrick Luo <merrick@luois.me>
;; Created: November 27, 2022
;; Modified: November 27, 2022
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

(defcustom term-mux-project-name-fn
  (cond
   ((fboundp 'projectile-project-root) #'projectile-project-name))
  "The function used to find current project name."
  :group 'term-mux
  :type 'function)

(defcustom term-mux-project-root-fn #'projectile-project-root
  "The function to find current project root."
  :group 'term-mux
  :type 'function)

(defcustom term-mux-terminal 'vterm
  "The implemention used to create terminal buffer."
  :group 'term-mux
  :type 'symbol
  :options '(vterm eshell))

(defcustom term-mux-buffer-prefix "*term-mux-"
  "The buffer prefix for term mux buffers."
  :group 'term-mux
  :type 'string)


(defvar term-mux--buffer-table (make-hash-table :test 'equal)
  "Currently opened term buffers grouped by session.")

(defvar-local term-mux--last-visited-table (make-hash-table :test 'equal)
  "Last visited term buffer grouped by session.")

(defvar-local term-mux--buffer-session nil
  "Buffer local variable to store current buffer's session.")

(defvar-local term-mux--buffer-index nil
  "Buffer local variable to store current buffer's index.")


(defun term-mux--add-to-buffer-table (session buffer)
  "Add new BUFFER to the buffer list under SESSION key."
  (if-let ((buffers (gethash session term-mux--buffer-table)))
      (puthash session (cl-pushnew buffer buffers) term-mux--buffer-table)
    (puthash session (list buffer) term-mux--buffer-table)))

(defun term-mux--handle-kill-buffer ()
  "Hook to run on term mux buffer get killed.

- Remove it from `term-mux--last-visited-table'
- Switch to other buffer in the same session if exists."
  (when (bound-and-true-p term-mux-mode)
    (let* ((session (buffer-local-value 'term-mux--buffer-session (current-buffer))))
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

;; TODO: make it customizable
(defun term-mux--session-name ()
  (if (projectile-project-p)
      (projectile-project-name)
    "global"))

;; TODO: make it customizable
(defun term-mux--session-root ()
  (if (projectile-project-p)
      (projectile-project-root)
    default-directory))

(defun term-mux--new-buffer (session &optional index)
  "Create & pop to a new term-mux buffer for SESSION.

use INDEX if provide."
  (let* ((default-directory (term-mux--session-root))
         (index (or index 1))
         (buffer (get-buffer-create (format "%s-%s-%d*" term-mux-buffer-prefix session index))))
    (term-mux--add-to-buffer-table session buffer)
    (with-current-buffer buffer
      (cond
       ((eq 'vterm term-mux-terminal) (term-mux--vterm-start))
       ((eq 'eshell term-mux-terminal) (term-mux--eshell-start))
       (t (error "Unsupported terminal: %s" term-mux-terminal)))

      (setq-local term-mux--buffer-session session)
      (setq-local term-mux--buffer-index index))
    buffer))

;; TODO: add keyword parameter to determine pop up is needed
(defun term-mux--show-buffer (buffer-or-name &optional session)
  "Show the BUFFER-OR-NAME in existing window of pop up a new window.
Show only buffers in SESSION if given."
  (let* ((buffer (get-buffer buffer-or-name))
         (session (or session (buffer-local-value 'term-mux--buffer-session buffer))))
    (puthash session buffer term-mux--last-visited-table)
    (if-let ((window (term-mux--current-window)))
        (progn
          (with-selected-window window
            (display-buffer buffer 'display-buffer-same-window))
          (select-window window))
      (pop-to-buffer buffer))))

(defun term-mux--current-session ()
  (or (buffer-local-value 'term-mux--buffer-session (current-buffer))
      (term-mux--session-name)))

(defun term-mux--list-buffers (&optional session)
  (let ((session (or session (term-mux--session-name))))
    (mapcar #'buffer-name (gethash session term-mux--buffer-table))))

(defun term-mux--vterm-start ()
  "Setup buffer for vterm."
  (unless (eq major-mode 'vterm-mode)
    (vterm-mode))
  (term-mux-mode))

(defun term-mux--eshell-start ()
  "Setup buffer for eshell."
  (unless (eq major-mode 'eshell-mode)
    (eshell-mode))
  (term-mux-mode))

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
           (buffer (or (gethash session term-mux--last-visited-table)
                       (term-mux--new-buffer session))))
      (term-mux--show-buffer buffer session))))

(defun term-mux-switch (buffer-or-name)
  "Switch to specific term buffer BUFFER-OR-NAME."
  (interactive
   (list (completing-read "Switch to:" (term-mux--list-buffers))))
  (term-mux--show-buffer buffer-or-name))

(defun term-mux-next (&optional direction)
  "Find or create next term mux buffer in DIRECTION."
  (interactive)
  (let ((session (buffer-local-value 'term-mux--buffer-session (current-buffer)))
        (index (buffer-local-value 'term-mux--buffer-index (current-buffer))))
    (if (and session index)
      (let ((next-index (+ index (or direction 1))))
        (if (<= next-index 0)
            (message "already on the first term.")
          (term-mux--show-buffer (term-mux--new-buffer session next-index) session)))
      (message "only works in a term-mux buffer."))))

(defun term-mux-prev ()
  "Find or create prev term mux buffer."
  (interactive)
  (term-mux-next -1))

(define-minor-mode term-mux-mode
  "Adds term mux utilities."
  :lighter "mux"
  :keymap (list (cons (kbd "C-c . n") #'term-mux-next)
                (cons (kbd "C-c . p") #'term-mux-prev)
                (cons (kbd "C-c . s") #'term-mux-switch))
  (if term-mux-mode
      (add-hook 'kill-buffer-hook #'term-mux--handle-kill-buffer)
    (remove-hook 'kill-buffer-hook #'term-mux--handle-kill-buffer)))

(provide 'term-mux)
;;; term-mux.el ends here
