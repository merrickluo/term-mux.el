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


(defvar term-mux--prefix "*term-mux-"
  "The buffer prefix for term mux buffers.")

(defvar term-mux--buffer-table (make-hash-table :test 'equal)
  "Currently opened term buffers grouped by prefix.")

(defvar-local term-mux--last-visited-table (make-hash-table :test 'equal)
  "Last visited term buffer grouped by prefix.")

(defvar-local term-mux--buffer-prefix nil
  "Buffer local variable to store current buffer's prefix.")

(defvar-local term-mux--buffer-index nil
  "Buffer local variable to store current buffer's index.")


(defun term-mux--add-to-buffer-table (prefix buffer)
  "Add new BUFFER to the buffer list under PREFIX key."
  (if-let ((buffers (gethash prefix term-mux--buffer-table)))
      (puthash prefix (cl-pushnew buffer buffers) term-mux--buffer-table)
    (puthash prefix (list buffer) term-mux--buffer-table)))

(defun term-mux--handle-kill-buffer ()
  "Hook to run on term mux buffer get killed."
  (when (bound-and-true-p term-mux-mode)
    (let* ((prefix (buffer-local-value 'term-mux--buffer-prefix (current-buffer))))
      (remhash prefix term-mux--last-visited-table)
      (when-let ((buffers (gethash prefix term-mux--buffer-table)))
        (let* ((others (delq (current-buffer) buffers)))
          (puthash prefix others term-mux--buffer-table)
          (if others
              (term-mux--show-buffer (car others))))))))

(defun term-mux--current-window ()
  "Find the window currently displaying the term mux buffer."
  (let ((window nil))
    (maphash (lambda (_prefix buffers)
               (mapcar (lambda (buffer)
                         (if-let ((win (get-buffer-window buffer)))
                             (setq window win)))
                       buffers))
             term-mux--buffer-table)
    window))

(defun term-mux--new-buffer (prefix &optional index)
  "Create & pop to a new term-mux buffer with PREFIX.

use INDEX if provide."
  (let* ((default-directory (funcall term-mux-project-root-fn))
         (index (or index 1))
         (buffer (get-buffer-create (format "%s-%d*" prefix index))))
    (term-mux--add-to-buffer-table prefix buffer)
    (with-current-buffer buffer
      (cond
       ((eq 'vterm term-mux-terminal) (term-mux--vterm-start))
       ((eq 'eshell term-mux-terminal) (term-mux--eshell-start))
       (t (error "unsupported terminal: %s" term-mux-terminal)))

      (setq-local term-mux--buffer-prefix prefix)
      (setq-local term-mux--buffer-index index))
    buffer))

;; TODO: add keyword parameter to determine pop up is needed
(defun term-mux--show-buffer (buffer-or-name &optional prefix)
  "Show the BUFFER in existing window of pop up a new window."
  (let* ((buffer (get-buffer buffer-or-name))
         (prefix (or prefix (buffer-local-value 'term-mux--buffer-prefix buffer))))
    (puthash prefix buffer term-mux--last-visited-table)
    (if-let ((window (term-mux--current-window)))
        (progn
          (with-selected-window window
            (display-buffer buffer 'display-buffer-same-window))
          (select-window window))
      (pop-to-buffer buffer))))

(defun term-mux--get-prefix ()
  (or (buffer-local-value 'term-mux--buffer-prefix (current-buffer))
      (let ((project-name (funcall term-mux-project-name-fn)))
        (format "%s-%s" term-mux--prefix project-name))))

(defun term-mux--list-buffers (&optional prefix)
  (let ((prefix (or prefix (term-mux--get-prefix))))
    (mapcar #'buffer-name (gethash prefix term-mux--buffer-table))))

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
    (let* ((prefix (term-mux--get-prefix))
           (buffer (or (gethash prefix term-mux--last-visited-table)
                       (term-mux--new-buffer prefix))))
      (term-mux--show-buffer buffer prefix))))

(defun term-mux-switch (buffer-or-name)
  "Switch to specific term buffer BUFFER-OR-NAME."
  (interactive
   (list (completing-read "Switch to:" (term-mux--list-buffers))))
  (term-mux--show-buffer buffer-or-name))

(defun term-mux-next (&optional direction)
  "Find or create next term mux buffer in DIRECTION."
  (interactive)
  (let ((prefix (buffer-local-value 'term-mux--buffer-prefix (current-buffer)))
        (index (buffer-local-value 'term-mux--buffer-index (current-buffer))))
    (if (and prefix index)
      (let ((next-index (+ index (or direction 1))))
        (if (<= next-index 0)
            (message "already on the first term.")
          (term-mux--show-buffer (term-mux--new-buffer prefix next-index) prefix)))
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
