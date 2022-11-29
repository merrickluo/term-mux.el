;;; vterm-mux.el vterm multiplexer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Merrick Luo
;;
;; Author: Merrick Luo <merrick@luois.me>
;; Maintainer: Merrick Luo <merrick@luois.me>
;; Created: November 27, 2022
;; Modified: November 27, 2022
;; Version: 0.0.1
;; Keywords: terminals processes
;; Homepage: https://github.com/merrickluo/vterm-mux
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


(defgroup vterm-mux nil
  "Vterm multiplexer."
  :group 'vterm)

(defcustom vterm-mux-project-find-fn
  (cond
   ((fboundp 'projectile-project-root) #'projectile-project-name))
  "The function used to find current project."
  :group 'vterm-mux
  :type 'function)


(defvar vterm-mux--prefix "*vterm-mux-"
  "The buffer prefix for vterm-mux buffers.")

(defvar vterm-mux--buffer-table (make-hash-table :test 'equal)
  "Currently opened vterm buffer lits.")

(defvar-local vterm-mux--buffer-prefix nil
  "Buffer local variable to store current buffer's prefix.")

(defvar-local vterm-mux--buffer-index nil
  "Buffer local variable to store current buffer's index.")


(defun vterm-mux--add-to-buffer-table (prefix buffer)
  "Add new BUFFER to the buffer list under PREFIX key."
  (if-let ((buffers (gethash prefix vterm-mux--buffer-table)))
      (puthash prefix (cl-pushnew buffer buffers) vterm-mux--buffer-table)
    (puthash prefix (list buffer) vterm-mux--buffer-table)))

(defun vterm-mux--handle-kill-buffer ()
  "Hook to run on vterm mux buffer get killed."
  (when (bound-and-true-p vterm-mux-mode)
    (let* ((prefix (buffer-local-value 'vterm-mux--buffer-prefix (current-buffer))))
      (if-let ((buffers (gethash prefix vterm-mux--buffer-table)))
          (puthash prefix (delq (current-buffer) buffers) vterm-mux--buffer-table)))))

(defun vterm-mux--current-window ()
  "Find the window currently displaying the vterm mux buffer."
  (let ((window nil))
    (maphash (lambda (_prefix buffers)
               (mapcar (lambda (buffer)
                         (if-let ((win (get-buffer-window buffer)))
                             (setq window win)))
                       buffers))
             vterm-mux--buffer-table)
    window))

(defun vterm-mux--new-buffer (prefix &optional index)
  "Create & pop to a new vterm-mux buffer with PREFIX.

use INDEX if provide."
  (let* ((index (or index 1))
         (buffer (get-buffer-create (format "%s-%d*" prefix index))))
    (vterm-mux--add-to-buffer-table prefix buffer)
    (with-current-buffer buffer
      (unless (eq major-mode 'vterm-mode)
        (vterm-mode)
        (vterm-mux-mode))
      (setq-local vterm-mux--buffer-prefix prefix)
      (setq-local vterm-mux--buffer-index index)
      buffer)))

;;;###autoload
(defun vterm-mux-toggle ()
  "Toggle vterm mux window.

If there is a window displaying a vterm mux buffer, switch to it.
Otherwise create or find the latest vterm mux buffer and pop up."
  (interactive)
  (if-let ((window (vterm-mux--current-window)))
      (select-window window)
    (let* ((project-name (funcall vterm-mux-project-find-fn))
           (prefix (format "%s-%s" vterm-mux--prefix project-name))
           (buffers (or (gethash prefix vterm-mux--buffer-table) '())))
      (pop-to-buffer (or (car buffers) (vterm-mux--new-buffer prefix))))))

(defun vterm-mux-next (&optional direction)
  "Find or create next vterm mux buffer in DIRECTION."
  (interactive)
  (let ((prefix (buffer-local-value 'vterm-mux--buffer-prefix (current-buffer)))
        (index (buffer-local-value 'vterm-mux--buffer-index (current-buffer))))
    (if (and prefix index)
      (let ((next-index (+ index (or direction 1))))
        (if (<= next-index 0)
            (message "already on the first vterm.")
          (display-buffer (vterm-mux--new-buffer prefix next-index) 'display-buffer-same-window)))
      (message "only works in a vterm-mux buffer."))))

(defun vterm-mux-prev ()
  "Find or create prev vterm mux buffer."
  (interactive)
  (vterm-mux-next -1))

(define-minor-mode vterm-mux-mode
  "Add vterm mux utilities to vterm-mode."
  :lighter "mux"
  (when (derived-mode-p 'vterm-mode)
    (add-hook 'kill-buffer-hook #'vterm-mux--handle-kill-buffer)))

(provide 'vterm-mux)
;;; vterm-mux.el ends here