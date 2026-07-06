;;; term-mux-test.el --- Tests for term-mux.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for term-mux.el functionality

;;; Code:

(require 'ert)
(require 'term-mux)

(ert-deftest term-mux-test-session-name ()
  "Test session name generation."
  (let ((default-directory "/tmp/"))
    ;; Test with custom session name
    (let ((term-mux-session-name "custom-session"))
      (should (string= (term-mux--session-name) "custom-session")))

    ;; Test with projectile project when projectile is available
    (cl-letf (((symbol-function 'featurep) (lambda (feature) (eq feature 'projectile)))
              ((symbol-function 'projectile-project-p) (lambda () t))
              ((symbol-function 'projectile-project-name) (lambda () "test-project")))
      (let ((term-mux-session-name nil))
        (should (string= (term-mux--session-name) "test-project"))))

    ;; Test with project.el when projectile is not available
    (cl-letf (((symbol-function 'featurep) (lambda (feature) nil))
              ((symbol-function 'project-current) (lambda () (list 'project)))
              ((symbol-function 'project-root) (lambda (_) "/tmp/test-proj/"))
              ((symbol-function 'fboundp) (lambda (_) nil)))
      (let ((term-mux-session-name nil))
        (should (string= (term-mux--session-name) "test-proj"))))

    ;; Test fallback to global when no project system is available
    (cl-letf (((symbol-function 'featurep) (lambda (feature) nil))
              ((symbol-function 'project-current) (lambda () nil)))
      (let ((term-mux-session-name nil))
        (should (string= (term-mux--session-name) "global"))))))

(ert-deftest term-mux-test-buffer-table ()
  "Test buffer table management."
  (let ((term-mux--buffer-table (make-hash-table :test 'equal))
        (test-buffer (generate-new-buffer "*test-term*")))
    (unwind-protect
        (progn
          ;; Test adding buffer
          (term-mux--add-to-buffer-table "test-session" test-buffer)
          (should (equal (gethash "test-session" term-mux--buffer-table)
                        (list test-buffer)))

          ;; Test adding another buffer to same session
          (let ((test-buffer-2 (generate-new-buffer "*test-term-2*")))
            (unwind-protect
                (progn
                  (term-mux--add-to-buffer-table "test-session" test-buffer-2)
                  (should (equal (length (gethash "test-session" term-mux--buffer-table)) 2))
                  (should (member test-buffer (gethash "test-session" term-mux--buffer-table)))
                  (should (member test-buffer-2 (gethash "test-session" term-mux--buffer-table))))
              (kill-buffer test-buffer-2))))
      (kill-buffer test-buffer))))

(ert-deftest term-mux-test-empty-slot ()
  "Test finding empty slots."
  (let ((term-mux--buffer-table (make-hash-table :test 'equal))
        (session "test-session"))
    ;; Test with no buffers
    (should (= (term-mux--find-empty-slot session) 0))

    ;; Test with one buffer at slot 0
    (let ((test-buffer (generate-new-buffer "*test-term*")))
      (unwind-protect
          (progn
            (with-current-buffer test-buffer
              (setq term-mux--buffer-slot 0))
            (term-mux--add-to-buffer-table session test-buffer)
            (should (= (term-mux--find-empty-slot session) 1)))
        (kill-buffer test-buffer)))))

(ert-deftest term-mux-test-session-empty-p ()
  "Test session empty check."
  (let ((term-mux--buffer-table (make-hash-table :test 'equal)))
    ;; Test empty session
    (should (term-mux-session-empty-p "test-session"))

    ;; Test non-empty session
    (let ((test-buffer (generate-new-buffer "*test-term*")))
      (unwind-protect
          (progn
            (term-mux--add-to-buffer-table "test-session" test-buffer)
            (should-not (term-mux-session-empty-p "test-session")))
        (kill-buffer test-buffer)))))

(ert-deftest term-mux-test-next-buffer ()
  "Test next buffer navigation."
  (let ((term-mux--buffer-table (make-hash-table :test 'equal))
        (session "test-session")
        buffer-1
        buffer-2)
    (unwind-protect
        (progn
          ;; Create and setup test buffers
          (setq buffer-1 (generate-new-buffer "*test-term-1*"))
          (setq buffer-2 (generate-new-buffer "*test-term-2*"))

          ;; Setup buffers
          (with-current-buffer buffer-1
            (setq term-mux--buffer-slot 0))
          (with-current-buffer buffer-2
            (setq term-mux--buffer-slot 1))

          (term-mux--add-to-buffer-table session buffer-1)
          (term-mux--add-to-buffer-table session buffer-2)

          ;; Test forward navigation
          (should (eq (term-mux--next-buffer session 0 1) buffer-2))
          ;; Test backward navigation
          (should (eq (term-mux--next-buffer session 1 -1) buffer-1)))
      ;; Cleanup
      (when buffer-1 (kill-buffer buffer-1))
      (when buffer-2 (kill-buffer buffer-2)))))

(ert-deftest term-mux-test-last-visited ()
  "Test last visited buffer tracking."
  (let ((term-mux--buffer-table (make-hash-table :test 'equal))
        (term-mux--last-visited-table (make-hash-table :test 'equal))
        (session "test-session"))
    ;; Create test buffers
    (let ((buffer-1 (generate-new-buffer "*test-term-1*"))
          (buffer-2 (generate-new-buffer "*test-term-2*")))
      (unwind-protect
          (progn
            ;; Setup buffers
            (with-current-buffer buffer-1
              (setq term-mux--buffer-slot 0)
              (setq term-mux--buffer-session session))
            (with-current-buffer buffer-2
              (setq term-mux--buffer-slot 1)
              (setq term-mux--buffer-session session))

            (term-mux--add-to-buffer-table session buffer-1)
            (term-mux--add-to-buffer-table session buffer-2)

            ;; Test last visited tracking
            (term-mux--show-buffer buffer-1 session)
            (should (eq (gethash session term-mux--last-visited-table) buffer-1))

            (term-mux--show-buffer buffer-2 session)
            (should (eq (gethash session term-mux--last-visited-table) buffer-2)))
        (kill-buffer buffer-1)
        (kill-buffer buffer-2)))))

(ert-deftest term-mux-test-detect-terminal ()
  "Test terminal backend auto-detection priority."
  ;; Ghostel takes priority
  (cl-letf (((symbol-function 'featurep) (lambda (feature) (eq feature 'ghostel)))
            ((symbol-function 'ghostel-mode) (lambda (&rest _) (setq major-mode 'ghostel-mode)))
            (term-mux-mode nil))
    (with-temp-buffer
      (term-mux--detect-terminal)
      (should (eq major-mode 'ghostel-mode))))
  ;; Falls back to vterm when ghostel missing
  (cl-letf (((symbol-function 'featurep) (lambda (feature) (eq feature 'vterm)))
            ((symbol-function 'vterm-mode) (lambda (&rest _) (setq major-mode 'vterm-mode)))
            (term-mux-mode nil))
    (with-temp-buffer
      (term-mux--detect-terminal)
      (should (eq major-mode 'vterm-mode))))
  ;; Falls back to eshell when nothing else available
  (cl-letf (((symbol-function 'featurep) (lambda (_) nil))
            ((symbol-function 'eshell-mode) (lambda (&rest _) (setq major-mode 'eshell-mode)))
            (term-mux-mode nil))
    (with-temp-buffer
      (term-mux--detect-terminal)
      (should (eq major-mode 'eshell-mode)))))

(ert-deftest term-mux-test-setup-ghostel ()
  "Test ghostel setup function."
  ;; Error when ghostel not available
  (cl-letf (((symbol-function 'featurep) (lambda (_) nil)))
    (with-temp-buffer
      (should-error (term-mux--setup-ghostel))))
  ;; Activates ghostel-mode when available
  (cl-letf (((symbol-function 'featurep) (lambda (_) t))
            ((symbol-function 'ghostel-mode) (lambda (&rest _) (setq major-mode 'ghostel-mode)))
            (term-mux-mode nil))
    (with-temp-buffer
      (term-mux--setup-ghostel)
      (should (eq major-mode 'ghostel-mode)))))

(ert-deftest term-mux-test-setup-vterm ()
  "Test vterm setup function."
  ;; Error when vterm not available
  (cl-letf (((symbol-function 'featurep) (lambda (_) nil)))
    (with-temp-buffer
      (should-error (term-mux--setup-vterm))))
  ;; Activates vterm-mode when available
  (cl-letf (((symbol-function 'featurep) (lambda (_) t))
            ((symbol-function 'vterm-mode) (lambda (&rest _) (setq major-mode 'vterm-mode)))
            (term-mux-mode nil))
    (with-temp-buffer
      (term-mux--setup-vterm)
      (should (eq major-mode 'vterm-mode)))))

(provide 'term-mux-test)
;;; term-mux-test.el ends here