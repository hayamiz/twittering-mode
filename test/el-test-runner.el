;;; el-test-runner --- test runner for twittering-mode

;;; Commentary:

;;; Code:
(require 'test)
(require 'twittering-mode)

(setq twittering-test-dir (file-name-directory
                             (or load-file-name buffer-file-name)))

(defun get-fixture (name)
  "Read the fixture file specified by NAME."
  (when (symbolp name)
    (setq name (symbol-name name)))
  (let ((fixture-path (expand-file-name
                       (concat name ".el")
                       (expand-file-name "fixture" twittering-test-dir))))
    (when (file-exists-p fixture-path)
      (with-temp-buffer
        (insert-file-contents fixture-path)
        (goto-char (point-min))
        (read (current-buffer))))))

(defmacro with-network (&rest body)
  "Evaluate BODY when the NETWORK env variable is set."
  `(when (getenv "NETWORK")
     ,@body))

(defun twittering-run-test ()
  "Run the test suite."
  (interactive)
  (dolist (file-name (directory-files twittering-test-dir))
    (when (string-match "^test-" file-name)
      (let ((file-name (expand-file-name file-name twittering-test-dir)))
	(load file-name))))

  (test-run-all-cases)

  (with-current-buffer "*test-result*"
    (let* ((buf (buffer-string))
           (failures (car (twittering-extract-matched-substring-all
                           "Total: [0-9]+ pass, \\([0-9]+\\) fail"
                           (substring-no-properties buf nil))))
           (exit-status (if (string= "0" failures)
                            0
                          1)))
      (when noninteractive
        (message "%s" buf)
        (kill-emacs exit-status)))))

(twittering-run-test)

(provide 'el-test-runner)
;;; el-test-runner.el ends here
