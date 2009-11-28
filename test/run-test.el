
(defvar twittering-test-dir nil)

(when load-file-name
  (setq twittering-test-dir (file-name-directory load-file-name))
  (add-to-list 'load-path twittering-test-dir))

(require 'twittering-mode (expand-file-name "../twittering-mode.el"
					    twittering-test-dir))
(require 'test (expand-file-name "../test.el"
				 twittering-test-dir))

(defun twittering-run-test ()
  (interactive)

  (dolist (file-name (directory-files twittering-test-dir))
    (when (string-match "^test-" file-name)
      (let ((file-name (expand-file-name file-name twittering-test-dir)))
	(load file-name))))

  (test-run-all-cases)
  
  (princ (save-excursion (set-buffer "*test-result*")
			 (buffer-string)))
  (terpri))

(twittering-run-test)
