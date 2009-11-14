
(defvar twittering-test-dir nil)

(when load-file-name
  (setq twittering-test-dir (file-name-directory load-file-name))
  (add-to-list 'load-path twittering-test-dir))

(require 'elunit)

(defun twittering-run-test ()
  (interactive)
  (let ((test-dir twittering-test-dir))
    (elunit-clear)

    (dolist (file-name (directory-files test-dir))
      (when (string-match "^test-" file-name)
	(let ((file-name (expand-file-name file-name test-dir)))
	  (load file-name))))
    
    (elunit "twittering-mode")))

