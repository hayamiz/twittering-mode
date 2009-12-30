
(defvar twittering-test-dir nil)

(when load-file-name
  (setq twittering-test-dir (file-name-directory load-file-name))
  (add-to-list 'load-path twittering-test-dir))

(require 'twittering-mode (expand-file-name "../twittering-mode.el"
					    twittering-test-dir))
(require 'test (expand-file-name "../test.el"
				 twittering-test-dir))

(defun get-fixture (name)
  (when (symbolp name)
    (setq name (symbol-name name)))
  (let ((fixture-path (expand-file-name
		       (concat name ".el")
		       (expand-file-name "fixture" twittering-test-dir))))
    (if (file-exists-p fixture-path)
	(with-temp-buffer
	  (insert-file-contents fixture-path)
	  (beginning-of-buffer)
	  (read (current-buffer)))
      nil)))

(defmacro with-network (&rest body)
  `(when (getenv "NETWORK")
     ,@body))

(defun twittering-run-test ()
  (interactive)

  (dolist (file-name (directory-files twittering-test-dir))
    (when (string-match "^test-" file-name)
      (let ((file-name (expand-file-name file-name twittering-test-dir)))
	(load file-name))))

  (test-run-all-cases)
  
  (let* ((fail-count (cdr (assoc 'fail test-last-summary)))
	 (exit-status (if (eq 0 fail-count) 0 1)))
    (save-excursion
      (set-buffer "*test-result*")
      (end-of-buffer)
      (search-backward-regexp "" nil t)
      (beginning-of-buffer)
      (while (search-forward-regexp "\\b\\([0-9]+\\) pass\\b" nil t)
	(replace-match
	 (concat "\033[1m\033[32m" (match-string 0) "\033[0m")))

      (beginning-of-buffer)
      (while (search-forward-regexp "\\b\\([0-9]+\\) fail\\b" nil t)
	(let ((pass (string-equal "0" (match-string 1))))
	  (replace-match
	   (concat "\033[1m\033[31m" (match-string 0) "\033[0m"))
	  (beginning-of-line)
	  (when (search-forward-regexp "^\\([^:#]+\\):" nil t)
	    (replace-match
	     (concat (if pass
			 "\033[1m\033[32m"
		       "\033[1m\033[31m")
		     (match-string 1)
		     "\033[0m")
	     nil nil nil 1))
	  ))
      (princ (buffer-string)))
    (terpri)
    (when noninteractive
      (kill-emacs exit-status))
    ))

(twittering-run-test)
