(setq debug-on-error t)
(setq fancy-splash-image "")
(require 'cl)

(setq srcdir (or (getenv "URLSRCDIR") "."))

(push srcdir load-path)
(push (or (getenv "GNUSDIR") (expand-file-name "../../gnus/lisp" srcdir)) load-path)

(setq max-specpdl-size (* 10 max-specpdl-size)
      max-lisp-eval-depth (* 10 max-lisp-eval-depth))

;; If we are building url in a different directory than the source
;; directory, we must read *.el from source directory and write *.elc
;; into the building directory.  For that, we define this function
;; before loading bytecomp.  Bytecomp doesn't overwrite this function.
(defun byte-compile-dest-file (filename)
  "Convert an Emacs Lisp source file name to a compiled file name.
 In addition, remove directory name part from FILENAME."
  (setq filename (byte-compiler-base-file-name filename))
  (setq filename (file-name-sans-versions filename))
  (setq filename (file-name-nondirectory filename))
  (if (memq system-type '(win32 w32 mswindows windows-nt))
      (setq filename (downcase filename)))
  (cond ((eq system-type 'vax-vms)
 	 (concat (substring filename 0 (string-match ";" filename)) "c"))
 	((string-match emacs-lisp-file-regexp filename)
 	 (concat (substring filename 0 (match-beginning 0)) ".elc"))
 	(t (concat filename ".elc"))))

(require 'bytecomp)

;; Emacs 19 byte compiler complains about too much stuff by default.
;; Turn off most of the warnings here.
(setq byte-compile-warnings nil
      byte-optimize t)

(defun compile-it ()
  (let ((files (directory-files "." t ".*.[eE][lL]$" nil)))
    (while files
      (if (and (not (file-directory-p (car files)))
	       (not (string-match "w3-sysdp.el$" (car files))))
	  (byte-compile-file (car files)))
      (setq files (cdr files)))))

(defun emacs-build-autoloads (dir autofile)
  (require 'autoload)
  (let ((files (directory-files dir t ".*.[eE][lL]$" nil)))
    (save-excursion
      (find-file autofile)
      (erase-buffer)
      (mapcar 'generate-file-autoloads files)
      (goto-char (point-max))
      (setq buffer-read-only nil)
      (insert "\n(provide 'url-autoloads)\n")
      (save-buffer)
      (kill-buffer (current-buffer))))

  ;; Now we need to munge that file to deal with
  (find-file "url-auto.el")
  (erase-buffer)
  (insert-file-contents autofile)
  (goto-char (point-min))
  (while (re-search-forward "url-autoloads" nil t)
    (replace-match "url-auto"))
  (save-buffer)
  (kill-buffer (current-buffer))
  (kill-emacs))

(defun emacs-batch-build-autoloads ()
  (emacs-build-autoloads (nth 0 command-line-args-left)
			 (nth 1 command-line-args-left)))

(defun emacs-build-custom-load (dir)
  (let ((foundit t))
    (save-excursion
      (condition-case ()
	  (load-library "cus-dep")
	(error (setq foundit nil)))
      (if foundit
	  (let ((command-line-args-left (list dir)))
	    (custom-make-dependencies))
	(write-region "\n" nil "cus-load.el")))))

(defun emacs-batch-build-custom-load ()
  (emacs-build-custom-load (car command-line-args-left)))

(provide 'url-auto)
