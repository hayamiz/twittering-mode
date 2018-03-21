;;; test.el --- Unit test framework for Emacs lisp program
;; Copyright (C) 2008 by Wang Liang

;; Author: Wang Liang <netcasper@gmail.com>

;; test.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; test.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Download
;;    Latest version is at http://www.wanglianghome.org/svn/test/test.el
;;    You can check out with subversion.
;;      $ svn co http://www.wanglianghome.org/svn/test/

;;; Screenshot
;;    http://www.wanglianghome.org/images/test.png

;;; Usage

;; Overview
;;   This unit test framework is developed for testing operations on a buffer.
;;   But it definitely can be used to test operations unrelated to buffers.
;;   
;;   For example, I write `my-buffer-substring'.  I also have a file and if it
;;   is loaded into a buffer and run (my-buffer-substring 10 22) on that buffer,
;;   it must return "hello, world".  To test it, I can write a test case with
;;   this framework,
;;   
;;     (defun my-buffer-substring-setup ()
;;       (insert-file-contents "my-file"))
;;   
;;     (defcase my-buffer-substring-test nil 'my-buffer-substring-setup
;;       (test-assert-string-equal (my-buffer-substring 10 22) "hello, world"))
;;   
;;   And then run it by invoking 
;;   `M-x test-run-one-case MY-BUFFER-SUBSTRING-TEST'.
;;
;;   http://www.wanglianghome.org/svn/test/example.el is an example of usage.

;; To use this framework, add the following lines to your .emacs file
;;     (add-to-list 'load-path "/path/to/test/")
;;     (require 'test)

;; Write test cases
;;   Test case is written with `defcase' macro.  For example,
;;
;;     (defcase my-code-test nil nil
;;       (test-assert-ok (my-fun)))
;;
;;   This test case includes one assertion but no tags and no setup code.
;;   It checks return value of `my-fun'.  If it's `nil', case fails.
;;   Otherwise, case passes.  You can add more assertions into one case.
;;
;;   Besides `test-assert-ok', `test-assert-key' is used to assert that a key
;;   is bound to a function in a mode-map.  For example,
;;     (test-assert-key example-mode-map (kbd "C-c C-c") 'example-function)
;;
;;   Assertion for binary comparison or prediction is much more flexible.
;;   You can use `test-assert-CMP' if `CMP', either provided by Emacs or
;;   written by you, accepts more than two parameters.  All those assertions
;;   fallback to `test-assert-binary-relation' function so that I do not need
;;   to write them as many as possible.  Since `test-assert-binary-relation'
;;   only considers first two parameters, others are ignored.
;;
;;   You can develop your own assertions by using "test-assert-extended-" as
;;   function name prefix. For example You develop `test-assert-extended->'
;;   to compare many numbers since `test-assert->' only compares two.  Inside
;;   your own assertion function, you should use `assert' from `cl' package to
;;   do real job.
;;
;;   Test cases can be grouped with tags so that you can run them with one 
;;   command.  To add tags to the previous test case,
;;
;;     (defcase my-code-test (my-lib-suite my-lib-sublib-suite) nil
;;       (test-assert-ok (my-fun)))
;;
;;   All test cases are run in a temporary buffer.  You can setup buffer 
;;   content by providing `setup' code.  For example,
;;
;;     (defun my-lib-setup ()
;;       (insert-file-contents "my-input-filename")
;;       (my-mode))
;;
;;     (defcase my-code-test (my-lib my-lib-sublib) 'my-lib-setup
;;       (test-assert-ok (my-fun)))
;;
;;   You probably want to add a common tag to all your test case for a specific
;;   package, and add a common setup code too.  So you can write your own macro
;;   to make it easy to develop test cases.  For example,
;;
;;     (defmacro defmylibcase (case-name tags &rest body)
;;       `(defcase ,case-name ,(append '(my-lib-suite) tags) 'my-lib-setup
;;                 ,@body))
;;
;;   And then,
;;
;;     (defmylibcase my-code-test-2 (my-lib-sublib-suite)
;;       (test-assert-ok (my-fun)))
;;

;; Run test cases
;;   `M-x test-run-one-case CASE' runs one test case.
;;   `M-x test-run-all-cases' runs all test cases stored in `test-cases'.
;;   `M-x test-run-one-tag TAG' runs all test cases grouped by TAG.
;;   `(test-run-tags TAGS)' runs all test cases grouped by one of TAGS.
;;
;;   Test result, including detail error message and summary, is shown in
;;   buffer `*test-result*'.
;;   Every test case has a summary line to show how many cases pass and how
;;   many cases fail.  There are also a summary line to show total number of
;;   pass and failure for all commands except `test-run-one-case'.
;;   Error message is helpful.  If assertion fails, `test-assert-ok' prints
;;   evaluated form, `test-assert-binary-relation' prints what is got and why
;;   it failed.

;;; Code

(require 'cl)

(defconst test-version "0.9"
  "test version")

(defun test-version ()
  "Show test version."
  (interactive)
  (message "test version %s" test-version))

(defvar test-cases (make-hash-table)
  "All case in all tags")

(defvar test-tags (make-hash-table)
  "All tags from all test cases")

(defvar test-special-assertion-functions '(test-assert-ok test-assert-key)
  "Assertion functions must not fallback to `test-assert-binary-relation'.")

(defun test-completing-read (prompt choices dummy require-match)
  "Use iswitchb completion functionality."
  (let ((iswitchb-make-buflist-hook
	 (lambda ()
	   (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt nil require-match)))

(defvar test-completing-read-function
  (if (fboundp 'iswitchb-read-buffer) 'test-completing-read 'completing-read))

(defvar test-assert-method-prefix "test-assert-"
  "Prefix of test-assert methods.")

(defvar test-assert-extended-prefix "test-assert-extended-"
  "Prefix of user-extended test-assert methods.")

(defun test-assert-p (test)
  "Return non-nil if TEST is an assertion."
  (let ((method-name (symbol-name (car test))))
    (string-equal test-assert-method-prefix
		  (substring method-name
			     0
			     (min (length test-assert-method-prefix)
				  (length method-name))))))

(defun test-special-assert-p (test)
  "Return non-nil if TEST is `test-assert-ok' or user-extended assertion."
  (or (memq (car test) test-special-assertion-functions)
      (let ((method-name (symbol-name (car test))))
	(string-equal test-assert-extended-prefix
		      (substring method-name
				 0
				 (min (length test-assert-extended-prefix)
				      (length method-name)))))))

(defun test-report-error (test error)
  "Print form TEST and error message from ERROR."
  (princ "#  ")
  (prin1 test)
  (princ "\n")
  (let* ((msg (error-message-string error))
	 (from-here (string-equal
		     "#    " (substring msg 0 5))))
    (unless from-here
      (princ "#    "))
    (princ msg)
    (unless from-here)
    (princ "\n"))
  (princ "#  \n"))

(defun test-gensym (&optional prefix)
  "Generate uninterned symbol.
If PREFIX is non-nil, use it as prefix.  Otherwise, use \"--test--\"."
  (gensym (or prefix "--test--")))

(defun test-transform-body (body fail succ err &optional not-toplevel)
  "This function transforms the BODY, which is a body of defcase,
recursively to alternate `test-assert-*' to apropriate
expressions"
  (if (not (listp body))
      body
    (mapcar
     (lambda (arg)
       (cond ((not (listp arg))
	      arg)
	     ((not (test-assert-p arg))
	      (let ((arg (cond
			  ((listp arg)
			   (test-transform-body arg fail succ err t))
			  (t arg))))
		(if not-toplevel
		    arg
		  `(condition-case ,err
		       ;; do not count as success
		       ,arg
		     (error (incf ,fail) ; but count as failure
			    (test-report-error ',arg ,err))))))
	     ((test-special-assert-p arg)
	      `(condition-case ,err
		   (progn
		     ,arg
		     (incf ,succ))
		 (error (incf ,fail)
			(test-report-error ',arg ,err))))
	     (t
	      `(condition-case ,err
		   (progn
		     (test-assert-binary-relation
			 ;; function to test binary relation
			 ',(intern
			    (substring
			     (symbol-name (car arg))
			     (length test-assert-method-prefix)))
		       ;; parameters to above function
		       ,@(cdr arg))
		     (incf ,succ))
		 (error (incf ,fail)
			(test-report-error ',arg ,err))))))
     body)))

(defmacro defcase (case-name tags setup &rest body)
  "Define test case which includes one or multiple assertions."
  (let ((tag (test-gensym))
	(tag-cases (test-gensym))
	(fail (test-gensym))
	(succ (test-gensym))
	(err (test-gensym)))
    `(progn
       (assert (and (listp ',tags)
		     (or (null ',tags)
			 (every 'symbolp ',tags)))
	       ;; `show-args' can not be `t' since `assert' will
	       ;; evaluate EVERY parameter of `and'.  This breaks
	       ;; short-circuit effect of `and'.
	       nil
		"Tags must be nil or a list of symbols.")
       ;; Associate case with every tag, and push tags into hash table.
       (dolist (,tag ',tags)
	 (when ,tag
	   (let ((,tag-cases (gethash ,tag test-tags '())))
	     (add-to-list ',tag-cases ',case-name)
	     (puthash ,tag ,tag-cases test-tags))))
       ;; Push function, which runs test case and returns `(pass-count
       ;; fail-count)', into hash table.
       (puthash ',case-name
		(lambda ()
		  (let ((,fail 0)
			(,succ 0))
		    (with-temp-buffer
		      (when ,setup
			(funcall ,setup))		
		      ;; transform `body' of macro during expansion time.
		      ,@(test-transform-body body fail succ err)
		      ;; summarize
		      (princ (format "%s: %d pass, %d fail."
				     (symbol-name ',case-name)
				     ,succ ,fail))
		      (princ "\n"))
		    ;; return value
		    (list ,succ ,fail)))
		test-cases))))

(defun test-princ-current-time ()
  "Print start time to run test cases."
  (princ "#  ")
  (princ (current-time-string))
  (princ "\n"))

(defmacro test-report (&rest body)
  "Show test report in buffer `*test-result*'."
  `(progn
     (with-output-to-temp-buffer "*test-result*"
       (test-princ-current-time)
       ,@body)
     (with-current-buffer "*test-result*"
       (test-result-mode))))

(defun test-run (cases)
  "Run test cases in CASES."
  (dolist (case-name (test-args-to-list cases))
    (funcall (gethash case-name test-cases))))

(defvar test-last-summary nil)
(defun test-run-and-summarize (cases)
  "Run test cases in CASES and print summary."
  (let ((total-succ 0)
	(total-fail 0))
    (dolist (case-name (test-args-to-list cases))
      (let ((summary (funcall (gethash case-name test-cases))))
	(incf total-succ (car summary))
	(incf total-fail (cadr summary))))
    (setq test-last-summary
	  `((succ . ,total-succ) (fail . ,total-fail)))
    (princ "#  ")
    (princ (format "Total: %d pass, %d fail." total-succ total-fail))))

(defun test-args-to-list (args)
  "Make sure ARGS is a list."
  (if (listp args)
      args
    (list args)))

(defun test-find-all-cases (tags)
  "Return all test cases grouped by TAGS.
This function guarantees that no duplicated cases in return value."
  (let ((tag-list (test-args-to-list tags))
	(cases '()))
    (dolist (tag tag-list)
      (dolist (test-case (gethash tag test-tags))
	(add-to-list 'cases test-case)))
    cases))

(defun test-maphashkey (fn hashtable)
  "Call FN on each key of hash table HASHTABLE and return a list of result."
  (let ((keys '()))
    (maphash (lambda (key value)
	       (add-to-list 'keys (funcall fn key)))
	     hashtable)
    keys))

(defun test-find-all-case-names (&optional tag)
  "Return all case names, or name of cases grouped by TAG if TAG is non-nil, as a list."
  (if tag
      (mapcar 'symbol-name (test-find-all-cases tag))
    (test-maphashkey 'symbol-name test-cases)))

(defun test-find-all-tag-names ()
  "Return all tag names as a list."
  (test-maphashkey 'symbol-name test-tags))

(defun test-completing-read-one (prompt choice)
  "Read a string and return a symbol whose name is the string being read."
  ;; Better to read a tag symbol directly.
  (intern (funcall test-completing-read-function prompt choice nil t)))

(defun test-completing-read-one-tag ()
  "Read a tag name and return corresponding tag symbol."
  (test-completing-read-one "Tag: " (test-find-all-tag-names)))

(defun test-completing-read-one-case (&optional tag)
  "Read a case name and return corresponding case symbol. If TAG is non-nil, only cases grouped by it is allowed."
  (test-completing-read-one "Case name: " (test-find-all-case-names tag)))

(defun test-run-one-tag (tag-name)
  "Run test cases grouped by tag TAG-NAME."
  (interactive (list (test-completing-read-one-tag)))
  (test-report (princ "#  Tag: ") (princ tag-name) (princ "\n")
	       (test-run-and-summarize (test-find-all-cases tag-name))))

(defun test-run-tags (&rest tags)
  "Run all test cases grouped by TAGS."
  (test-report (princ "#  Tags: ")
	       (princ (mapconcat 'symbol-name tags " "))
	       (princ "\n")
	       (test-run-and-summarize (test-find-all-cases tags))))

(defun test-run-one-case (case-name)
  "Run one test case whose name is CASE-NAME."
  (interactive (list (test-completing-read-one-case)))
  (test-report (test-run case-name)))

(defun test-run-all-cases ()
  "Run all test cases saved in TEST-CASES."
  (interactive)
  (test-report (test-run-and-summarize (test-maphashkey 'identity test-cases))))

(defmacro test-motion-target (&rest body)
  "Return position after motion."
  `(progn
     ,@body
    (point)))

;;; Special assertions
(defun test-assert-ok (form)
  "Assert that FORM returns non-nil."
  (assert form nil
	  (with-output-to-string
	    (princ "#    not ok: ")
	    (prin1 form))))

(defun test-assert-key (mode-map kbd function)
  "Assert that KBD is binding to FUNCTION in MODE-MAP."
  (let ((got (lookup-key mode-map kbd)))
    (assert (eq got function)
	    t
	    (with-output-to-string
	      (princ "#    got: ")
	      (prin1 got)
	      (princ "\n")
	      (princ "#    not bound to: ")
	      (prin1 function)))))

;;; Assertion for binary comparison or prediction.
(defun test-assert-binary-relation (fn got expected)
  "Fallback function to assert all binary relation between GOT and EXPECTED with FN."
  (assert (funcall fn got expected)
	  t
	  (with-output-to-string
	    (princ "#    got: ")
	    (prin1 got)
	    (princ "\n")
	    (princ "#    not ")
	    (prin1 fn)
	    (princ ": ")
	    (prin1 expected))))

;;; `test-result-mode'

(defvar test-result-font-lock-keywords
  `(("^\\(#    got: \\)\\(.*\\)$"
     (1 font-lock-preprocessor-face) (2 font-lock-warning-face))
    ("^\\(#    not ok: \\)\\(.*\\)$"
     (1 font-lock-preprocessor-face) (2 font-lock-warning-face))
    ("^\\(#    .*?: \\)\\(.*\\)$"
     (1 font-lock-preprocessor-face) (2 font-lock-type-face))
    ;; be careful about the order
    ("^#  .*$" . font-lock-preprocessor-face)
    ("^\\(.*\\): \\([0-9]+\\) pass, \\([0-9]+\\) fail.$"
     (1 font-lock-function-name-face) (2 font-lock-type-face) (3 font-lock-warning-face)))
  "Font lock for `test-result-mode'.")

(defconst test-result-font-lock-defaults
  '(test-result-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))

(define-derived-mode test-result-mode nil "Test-Result"
  (set (make-local-variable 'font-lock-defaults) test-result-font-lock-defaults))

(provide 'test)
