;;; twittering-compatibility.el --- Compatibility functions for Emacs 21

;; Copyright (C) 2009-2015 Tadashi MATSUO
;;               2007, 2009-2011 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO
;;               2014, 2015 Xavier Maillard

;; Author: Tadashi MATSUO <tad@mymail.twin.ne.jp>
;;	Y. Hayamizu <y.hayamizu@gmail.com>
;;	Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;	Alberto Garcia <agarcia@igalia.com>
;;	Xavier Maillard <xavier@maillard.im>
;; Created: Sep 4, 2007
;; Version: HEAD
;; Identity: $Id: a2fc6eb695ad0994e986ab0413e53f335d9a947b $
;; Keywords: twitter web
;; URL: http://twmode.sf.net/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Compatibility functions for Emacs 21

;;; Code:

(eval-and-compile
  ;; On byte-compilation, Emacs21 requires loading the libraries
  ;; distributed with twittering-mode.el for macros defined in them.
  (when (> 22 emacs-major-version)
    (setq load-path
          (append (mapcar (lambda (dir)
                            (expand-file-name
                             dir
                             (if load-file-name
                                 (or (file-name-directory load-file-name)
                                     ".")
                               ".")))
                          '("url-emacs21" "emacs21"))
                  load-path))))

(when (> 22 emacs-major-version)
  (and (require 'un-define nil t)
       ;; the explicitly require 'unicode to update a workaround with
       ;; navi2ch. see a comment of `twittering-ucs-to-char' for more
       ;; details.
       (require 'unicode nil t))
  (defadvice url-scheme-register-proxy (around twittering-fix-process-env (scheme) activate)
    (let ((process-environment
           (apply 'append
                  (let ((env-var (concat scheme "_proxy")))
                    (mapcar
                     (lambda (str)
                       (if (string-match
                            (concat "^\\("
                                    (regexp-opt (list (upcase env-var)
                                                      (downcase env-var)))
                                    "\\)=$")
                            str)
                           nil
                         (list str)))
                     process-environment)))))
      ad-do-it)))

(defun twittering-remove-duplicates (list)
  "Return a copy of LIST with all duplicate elements removed.
This is non-destructive version of `delete-dups' which is not
defined in Emacs21."
  (if (fboundp 'delete-dups)
      (delete-dups (copy-sequence list))
    (let ((rest list)
          (result nil))
      (while rest
        (unless (member (car rest) result)
          (setq result (cons (car rest) result)))
        (setq rest (cdr rest)))
      (nreverse result))))

(defun twittering-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a string in the minibuffer, with completion.
This is a modified version of `completing-read' and accepts candidates
as a list of a string on Emacs21."
  ;; completing-read() of Emacs21 does not accepts candidates as
  ;; a list. Candidates must be given as an alist.
  (let* ((collection (twittering-remove-duplicates collection))
         (collection
          (if (and (> 22 emacs-major-version)
                   (listp collection)
                   (stringp (car collection)))
              (mapcar (lambda (x) (cons x nil)) collection)
            collection)))
    (completing-read prompt collection predicate require-match
                     initial-input hist def inherit-input-method)))

(defun twittering-add-to-history (history-var elt &optional maxelt keep-all)
  (if (functionp 'add-to-history)
      (add-to-history history-var elt maxelt keep-all)
    (let* ((added (cons elt
                        (if (and (not keep-all)
                                 (boundp 'history-delete-duplicates)
                                 history-delete-duplicates)
                            (delete elt (symbol-value history-var))
                          (symbol-value history-var))))
           (maxelt (or maxelt history-length))
           (len (length added)))
      (set history-var
           (if (<= len maxelt)
               added
             (butlast added (- len maxelt)))))))

(if (fboundp 'assoc-string)
    (defalias 'twittering-assoc-string 'assoc-string)
  (defun twittering-assoc-string (key list &optional case-fold)
    "Like `assoc' but specifically for strings (and symbols).
This returns the first element of LIST whose car matches the string or
symbol KEY, or nil if no match exists.  When performing the
comparison, symbols are first converted to strings, and unibyte
strings to multibyte.  If the optional arg CASE-FOLD is non-nil, case
is ignored.

Unlike `assoc', KEY can also match an entry in LIST consisting of a
single string, rather than a cons cell whose car is a string.

This is reimplemented version of `assoc-string' which is not
defined in Emacs21."
    (let* ((key (if (stringp key)
                    key
                  (symbol-name key)))
           (regexp (concat "\\`" key "\\'"))
           (rest list)
           (result nil)
           (case-fold-search case-fold))
      (while (not (null rest))
        (let* ((current (car rest))
               (current-key
                (if (listp current)
                    (car current)
                  current))
               (current-key
                (if (stringp current-key)
                    current-key
                  (symbol-name current-key))))
          (if (string-match key current-key)
              (setq result current
                    rest nil)
            (setq rest (cdr rest)))))
      result)))

(defun twittering-ucs-to-char-internal (code-point)
  ;; Check (featurep 'unicode) is a workaround with navi2ch to avoid
  ;; error "error in process sentinel: Cannot open load file:
  ;; unicode".
  ;;
  ;; Details: navi2ch prior to 1.8.3 (which is currently last release
  ;; version as of 2010-01-18) always define `ucs-to-char' as autoload
  ;; file "unicode(.el)" (which came from Mule-UCS), hence it breaks
  ;; `ucs-to-char' under non Mule-UCS environment. The problem is
  ;; fixed in navi2ch dated 2010-01-16 or later, but not released yet.
  (if (and (featurep 'unicode) (functionp 'ucs-to-char))
      (ucs-to-char code-point)
    ;; Emacs21 have a partial support for UTF-8 text, so it can decode
    ;; only parts of a text with Japanese.
    (decode-char 'ucs code-point)))

(provide 'twittering-compatibility)
;;; twittering-compatibility.el ends here
