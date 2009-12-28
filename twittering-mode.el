;;; -*- indent-tabs-mode: t; tab-width: 8 -*-
;;;
;;; twittering-mode.el --- Major mode for Twitter

;; Copyright (C) 2007 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO

;; Author: Y. Hayamizu <y.hayamizu@gmail.com>
;;         Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;         Alberto Garcia  <agarcia@igalia.com>
;; Created: Sep 4, 2007
;; Version: 0.9
;; Keywords: twitter web
;; URL: http://github.com/hayamiz/twittering-mode/

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

;; twittering-mode.el is a major mode for Twitter.
;; You can check friends timeline, and update your status on Emacs.

;;; Feature Request:

;; URL : http://twitter.com/d00dle/statuses/577876082
;; * Status Input from Popup buffer and C-cC-c to POST.
;; URL : http://code.nanigac.com/source/view/419
;; * update status for region

;;; Code:

(eval-when-compile (require 'cl))
(require 'xml)
(require 'parse-time)
(when (< emacs-major-version 22)
  (require 'un-define)
  (set-terminal-coding-system 'utf-8))

(defconst twittering-mode-version "0.9")
(defconst twittering-max-number-of-tweets-on-retrieval 200
  "The maximum number of `twittering-number-of-tweets-on-retrieval'.")

(defvar twittering-tinyurl-service 'tinyurl
  "The service to use. One of 'tinyurl' or 'toly'")

(defvar twittering-tinyurl-services-map
  '((tinyurl . "http://tinyurl.com/api-create.php?url=")
    (toly    . "http://to.ly/api.php?longurl="))
  "Alist of tinyfy services")

(defun twittering-mode-version ()
  "Display a message for twittering-mode version."
  (interactive)
  (let ((version-string
	 (format "twittering-mode-v%s" twittering-mode-version)))
    (if (interactive-p)
	(message "%s" version-string)
      version-string)))

(defvar twittering-mode-map (make-sparse-keymap))

(defvar twittering-timer nil "Timer object for timeline refreshing will be
stored here. DO NOT SET VALUE MANUALLY.")

(defvar twittering-tweet-history nil)
(defvar twittering-user-history nil)
(defvar twittering-hashtag-history nil)

(defvar twittering-number-of-tweets-on-retrieval 20
  "*The number of tweets which will be retrieved in one request.
The upper limit is `twittering-max-number-of-tweets-on-retrieval'.")

(defvar twittering-current-hashtag nil
  "A hash tag string currently set. You can set it by calling
  `twittering-set-current-hashtag'")

(defvar twittering-timer-interval 90
  "The interval of auto reloading. You should use 60 or more
  seconds for this variable because the number of API call is
  limited by the hour.")

(defvar twittering-username nil
  "An username of your Twitter account.")
(defvar twittering-username-active nil
  "Copy of `twittering-username' for internal use.")

(defvar twittering-password nil
  "A password of your Twitter account. Leave it blank is the
  recommended way because writing a password in .emacs file is so
  dangerous.")
(defvar twittering-password-active nil
  "Copy of `twittering-password' for internal use.")

(defvar twittering-last-timeline-retrieved nil)
(defvar twittering-list-index-retrieved nil)

(defvar twittering-new-tweets-count 0
  "Number of new tweets when `twittering-new-tweets-hook' is run")

(defvar twittering-new-tweets-hook nil
  "Hook run when new twits are received.

You can read `twittering-new-tweets-count' to get the number of new
tweets received when this hook is run.")

(defvar twittering-scroll-mode nil)
(make-variable-buffer-local 'twittering-scroll-mode)

(defvar twittering-jojo-mode nil)
(make-variable-buffer-local 'twittering-jojo-mode)

(defvar twittering-status-format "%i %s,  %@:\n  %t // from %f%L%r"
  "Format string for rendering statuses.
Ex. \"%i %s,  %@:\\n  %t // from %f%L%r\"

Items:
 %s - screen_name
 %S - name
 %i - profile_image
 %d - description
 %l - location
 %L - \" [location]\"
 %r - \" in reply to user\"
 %u - url
 %j - user.id
 %p - protected?
 %c - created_at (raw UTC string)
 %C{time-format-str} - created_at (formatted with time-format-str)
 %@ - X seconds ago
 %t - text
 %' - truncated
 %f - source
 %# - id
")

(defvar twittering-retweet-format "RT: %t (via @%s)"
  "Format string for retweet.

Items:
 %s - screen_name
 %t - text
 %% - %
")

(defvar twittering-notify-successful-http-get t)

(defvar twittering-use-ssl t
  "Use SSL connection if this variable is non-nil.

SSL connections use 'curl' command as a backend.")

(defvar twittering-buffer "*twittering*")
(defun twittering-buffer ()
  (twittering-get-or-generate-buffer twittering-buffer))

(defvar twittering-timeline-data nil)
(defvar twittering-timeline-last-update nil)

(defvar twittering-username-face 'twittering-username-face)
(defvar twittering-uri-face 'twittering-uri-face)

(defun twittering-get-or-generate-buffer (buffer)
  (if (bufferp buffer)
      (if (buffer-live-p buffer)
	  buffer
	(generate-new-buffer (buffer-name buffer)))
    (if (stringp buffer)
	(or (get-buffer buffer)
	    (generate-new-buffer buffer)))))

(defun twittering-last-host ()
  (twittering-convert-last-timeline-retrieved)
  (car twittering-last-timeline-retrieved))

(defun twittering-last-method ()
  (twittering-convert-last-timeline-retrieved)
  (nth 1 twittering-last-timeline-retrieved))

(defun twittering-convert-last-timeline-retrieved ()
  "Adjust variable to keep a backward compatibility."
  (and (stringp twittering-last-timeline-retrieved)
       (setq twittering-last-timeline-retrieved
	     `("twitter.com" ,twittering-last-timeline-retrieved))))

(defun assocref (item alist)
  (cdr (assoc item alist)))
(defmacro list-push (value listvar)
  `(setq ,listvar (cons ,value ,listvar)))

;;; Proxy
(defvar twittering-proxy-use nil)
(defvar twittering-proxy-keep-alive nil)
(defvar twittering-proxy-server nil)
(defvar twittering-proxy-port 8080)
(defvar twittering-proxy-user nil)
(defvar twittering-proxy-password nil)

(defun twittering-toggle-proxy () ""
  (interactive)
  (setq twittering-proxy-use
	(not twittering-proxy-use))
  (twittering-update-mode-line)
  (message "%s %s"
	   "Use Proxy:"
	   (if twittering-proxy-use
	       "on" "off")))

(defun twittering-user-agent-default-function ()
  "Twittering mode default User-Agent function."
  (concat "Emacs/"
	  (int-to-string emacs-major-version) "." (int-to-string
						   emacs-minor-version)
	  " "
	  "Twittering-mode/"
	  twittering-mode-version))

(defvar twittering-sign-simple-string nil)

(defun twittering-sign-string-default-function ()
  "Tweet append sign string:simple "
  (if twittering-sign-simple-string
      (concat " [" twittering-sign-simple-string "]")
    ""))

(defvar twittering-user-agent-function 'twittering-user-agent-default-function)
(defvar twittering-sign-string-function 'twittering-sign-string-default-function)

(defun twittering-user-agent ()
  "Return User-Agent header string."
  (funcall twittering-user-agent-function))

(defun twittering-sign-string ()
  "Return Tweet sign string."
  (funcall twittering-sign-string-function))

;;; to show image files

(defvar twittering-wget-buffer "*twittering-wget-buffer*")
(defun twittering-wget-buffer ()
  (twittering-get-or-generate-buffer twittering-wget-buffer))

(defvar twittering-tmp-dir
  (expand-file-name (concat "twmode-images-" (user-login-name))
		    temporary-file-directory))

(defvar twittering-icon-mode nil "You MUST NOT CHANGE this variable
directly. You should change through function'twittering-icon-mode'")

(make-variable-buffer-local 'twittering-icon-mode)
(defun twittering-icon-mode (&optional arg)
  "Toggle display of icon images on timelines.
With a numeric argument, if the argument is positive, turn on
icon mode; otherwise, turn off icon mode."
  (interactive)
  (setq twittering-icon-mode
	(if twittering-icon-mode
	    (if (null arg)
		nil
	      (> (prefix-numeric-value arg) 0))
	  (when (or (null arg)
		    (and arg (> (prefix-numeric-value arg) 0)))
	    (when (file-writable-p twittering-tmp-dir)
	      (progn
		(if (not (file-directory-p twittering-tmp-dir))
		    (make-directory twittering-tmp-dir))
		t)))))
  (twittering-update-mode-line)
  (twittering-render-timeline))

(defun twittering-scroll-mode (&optional arg)
  (interactive)
  (setq twittering-scroll-mode
	(if (null arg)
	    (not twittering-scroll-mode)
	  (> (prefix-numeric-value arg) 0)))
  (twittering-update-mode-line))

(defun twittering-jojo-mode (&optional arg)
  (interactive)
  (setq twittering-jojo-mode
	(if (null arg)
	    (not twittering-jojo-mode)
	  (> (prefix-numeric-value arg) 0)))
  (twittering-update-mode-line))

(defvar twittering-image-stack nil)
(defvar twittering-image-type-cache nil)
(defvar twittering-convert-program (executable-find "convert"))
(defvar twittering-convert-fix-size nil)
(defvar twittering-use-convert (not (null (executable-find "convert")))
  "*This variable makes a sense only if `twittering-convert-fix-size'
is non-nil. If this variable is non-nil, icon images are converted by
invoking \"convert\". Otherwise, cropped images are displayed.")
(defvar twittering-use-wget nil
  "*If non-nil, icon images are retrieved by invoking \"wget\".
Otherwise, they are retrieved by `url-retrieve'.")

(defun twittering-image-type (file-name)
  (if (and (not (assoc file-name twittering-image-type-cache))
	   (file-exists-p file-name))
      (if (and twittering-convert-fix-size twittering-use-convert)
	  (let ((tmpfile (make-temp-file "emacstwit" nil ".png")))
	    (let ((coding-system-for-read 'raw-text)
		  (coding-system-for-write 'binary))
	      (call-process twittering-convert-program nil nil nil
			    file-name "-resize"
			    (format "%dx%d" twittering-convert-fix-size
				    twittering-convert-fix-size)
			    tmpfile)
	      (rename-file tmpfile file-name t))
	    (add-to-list 'twittering-image-type-cache `(,file-name . png)))
      (let* ((file-output (shell-command-to-string (concat "file -b " file-name)))
      	     (file-type (cond
      			 ((string-match "JPEG" file-output) 'jpeg)
      			 ((string-match "PNG" file-output) 'png)
      			 ((string-match "GIF" file-output) 'gif)
      			 ((and twittering-use-convert
			       (string-match "bitmap" file-output))
      			  (let ((coding-system-for-read 'raw-text)
      				(coding-system-for-write 'binary))
      			    (with-temp-buffer
      			      (set-buffer-multibyte nil)
      			      (insert-file-contents file-name)
      			      (call-process-region
      			       (point-min) (point-max)
      			       twittering-convert-program
      			       t (current-buffer) nil
      			       "bmp:-" "png:-")
      			      (write-region (point-min) (point-max)
      					    file-name)))
      			  'png)
      			 ((string-match "\\.jpe?g" file-name) 'jpeg)
      			 ((string-match "\\.png" file-name) 'png)
      			 ((string-match "\\.gif" file-name) 'gif)
      			 (t nil))))
      	(add-to-list 'twittering-image-type-cache `(,file-name . ,file-type)))))
  (cdr (assoc file-name twittering-image-type-cache)))

(defun twittering-setftime (fmt string uni)
  (format-time-string fmt ; like "%Y-%m-%d %H:%M:%S"
		      (apply 'encode-time (parse-time-string string))
		      uni))
(defun twittering-local-strftime (fmt string)
  (twittering-setftime fmt string nil))
(defun twittering-global-strftime (fmt string)
  (twittering-setftime fmt string t))


(defvar twittering-debug-mode nil)
(defvar twittering-debug-buffer "*debug*")
(defun twittering-debug-buffer ()
  (twittering-get-or-generate-buffer twittering-debug-buffer))
(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if twittering-debug-mode
	   (with-current-buffer (twittering-debug-buffer)
	     (insert "[debug] ")
	     (insert (prin1-to-string ,obsym))
	     (newline)
	     ,obsym)
	 ,obsym))))
(defun debug-printf (fmt &rest args)
  (when twittering-debug-mode
    (with-current-buffer (twittering-debug-buffer)
      (insert (concat "[debug] " (apply 'format fmt args)))
      (newline))))

(defun twittering-debug-mode ()
  (interactive)
  (setq twittering-debug-mode
	(not twittering-debug-mode))
  (message (if twittering-debug-mode "debug mode:on" "debug mode:off")))

(if twittering-mode-map
    (let ((km twittering-mode-map))
      (define-key km "\C-c\C-f" 'twittering-friends-timeline)
      (define-key km "\C-c\C-r" 'twittering-replies-timeline)
      (define-key km "\C-c\C-g" 'twittering-public-timeline)
      (define-key km "\C-c\C-u" 'twittering-user-timeline)
      (define-key km "\C-c\C-s" 'twittering-update-status-interactive)
      (define-key km "\C-c\C-e" 'twittering-erase-old-statuses)
      (define-key km "\C-c\C-m" 'twittering-retweet)
      (define-key km "\C-c\C-h" 'twittering-set-current-hashtag)
      (define-key km "\C-m" 'twittering-enter)
      (define-key km "\C-c\C-l" 'twittering-update-lambda)
      (define-key km [mouse-1] 'twittering-click)
      (define-key km "\C-c\C-v" 'twittering-view-user-page)
      (define-key km "g" 'twittering-current-timeline)
      (define-key km "d" 'twittering-direct-message)
      (define-key km "v" 'twittering-other-user-timeline)
      (define-key km "V" 'twittering-other-user-timeline-interactive)
      (define-key km "L" 'twittering-other-user-list-interactive)
      ;; (define-key km "j" 'next-line)
      ;; (define-key km "k" 'previous-line)
      (define-key km "j" 'twittering-goto-next-status)
      (define-key km "k" 'twittering-goto-previous-status)
      (define-key km "l" 'forward-char)
      (define-key km "h" 'backward-char)
      (define-key km "0" 'beginning-of-line)
      (define-key km "^" 'beginning-of-line-text)
      (define-key km "$" 'end-of-line)
      (define-key km "n" 'twittering-goto-next-status-of-user)
      (define-key km "p" 'twittering-goto-previous-status-of-user)
      (define-key km "\C-i" 'twittering-goto-next-thing)
      (define-key km "\M-\C-i" 'twittering-goto-previous-thing)
      (define-key km [backtab] 'twittering-goto-previous-thing)
      (define-key km [backspace] 'backward-char)
      (define-key km "G" 'end-of-buffer)
      (define-key km "H" 'beginning-of-buffer)
      (define-key km "i" 'twittering-icon-mode)
      (define-key km "s" 'twittering-scroll-mode)
      (define-key km "t" 'twittering-toggle-proxy)
      (define-key km "\C-c\C-p" 'twittering-toggle-proxy)
      (define-key km "q" 'twittering-suspend)
      nil))

(defun twittering-keybind-message ()
  (let ((important-commands
	 '(("Timeline" . twittering-friends-timeline)
	   ("Replies" . twittering-replies-timeline)
	   ("Update status" . twittering-update-status-interactive)
	   ("Next" . twittering-goto-next-status)
	   ("Prev" . twittering-goto-previous-status))))
    (mapconcat (lambda (command-spec)
		 (let ((descr (car command-spec))
		       (command (cdr command-spec)))
		   (format "%s: %s" descr (key-description
					   (where-is-internal
					    command
					    overriding-local-map t)))))
	       important-commands ", ")))

;; (run-with-idle-timer
;;  0.1 t
;;  '(lambda ()
;;     (when (equal (buffer-name (current-buffer)) twittering-buffer)
;;       (message (twittering-keybind-message)))))

(defvar twittering-mode-syntax-table nil "")

(if twittering-mode-syntax-table
    ()
  (setq twittering-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" twittering-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  twittering-mode-syntax-table)
  )

(defun twittering-mode-init-variables ()
  ;; (make-variable-buffer-local 'variable)
  ;; (setq variable nil)
  (font-lock-mode -1)
  (defface twittering-username-face
    `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-string-face 'twittering-username-face)
  (set-face-attribute 'twittering-username-face nil :underline t)
  (defface twittering-uri-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'twittering-uri-face nil :underline t)
;;   (add-to-list 'minor-mode-alist '(twittering-icon-mode " tw-icon"))
;;   (add-to-list 'minor-mode-alist '(twittering-scroll-mode " tw-scroll"))
;;   (add-to-list 'minor-mode-alist '(twittering-jojo-mode " tw-jojo"))
  (setq twittering-username-active twittering-username)
  (setq twittering-password-active twittering-password)
  )

(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key))
				 keylist))
		't)
	     ,@body)))
       clauses)))

;; If you use Emacs21, decode-char 'ucs will fail unless Mule-UCS is loaded.
;; TODO: Show error messages if Emacs 21 without Mule-UCS
(defun twittering-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      (ucs-to-char num)
    (decode-char 'ucs num)))

(defvar twittering-mode-string "twittering-mode")

(defvar twittering-mode-hook nil
  "Twittering-mode hook.")

(defun twittering-mode ()
  "Major mode for Twitter
\\{twittering-mode-map}"
  (interactive)
  (switch-to-buffer (twittering-buffer))
  (kill-all-local-variables)
  (twittering-mode-init-variables)
  (use-local-map twittering-mode-map)
  (setq major-mode 'twittering-mode)
  (twittering-update-mode-line)
  (set-syntax-table twittering-mode-syntax-table)
  (run-hooks 'twittering-mode-hook)
  (font-lock-mode -1)
  (twittering-stop)
  (twittering-start))

(defun twittering-update-mode-line ()
  "Update mode line"
  (let (enabled-options)
    (when twittering-jojo-mode
      (push "jojo" enabled-options))
    (when twittering-icon-mode
      (push "icon" enabled-options))
    (when twittering-scroll-mode
      (push "scroll" enabled-options))
    (when twittering-proxy-use
      (push "proxy" enabled-options))
    (when twittering-use-ssl
      (push "ssl" enabled-options))
    (setq mode-name
	  (concat twittering-mode-string
		  (if enabled-options
		      (concat "["
			      (mapconcat 'identity enabled-options ",")
			      "]")
		    ""))))
  (force-mode-line-update)
  )

;;;
;;; Basic HTTP functions
;;;

(defun twittering-find-curl-program ()
  "Returns an appropriate 'curl' program pathname or nil if not found."
  (or (executable-find "curl")
      (let ((windows-p (find system-type '(windows-nt cygwin)))
	    (curl.exe
	     (expand-file-name
	      "curl.exe"
	      (expand-file-name
	       "win-curl"
	       (file-name-directory (symbol-file 'twit))))))
	     (and windows-p
		  (file-exists-p curl.exe) curl.exe))))

(defun twittering-start-http-session
  (method headers host port path parameters
	       &optional noninteractive sentinel)
  "
METHOD    : http method
HEADERS   : http request heades in assoc list
HOST      : remote host name
PORT      : destination port number. nil means default port(http: 80, https: 443)
PATH      : http request path
PARAMETERS: http request parameters (query string)
"
  (block nil
    (unless (find method '("POST" "GET") :test 'equal)
      (error (format "Unknown HTTP method: %s" method)))
    (unless (string-match "^/" path)
      (error (format "Invalid HTTP path: %s" path)))
    
    (unless (assoc "Host" headers)
      (setq headers (cons `("Host" . ,host) headers)))
    (unless (assoc "User-Agent" headers)
      (setq headers (cons `("User-Agent" . ,(twittering-user-agent))
			  headers)))

    (let ((curl-program (twittering-find-curl-program)))
      (if (and (not curl-program) twittering-use-ssl)
	  (if (yes-or-no-p "HTTPS(SSL) is not available. Use HTTP instead?")
	      (progn (setq twittering-use-ssl nil)
		     (twittering-update-mode-line))
	    (message "Request canceled")
	    (return)))
      
      (if twittering-use-ssl
	  (twittering-start-http-ssl-session
	   curl-program method headers host port path parameters
	   noninteractive sentinel)
	(twittering-start-http-non-ssl-session
	 method headers host port path parameters
	 noninteractive sentinel)))))

;;; FIXME: file name is hard-coded. More robust way is desired.
(defun twittering-ensure-ca-cert ()
  "Create a CA certificate file if it does not exist, and return
its file name."
  (let ((file-name (expand-file-name
		    (format "twmode-twitter-cacert-%s.pem"
			    (user-login-name))
		    temporary-file-directory)))
    (with-temp-file file-name
      (insert "-----BEGIN CERTIFICATE-----
MIICkDCCAfmgAwIBAgIBATANBgkqhkiG9w0BAQQFADBaMQswCQYDVQQGEwJVUzEc
MBoGA1UEChMTRXF1aWZheCBTZWN1cmUgSW5jLjEtMCsGA1UEAxMkRXF1aWZheCBT
ZWN1cmUgR2xvYmFsIGVCdXNpbmVzcyBDQS0xMB4XDTk5MDYyMTA0MDAwMFoXDTIw
MDYyMTA0MDAwMFowWjELMAkGA1UEBhMCVVMxHDAaBgNVBAoTE0VxdWlmYXggU2Vj
dXJlIEluYy4xLTArBgNVBAMTJEVxdWlmYXggU2VjdXJlIEdsb2JhbCBlQnVzaW5l
c3MgQ0EtMTCBnzANBgkqhkiG9w0BAQEFAAOBjQAwgYkCgYEAuucXkAJlsTRVPEnC
UdXfp9E3j9HngXNBUmCbnaEXJnitx7HoJpQytd4zjTov2/KaelpzmKNc6fuKcxtc
58O/gGzNqfTWK8D3+ZmqY6KxRwIP1ORROhI8bIpaVIRw28HFkM9yRcuoWcDNM50/
o5brhTMhHD4ePmBudpxnhcXIw2ECAwEAAaNmMGQwEQYJYIZIAYb4QgEBBAQDAgAH
MA8GA1UdEwEB/wQFMAMBAf8wHwYDVR0jBBgwFoAUvqigdHJQa0S3ySPY+6j/s1dr
aGwwHQYDVR0OBBYEFL6ooHRyUGtEt8kj2Puo/7NXa2hsMA0GCSqGSIb3DQEBBAUA
A4GBADDiAVGqx+pf2rnQZQ8w1j7aDRRJbpGTJxQx78T3LUX47Me/okENI7SS+RkA
Z70Br83gcfxaz2TE4JaY0KNA4gGK7ycH8WUBikQtBmV1UsCGECAhX2xrD2yuCRyv
8qIYNMR1pHMc8Y3c7635s3a0kr/clRAevsvIO1qEYBlWlKlV
-----END CERTIFICATE-----"))
    file-name))

(defun twittering-start-http-ssl-session
  (curl-program method headers host port path parameters
		&optional noninteractive sentinel)
  ;; TODO: use curl
  (let* ((request (twittering-make-http-request
		   method headers host port path parameters))
	 (headers (if (assoc "Expect" headers)
		      headers
		    (cons '("Expect" . "") headers)))
	 (curl-args
	  `("--include" "--silent"
	    ,@(mapcan (lambda (pair)
			(list "-H"
			      (format "%s: %s"
				      (car pair) (cdr pair))))
		      headers)
	    "--cacert"
	    ,(twittering-ensure-ca-cert))))
    (when twittering-proxy-use
      (nconc curl-args `("-x" ,(format "%s:%s" twittering-proxy-server
					 twittering-proxy-port)))
      (when (and twittering-proxy-user
		 twittering-proxy-password)
	(nconc curl-args `("-U" ,(format "%s:%s" twittering-proxy-user
					   twittering-proxy-password)))))
    
    (flet ((request (key) (funcall request key)))
      (nconc curl-args `(,(if parameters
			      (concat (request :uri) "?"
				      (request :query-string))
			    (request :uri))))
      (when (string-equal "POST" method)
	(nconc curl-args 
	       `(,@(mapcan (lambda (pair)
			     (list
			      "-d"
			      (format "%s=%s"
				      (twittering-percent-encode
				       (car pair))
				      (twittering-percent-encode
				       (cdr pair)))))
			   parameters)))))
    (debug-print curl-args)
    (lexical-let ((temp-buffer
		   (generate-new-buffer "*twmode-http-buffer*"))
		  (sentinel sentinel))
      (let ((curl-process
	     (apply 'start-process
		    "*twmode-curl*"
		    temp-buffer
		    curl-program
		    curl-args)))
	(set-process-sentinel
	 curl-process
	 (lambda (&rest args)
	   (apply sentinel temp-buffer noninteractive args))))))
  )

;; TODO: proxy
(defun twittering-start-http-non-ssl-session
  (method headers host port path parameters
	       &optional noninteractive sentinel)
  (let ((request (twittering-make-http-request
		  method headers host port path parameters)))
    (flet ((request (key) (funcall request key)))
      (let* ((request-str
	      (format "%s %s%s HTTP/1.1\r\n%s\r\n\r\n"
		      (request :method)
		      (request :uri)
		      (if parameters (concat "?" (request :query-string))
			"")
		      (request :headers-string)))
	     (server (if twittering-proxy-use
			 twittering-proxy-server
		       (request :host)))
	     (port (if twittering-proxy-use
		       twittering-proxy-port
		     (request :port)))
	     (temp-buffer (generate-new-buffer "*twmode-http-buffer*"))
	     (proc (open-network-stream
		    "network-connection-process" temp-buffer server port))
	     )
	(lexical-let ((temp-buffer temp-buffer)
		      (sentinel sentinel)
		      (noninteractive noninteractive))
	  (set-process-sentinel
	   proc
	   (lambda (&rest args)
	     (apply sentinel temp-buffer noninteractive args))))
	(debug-print request-str)
	(process-send-string proc request-str))))
  )

;;; TODO: proxy
(defun twittering-make-http-request
  (method headers host port path parameters)
  "Returns an anonymous function, which holds request data.

A returned function, say REQUEST, is used in this way:
  (funcall REQUEST :schema) ; => \"http\" or \"https\"
  (funcall REQUEST :uri) ; => \"http://twitter.com/user_timeline\"
  (funcall REQUEST :query-string) ; => \"status=hello+twitter&source=twmode\"
  ...

Available keywords:
  :method
  :host
  :port
  :headers
  :headers-string
  :schema
  :uri
  :query-string
  "
  (let* ((schema (if twittering-use-ssl "https" "http"))
	 (default-port (if twittering-use-ssl 443 80))
	 (port (if port port default-port))
	 (headers-string
	  (mapconcat (lambda (pair)
		       (format "%s: %s" (car pair) (cdr pair)))
		     headers "\r\n"))
	 (uri (format "%s://%s%s%s"
		      schema
		      host
		      (if port
			  (if (equal port default-port)
			      ""
			    (format ":%s" port))
			"")
		      path))
	 (query-string
	  (mapconcat (lambda (pair)
		       (format
			"%s=%s"
			(twittering-percent-encode (car pair))
			(twittering-percent-encode (cdr pair))))
		     parameters
		     "&"))
	 )
    (lexical-let ((data `((:method . ,method)
			  (:host . ,host)
			  (:port . ,port)
			  (:headers . ,headers)
			  (:headers-string . ,headers-string)
			  (:schema . ,schema)
			  (:uri . ,uri)
			  (:query-string . ,query-string)
			  )))
      (lambda (key)
	(let ((pair (assoc key data)))
	  (if pair (cdr pair)
	    (error (format "No such key in HTTP request data: %s" key)))))
      )))

(defun twittering-http-application-headers
  (&optional method headers)
  "Retuns an assoc list of HTTP headers for twittering-mode.
"
  (unless method (setq method "GET"))

  (let ((headers headers))
    (push (cons "User-Agent" (twittering-user-agent)) headers)
    (push (cons "Authorization"
		(concat "Basic "
			(base64-encode-string
			 (concat
			  (twittering-get-username)
			  ":"
			  (twittering-get-password)))))
	  headers)
    (when (string-equal "GET" method)
      (push (cons "Accept"
		  (concat
		   "text/xml"
		   ",application/xml"
		   ",application/xhtml+xml"
		   ",application/html;q=0.9"
		   ",text/plain;q=0.8"
		   ",image/png,*/*;q=0.5"))
	    headers)
      (push (cons "Accept-Charset" "utf-8;q=0.7,*;q=0.7")
	    headers))
    (when (string-equal "POST" method)
      (push (cons "Content-Length" "0") headers)
      (push (cons "Content-Type" "text/plain") headers))
    (when twittering-proxy-use
      (when twittering-proxy-keep-alive
	(push (cons "Proxy-Connection" "Keep-Alive")
	      headers))
      (when (and twittering-proxy-user
		 twittering-proxy-password)
	(push (cons "Proxy-Authorization"
		    (concat
		     "Basic "
		     (base64-encode-string
		      (concat
		       twittering-proxy-user
		       ":"
		       twittering-proxy-password))))
	      headers)))
    headers
    ))

(defun twittering-http-get
  (host method &optional noninteractive parameters sentinel)
  (if (null sentinel) (setq sentinel 'twittering-http-get-default-sentinel))

  (twittering-start-http-session
   "GET" (twittering-http-application-headers "GET")
   host nil (concat "/" method ".xml") parameters noninteractive sentinel))

(defun twittering-created-at-to-seconds (created-at)
  (let ((encoded-time (apply 'encode-time (parse-time-string created-at))))
    (+ (* (car encoded-time) 65536)
       (cadr encoded-time))))

;; XXX: this is a preliminary implementation because we should parse
;; xmltree in the function.
(defun twittering-http-get-list-index-sentinel
  (temp-buffer noninteractive proc stat &optional suc-msg)
  (unwind-protect
      (let ((header (twittering-get-response-header temp-buffer)))
	(if (not (string-match "HTTP/1\.[01] \\([a-z0-9 ]+\\)\r?\n" header))
	    (setq twittering-list-index-retrieved "Failure: Bad http response.")
	  (let ((status (match-string-no-properties 1 header))
		(indexes nil))
	    (if (not (string-match "\r?\nLast-Modified: " header))
		(setq twittering-list-index-retrieved
		      (concat status ", but no contents."))
	      (case-string
	       status
	       (("200 OK")
		(save-excursion
		  (set-buffer temp-buffer)
		  (goto-char (point-min))
		  (if (search-forward-regexp "\r?\n\r?\n" nil t)
		      (while (re-search-forward
			      "<slug>\\([-a-zA-Z0-9_]+\\)</slug>" nil t)
			(push (match-string 1) indexes)))
		  (if indexes
		      (setq twittering-list-index-retrieved indexes)
		    (setq twittering-list-index-retrieved ""))))
	       (t
		(setq twittering-list-index-retrieved status)))))))
    (when (and (not twittering-debug-mode) (buffer-live-p temp-buffer))
      (kill-buffer temp-buffer)))
  )

(defun twittering-http-get-default-sentinel (temp-buffer noninteractive proc stat &optional suc-msg)
  (unwind-protect
      (debug-printf "get-default-sentinel: proc=%s stat=%s" proc stat)
      (let ((header (twittering-get-response-header temp-buffer))
	    (body (twittering-get-response-body temp-buffer))
	    (status nil)
	    )
	(if (string-match "HTTP/1\.[01] \\([a-zA-Z0-9 ]+\\)\r?\n" header)
	    (progn
	      (setq status (match-string-no-properties 1 header))
	      (case-string
	       status
	       (("200 OK")
		(setq twittering-new-tweets-count
		      (count t (mapcar
				#'twittering-cache-status-datum
				(reverse (twittering-xmltree-to-status
					  body)))))
		(setq twittering-timeline-data
		      (sort twittering-timeline-data
			    (lambda (status1 status2)
			      (let ((created-at1
				     (twittering-created-at-to-seconds
				      (cdr (assoc 'created-at status1))))
				    (created-at2
				     (twittering-created-at-to-seconds
				      (cdr (assoc 'created-at status2)))))
				(> created-at1 created-at2)))))
		(if (and (> twittering-new-tweets-count 0)
			 noninteractive)
		    (run-hooks 'twittering-new-tweets-hook))
		(twittering-render-timeline)
		(when twittering-notify-successful-http-get
		  (message (if suc-msg suc-msg "Success: Get."))))
	       (t (message status))))
	  (message "Failure: Bad http response.")))
    (when (and (not twittering-debug-mode) (buffer-live-p temp-buffer))
      (kill-buffer temp-buffer)))
  )

(defun twittering-render-timeline ()
  (with-current-buffer (twittering-buffer)
    (let ((point (point))
	  (end (point-max)))
      (setq buffer-read-only nil)
      (erase-buffer)
      (mapc (lambda (status)
	      (insert (twittering-format-status
		       status twittering-status-format))
	      (fill-region-as-paragraph
	       (line-beginning-position) (point))
	      (insert "\n"))
	    twittering-timeline-data)
      (if (and twittering-image-stack window-system)
	  (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if twittering-scroll-mode (- (point-max) end) 0))))
    ))

(defun twittering-icon-path (icon-url)
  (require 'ffap)
  (concat (md5 icon-url nil nil 'iso-2022-7bit)
	  (or (ffap-file-suffix icon-url) ".img")))

(defun twittering-make-display-spec-for-icon (fullpath)
  "Return the specification for `display' text property, which limits
the size of an icon image FULLPATH up to FIXED-LENGTH.

If the size of the image exceeds FIXED-LENGTH, the center of the
image are displayed."
  (let* ((image-spec
	  `(image :type ,(twittering-image-type fullpath)
		  :file ,fullpath)))
    (if (and twittering-convert-fix-size (not twittering-use-convert))
	(let* ((size (if (file-exists-p fullpath)
			 (image-size image-spec t)
		       '(48 . 48)))
	       (width (car size))
	       (height (cdr size))
	       (fixed-length twittering-convert-fix-size)
	       (half-fixed-length (/ fixed-length 2))
	       (slice-spec
		(if (or (< fixed-length width) (< fixed-length height))
		    `(slice ,(max 0 (- (/ width 2) half-fixed-length))
			    ,(max 0 (- (/ height 2) half-fixed-length))
			    ,fixed-length ,fixed-length)
		  `(slice 0 0 ,fixed-length ,fixed-length))))
	  `(display (,image-spec ,slice-spec)))
      `(display ,image-spec))))

(defun twittering-format-status (status format-str)
  (flet ((attr (key)
	       (assocref key status))
	 (profile-image
	  ()
	  (let ((profile-image-url (attr 'user-profile-image-url))
		(icon-string "\n  "))
	    (if (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" profile-image-url)
		(let* ((filename (match-string-no-properties 1
							     profile-image-url))
		       (fullpath (concat twittering-tmp-dir "/" (twittering-icon-path profile-image-url))))
		  ;; download icons if does not exist
		  (if (file-exists-p fullpath)
		      t
		    (add-to-list 'twittering-image-stack profile-image-url))

		  (when (and icon-string twittering-icon-mode)
		    (let ((display-spec
			   (twittering-make-display-spec-for-icon fullpath)))
		      (set-text-properties 1 2 display-spec icon-string))
		    icon-string)
		  ))))
	 (make-string-with-url-property
	  (str url)
	  (let ((result (copy-sequence str)))
	    (add-text-properties
	     0 (length result)
	     `(mouse-face highlight face twittering-uri-face uri ,url)
	     result)
	    result)))
    (let* ((replace-table
	    `(("%" . "%")
	      ("#" . ,(attr 'id))
	      ("'" . ,(if (string= "true" (attr 'truncated)) "..." ""))
	      ("@" .
	       ,(let* ((created-at
			(apply
			 'encode-time
			 (parse-time-string (attr 'created-at))))
		       (now (current-time))
		       (secs (+ (* (- (car now) (car created-at)) 65536)
				(- (cadr now) (cadr created-at))))
		       (time-string
			(cond
			 ((< secs 5) "less than 5 seconds ago")
			 ((< secs 10) "less than 10 seconds ago")
			 ((< secs 20) "less than 20 seconds ago")
			 ((< secs 30) "half a minute ago")
			 ((< secs 60) "less than a minute ago")
			 ((< secs 150) "1 minute ago")
			 ((< secs 2400) (format "%d minutes ago"
						(/ (+ secs 30) 60)))
			 ((< secs 5400) "about 1 hour ago")
			 ((< secs 84600) (format "about %d hours ago"
						 (/ (+ secs 1800) 3600)))
			 (t (format-time-string "%I:%M %p %B %d, %Y"
						created-at))))
		       (url
			(twittering-get-status-url (attr 'user-screen-name)
						   (attr 'id))))
		  ;; make status url clickable
		  (make-string-with-url-property time-string url)))
	      ("C\\({\\([^}]*\\)}\\)?" .
	       (lambda (context)
		 (let ((str (cdr (assq 'following-string context)))
		       (match-data (cdr (assq 'match-data context))))
		   (let* ((time-format
			   (or (match-string 2 str) "%H:%M:%S"))
			  (created-at
			   (apply 'encode-time
				  (parse-time-string (attr 'created-at)))))
		     (format-time-string time-format created-at)))))
	      ("c" . ,(attr 'created-at))
	      ("d" . ,(attr 'user-description))
	      ("f" . ,(attr 'source))
	      ("i" . (lambda (context) (profile-image)))
	      ("j" . ,(attr 'user-id))
	      ("L" . ,(let ((location (or (attr 'user-location) "")))
			(if (not (string= "" location))
			    (concat " [" location "]")
			  "")))
	      ("l" . ,(attr 'user-location))
	      ("p" . ,(if (string= "true" (attr 'user-protected))
			  "[x]"
			""))
	      ("r" .
	       ,(let ((reply-id (or (attr 'in-reply-to-status-id) ""))
		      (reply-name (or (attr 'in-reply-to-screen-name) "")))
		  (if (or (string= "" reply-id) (string= "" reply-name))
		      ""
		    (let ((in-reply-to-string
			   (concat "in reply to " reply-name))
			  (url
			   (twittering-get-status-url reply-name reply-id)))
		      (concat " "
			      (make-string-with-url-property
			       in-reply-to-string url))))))
	      ("S" . ,(attr 'user-name))
	      ("s" . ,(attr 'user-screen-name))
	      ("t" . ,(attr 'text))
	      ("u" . ,(attr 'user-url))
	      ))
	   (formatted-status
	    (twittering-format-string format-str "%" replace-table)))
      (add-text-properties
       0 (length formatted-status)
       `(username ,(attr 'user-screen-name) id ,(attr 'id) text ,(attr 'text))
       formatted-status)
      formatted-status)))

(defun twittering-http-post
  (host method &optional parameters contents sentinel)
  "Send HTTP POST request to twitter.com (or api.twitter.com)

HOST is hostname of remote side, twitter.com or api.twitter.com.
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (if (null sentinel) (setq sentinel 'twittering-http-post-default-sentinel))

  (twittering-start-http-session
   "POST" (twittering-http-application-headers "POST")
   host nil (concat "/" method ".xml") parameters noninteractive sentinel))

(defun twittering-http-post-default-sentinel (temp-buffer noninteractive proc stat &optional suc-msg)

  (unwind-protect
      (condition-case err-signal
	  (let ((header (twittering-get-response-header temp-buffer))
		;; (body (twittering-get-response-body)) not used now.
		(status nil))
	    (if (string-match "HTTP/1\.1 \\([a-z0-9 ]+\\)\r?\n" header)
		(setq status (match-string-no-properties 1 header))
	      (setq status
		    (progn (string-match "^\\([^\r\n]+\\)\r?\n" header)
			   (match-string-no-properties 1 header))))
	    (case-string status
			 (("200 OK")
			  (message (if suc-msg suc-msg "Success: Post")))
			 (t (message "Response status code: %s" status)))
	    )
	(error (message (prin1-to-string err-signal))))
    (when (and (not twittering-debug-mode) (buffer-live-p temp-buffer))
      (kill-buffer temp-buffer)))
  )

(defun twittering-get-response-header (buffer)
  "Exract HTTP response header from HTTP response.
`buffer' may be a buffer or the name of an existing buffer which contains the HTTP response."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  
  ;; FIXME:
  ;; curl prints HTTP proxy response header, so strip it
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (when (search-forward-regexp
	   "HTTP/1\\.[01] 200 Connection established\r\n\r\n" nil t)
      (delete-region (point-min) (point)))
    (let* ((start (point))
	   (end (if (search-forward-regexp "\r?\n\r?\n")
		    (match-end 0)
		  current)))
      (buffer-substring start end))))

(defun twittering-get-response-body (buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a
XML tree as list. `buffer' may be a buffer or the name of an existing buffer. "
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((start (if (search-forward-regexp "\r?\n\r?\n" nil t)
		     (match-end 0)
		   (point))))
      (xml-parse-region start (point-max)))
      ))

(defun twittering-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default twittering-timeline-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'twittering-timeline-data))
  (let ((id (cdr (assq 'id status-datum))))
    (if (or (null (symbol-value data-var))
	    (not (find-if
		  (lambda (item)
		    (string= id (cdr (assq 'id item))))
		  (symbol-value data-var))))
	(progn
	  (if twittering-jojo-mode
	      (twittering-update-jojo (cdr (assq 'user-screen-name
						 status-datum))
				      (cdr (assq 'text status-datum))))
	  (set data-var (cons status-datum (symbol-value data-var)))
	  t)
      nil)))

(defun twittering-status-to-status-datum (status)
  (flet ((assq-get (item seq)
		   (car (cddr (assq item seq)))))
    (let* ((status-data (cddr status))
	   id text source created-at truncated
	   in-reply-to-status-id
	   in-reply-to-screen-name
	   (user-data (cddr (assq 'user status-data)))
	   user-id user-name
	   user-screen-name
	   user-location
	   user-description
	   user-profile-image-url
	   user-url
	   user-protected
	   regex-index)

      (setq id (assq-get 'id status-data))
      (setq text (twittering-decode-html-entities
		  (assq-get 'text status-data)))
      (setq source (twittering-decode-html-entities
		    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq in-reply-to-status-id
	    (twittering-decode-html-entities
	     (assq-get 'in_reply_to_status_id status-data)))
      (setq in-reply-to-screen-name
	    (twittering-decode-html-entities
	     (assq-get 'in_reply_to_screen_name status-data)))
      (setq user-id (assq-get 'id user-data))
      (setq user-name (twittering-decode-html-entities
		       (assq-get 'name user-data)))
      (setq user-screen-name (twittering-decode-html-entities
			      (assq-get 'screen_name user-data)))
      (setq user-location (twittering-decode-html-entities
			   (assq-get 'location user-data)))
      (setq user-description (twittering-decode-html-entities
			      (assq-get 'description user-data)))
      (setq user-profile-image-url (assq-get 'profile_image_url user-data))
      (setq user-url (assq-get 'url user-data))
      (setq user-protected (assq-get 'protected user-data))

      ;; make username clickable
      (add-text-properties
       0 (length user-name)
       `(mouse-face highlight
		    uri ,(concat "http://twitter.com/" user-screen-name)
		    face twittering-username-face)
       user-name)

      ;; make screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
		    uri ,(concat "http://twitter.com/" user-screen-name)
		    face twittering-username-face)
       user-screen-name)

      ;; make screen-name in text clickable
      (let ((pos 0))
	(block nil
	  (while (string-match "@\\([_a-zA-Z0-9]+\\)" text pos)
	    (let ((next-pos (match-end 0))
		  (screen-name (match-string 1 text)))
	      (when (eq next-pos pos)
		(return nil))
	      
	      (add-text-properties
	       (match-beginning 1) (match-end 1)
	       `(screen-name-in-text ,screen-name) text)
	      
	      (setq pos next-pos)))))

      ;; make URI clickable
      (setq regex-index 0)
      (while regex-index
	(setq regex-index
	      (string-match "@\\([_a-zA-Z0-9]+\\)\\|\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)"
			    text
			    regex-index))
	(when regex-index
	  (let* ((matched-string (match-string-no-properties 0 text))
		 (screen-name (match-string-no-properties 1 text))
		 (uri (match-string-no-properties 2 text)))
	    (add-text-properties
	     (if screen-name
		 (+ 1 (match-beginning 0))
	       (match-beginning 0))
	     (match-end 0)
	     (if screen-name
		 `(mouse-face
		   highlight
		   face twittering-uri-face
		   uri-in-text ,(concat "http://twitter.com/" screen-name))
	       `(mouse-face highlight
			    face twittering-uri-face
			    uri-in-text ,uri))
	     text))
	  (setq regex-index (match-end 0)) ))


      ;; make source pretty and clickable
      (if (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>" source)
	  (let ((uri (match-string-no-properties 1 source))
		(caption (match-string-no-properties 2 source)))
	    (setq source caption)
	    (add-text-properties
	     0 (length source)
	     `(mouse-face highlight
			  uri ,uri
			  face twittering-uri-face
			  source ,source)
	     source)
	    ))

      ;; save last update time
      (when (or (null twittering-timeline-last-update)
                (< (twittering-created-at-to-seconds
                    twittering-timeline-last-update)
                   (twittering-created-at-to-seconds created-at)))
        (setq twittering-timeline-last-update created-at))

      (mapcar
       (lambda (sym)
	 `(,sym . ,(symbol-value sym)))
       '(id text source created-at truncated
	    in-reply-to-status-id
	    in-reply-to-screen-name
	    user-id user-name user-screen-name user-location
	    user-description
	    user-profile-image-url
	    user-url
	    user-protected)))))

(defun twittering-xmltree-to-status (xmltree)
  (mapcar #'twittering-status-to-status-datum
	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
	  ;; On Emacs22, there may be blank strings
	  (let ((ret nil) (statuses (reverse (cddr (car xmltree)))))
	    (while statuses
	      (if (consp (car statuses))
		  (setq ret (cons (car statuses) ret)))
	      (setq statuses (cdr statuses)))
	    ret)))

(defun twittering-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
	  (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
      ((twittering-url-reserved-p c)
       (char-to-string c))
      ((eq c ? ) "+")
      (t (format "%%%x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun twittering-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun twittering-decode-html-entities (encoded-str)
  (if encoded-str
      (let ((cursor 0)
	    (found-at nil)
	    (result '()))
	(while (setq found-at
		     (string-match "&\\(#\\([0-9]+\\)\\|\\([A-Za-z]+\\)\\);"
				   encoded-str cursor))
	  (when (> found-at cursor)
	    (list-push (substring encoded-str cursor found-at) result))
	  (let ((number-entity (match-string-no-properties 2 encoded-str))
		(letter-entity (match-string-no-properties 3 encoded-str)))
	    (cond (number-entity
		   (list-push
		    (char-to-string
		     (twittering-ucs-to-char
		      (string-to-number number-entity))) result))
		  (letter-entity
		   (cond ((string= "gt" letter-entity) (list-push ">" result))
			 ((string= "lt" letter-entity) (list-push "<" result))
			 (t (list-push "?" result))))
		  (t (list-push "?" result)))
	    (setq cursor (match-end 0))))
	(list-push (substring encoded-str cursor) result)
	(apply 'concat (nreverse result)))
    ""))

(defun twittering-timer-action (func)
  (let ((buf (get-buffer twittering-buffer)))
    (if (null buf)
	(twittering-stop)
      (funcall func)
      )))

(defun twittering-status-not-blank-p (status)
  (not (string-match
	"^\\s-*\\(?:@[-_a-z0-9]+\\(\\s+@[-_a-z0-9]+\\)*\\)?\\s-*$" status)))
 
(defun twittering-show-minibuffer-length (&optional beg end len)
  "Show the number of charactors in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (if (and transient-mark-mode deactivate-mark)
	(deactivate-mark))
    (let* ((deactivate-mark deactivate-mark)
	   (status-len (- (buffer-size) (minibuffer-prompt-width)))
	   (sign-len (length (twittering-sign-string)))
	   (mes (if (< 0 sign-len)
		    (format "%d=%d+%d"
			    (+ status-len sign-len) status-len sign-len)
		  (format "%d" status-len))))
      (if (<= 23 emacs-major-version)
	  (minibuffer-message mes) ; Emacs23 or later
	(minibuffer-message (concat " (" mes ")")))
      )))

(defun twittering-setup-minibuffer ()
  (add-hook 'post-command-hook 'twittering-show-minibuffer-length t t))

(defun twittering-finish-minibuffer ()
  (remove-hook 'post-command-hook 'twittering-show-minibuffer-length t))

(defun twittering-update-status-from-minibuffer (&optional init-str
							   reply-to-id)
  (when (and (null init-str)
	     twittering-current-hashtag)
    (setq init-str (format " #%s " twittering-current-hashtag)))
  (let ((status init-str)
	(sign-str (twittering-sign-string))
	(not-posted-p t)
	(prompt "status: ")
	(map minibuffer-local-map)
	(minibuffer-message-timeout nil))
    (define-key map (kbd "<f4>") 'twittering-tinyurl-replace-at-point)
    (add-hook 'minibuffer-setup-hook 'twittering-setup-minibuffer t)
    (add-hook 'minibuffer-exit-hook 'twittering-finish-minibuffer t)
    (unwind-protect
	(while not-posted-p
	  (setq status (read-from-minibuffer prompt status map nil 'twittering-tweet-history nil t))
	  (let ((status-with-sign (concat status sign-str)))
	    (if (< 140 (length status-with-sign))
		(setq prompt "status (too long): ")
	      (progn
		(setq prompt "status: ")
		(when (twittering-status-not-blank-p status)
		  (let ((parameters `(("status" . ,status-with-sign)
				      ("source" . "twmode")
				      ,@(if reply-to-id
					    `(("in_reply_to_status_id"
					       . ,reply-to-id))))))
		    (twittering-http-post "twitter.com" "statuses/update" parameters)
		    (setq not-posted-p nil)))
		))))
      (remove-hook 'minibuffer-setup-hook 'twittering-setup-minibuffer)
      (remove-hook 'minibuffer-exit-hook 'twittering-finish-minibuffer)
      )))

(defun twittering-get-timeline (method &optional noninteractive id)
  (twittering-get-twits "twitter.com"
			(concat "statuses/" method) noninteractive id))

(defun twittering-get-list (username listname)
  (twittering-get-twits "api.twitter.com"
			(concat "1/" username "/lists/" listname "/statuses")))

(defun twittering-get-list-index (username)
  (twittering-http-get "api.twitter.com"
			(concat "1/" username "/lists")
			t nil
			'twittering-http-get-list-index-sentinel))


(defun twittering-manage-friendships (method username)
  (twittering-http-post "twitter.com"
			(concat "friendships/" method)
			`(("screen_name" . ,username)
			  ("source" . "twmode"))))

(defun twittering-manage-favorites (method id)
  (twittering-http-post "twitter.com"
			(concat "favorites/" method "/" id)
			`(("source" . "twmode"))))

(defun twittering-tinyurl-get (longurl)
  "Tinyfy LONGURL"
  (require 'url)
  (let ((api (cdr (assoc twittering-tinyurl-service
			 twittering-tinyurl-services-map))))
    (unless api
      (error (concat
	      "`twittering-tinyurl-service' was invalid. try one of "
	      (mapconcat (lambda (x)
			   (symbol-name (car x)))
			 twittering-tinyurl-services-map ", ")
	      ".")))
    (if longurl
	(save-excursion
	  (let ((buffer (url-retrieve-synchronously (concat api longurl))))
	    (set-buffer buffer)
	    (goto-char (point-min))
	    (search-forward-regexp "\n\r?\n\\([^\n\r]*\\)")
	    (prog1
		(match-string-no-properties 1)
	      (kill-buffer buffer))))
      nil)))

(defun twittering-tinyurl-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (twittering-tinyurl-get (thing-at-point 'url))))
	(when url
	  (save-restriction
	    (narrow-to-region (car url-bounds) (cdr url-bounds))
	    (delete-region (point-min) (point-max))
	    (insert url)))))))

;;;
;;; Commands
;;;

(defun twittering-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'twittering-current-timeline-noninteractive))
  (if twittering-timer
      nil
    (setq twittering-timer
	  (run-at-time "0 sec"
		       twittering-timer-interval
		       #'twittering-timer-action action))))

(defun twittering-stop ()
  (interactive)
  (when twittering-timer
    (cancel-timer twittering-timer)
    (setq twittering-timer nil)))

(defun twittering-get-twits (host method &optional noninteractive id)
  (unless (string= (twittering-last-method) method)
    (setq twittering-timeline-last-update nil
	  twittering-timeline-data nil
	  twittering-last-timeline-retrieved `(,host ,method)))
  (let ((buf (get-buffer twittering-buffer)))
    (if (not buf)
	(twittering-stop)
      (let* ((default-count 20)
	     (count twittering-number-of-tweets-on-retrieval)
	     (count (cond
		     ((integerp count) count)
		     ((string-match "^[0-9]+$" count)
		      (string-to-number count 10))
		     (t default-count)))
	     (count (min (max 1 count)
			 twittering-max-number-of-tweets-on-retrieval))
	     (regexp-list-method "^1/[^/]*/lists/[^/]*/statuses$")
	     (parameters
	      (list (cons (if (string-match regexp-list-method method)
			      "per_page"
			    "count")
			  (number-to-string count)))))
	(if id
	    (add-to-list 'parameters `("max_id" . ,id))
	  (when twittering-timeline-last-update
	    (let* ((system-time-locale "C")
		   (since
		    (twittering-global-strftime
		     "%a, %d %b %Y %H:%M:%S GMT"
		     twittering-timeline-last-update)))
	      (add-to-list 'parameters `("since" . ,since)))))
	(twittering-http-get (twittering-last-host) method
			     noninteractive parameters))))

  (if (and twittering-icon-mode window-system
	   twittering-image-stack)
      (twittering-retrieve-image twittering-image-stack)
    ))

(defun twittering-retrieve-image (images)
  (if twittering-use-wget
      (twittering-retrieve-image-with-wget images)
    (twittering-retrieve-image-without-wget images)))

(defun twittering-url-copy-file-async(url file)
  (require 'url)
  (url-retrieve
   url
   `(lambda ,(if (<= 22 emacs-major-version) '(status) '())
      (let ((coding-system-for-write 'binary)
	    (require-final-newline nil))
	(goto-char (point-min))
	(search-forward-regexp "^$")
	(write-region (1+ (point)) (point-max) ,file)
	(kill-buffer (current-buffer))))))

(defun twittering-retrieve-image-without-wget (image-urls)
  (mapc
   (lambda (url)
     (let ((file (concat twittering-tmp-dir "/" (twittering-icon-path url))))
       (unless (file-exists-p file)
	 (twittering-url-copy-file-async url file))))
   image-urls))

(defun twittering-retrieve-image-with-wget (image-urls)
  (dolist (url image-urls)
    (let ((file (concat twittering-tmp-dir "/" (twittering-icon-path url))))
      (unless (file-exists-p file)
	(let ((proc
	       (funcall
		#'start-process
		"wget-images"
		(twittering-wget-buffer)
		"wget"
		"--quiet"
		(format "--directory-prefix=%s" twittering-tmp-dir)
		"-O" file
		url)))
	  (set-process-sentinel
	   proc
	   (lambda (proc stat)
	     (clear-image-cache)
	     (save-excursion
	       (set-buffer (twittering-wget-buffer))
	       ))))))))

(defun twittering-friends-timeline ()
  (interactive)
  (twittering-get-timeline "friends_timeline"))

(defun twittering-replies-timeline ()
  (interactive)
  (twittering-get-timeline "replies"))

(defun twittering-public-timeline ()
  (interactive)
  (twittering-get-timeline "public_timeline"))

(defun twittering-user-timeline ()
  (interactive)
  (twittering-get-timeline "user_timeline"))

(defun twittering-current-timeline-noninteractive ()
  (twittering-current-timeline t))

(defun twittering-current-timeline (&optional noninteractive)
  (interactive)
  (if (not twittering-last-timeline-retrieved)
      (setq twittering-last-timeline-retrieved
	    '("twitter.com" "statuses/friends_timeline")))
  (twittering-get-twits (twittering-last-host) (twittering-last-method)
			noninteractive))

(defun twittering-update-status-interactive ()
  (interactive)
  (twittering-update-status-from-minibuffer))

(defun twittering-update-lambda ()
  (interactive)
  (when (and (string-equal "Japanese" current-language-environment)
	     (or (> emacs-major-version 21)
		 (eq 'utf-8 (terminal-coding-system))))
    (twittering-http-post
     "twitter.com"
     "statuses/update"
     `(("status" . ,(mapconcat
		     'char-to-string
		     (mapcar 'twittering-ucs-to-char
			     '(955 12363 12431 12356 12356 12424 955)) ""))
       ("source" . "twmode")))))

(defun twittering-update-jojo (usr msg)
  (when (and (string-equal "Japanese" current-language-environment)
	     (or (> emacs-major-version 21)
		 (eq 'utf-8 (terminal-coding-system))))
    (if (string-match
	 (mapconcat
	  'char-to-string
	  (mapcar 'twittering-ucs-to-char
		  '(27425 12395 92 40 12362 21069 92 124 36020 27096
			  92 41 12399 12300 92 40 91 94 12301 93 43 92 
			  41 12301 12392 35328 12358)) "")
	 msg)
	(twittering-http-post
	 "twitter.com"
	 "statuses/update"
	 `(("status" . ,(concat
			 "@" usr " "
			 (match-string-no-properties 2 msg)
			 (string-as-multibyte
			  (if (>= emacs-major-version 23)
			      "\343\200\200\343\201\257\343\201\243!?"
			    "\222\241\241\222\244\317\222\244\303!?"))))
	   ("source" . "twmode"))))))

(defun twittering-set-current-hashtag (&optional tag)
  (interactive)
  (unless tag
    (setq tag (completing-read "hashtag (blank to clear): #"
			       twittering-hashtag-history
			       nil nil
			       twittering-current-hashtag
			       'twittering-hashtag-history
			       ))
    (message
     (if (eq 0 (length tag))
	 (progn (setq twittering-current-hashtag nil)
		"Current hashtag is not set.")
       (progn
	 (setq twittering-current-hashtag tag)
	 (format "Current hashtag is #%s" twittering-current-hashtag))))))

(defun twittering-erase-old-statuses ()
  (interactive)
  (setq twittering-timeline-data nil)
  (if (not twittering-last-timeline-retrieved)
      (setq twittering-last-timeline-retrieved
	    '("twitter.com" "statuses/friends_timeline"))
  (if (not twittering-timeline-last-update)
      (twittering-http-get (twittering-last-host) (twittering-last-method))
    (let* ((system-time-locale "C")
	   (since
	    (twittering-global-strftime
	     "%a, %d %b %Y %H:%M:%S GMT"
	     twittering-timeline-last-update)))
      (twittering-http-get (twittering-last-host) (twittering-last-method)
			   nil `(("since" . ,since)))))))

(defun twittering-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twittering-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(id (get-text-property (point) 'id))
	(uri (get-text-property (point) 'uri))
	(uri-in-text (get-text-property (point) 'uri-in-text))
	(screen-name-in-text
	 (get-text-property (point) 'screen-name-in-text)))
    (cond (screen-name-in-text
	   (twittering-update-status-from-minibuffer
	    (concat "@" screen-name-in-text " ") id))
	  (uri-in-text
	   (browse-url uri-in-text))
	  (username
	   (twittering-update-status-from-minibuffer
	    (concat "@" username " ") id))
	  (uri
	   (browse-url uri)))))

(defun twittering-format-string (string prefix replacement-table)
  "Format STRING according to PREFIX and REPLACEMENT-TABLE.
PREFIX is a regexp. REPLACEMENT-TABLE is a list of (FROM . TO) pairs,
where FROM is a regexp and TO is a string or a 2-parameter function.

The pairs in REPLACEMENT-TABLE are stored in order of precedence.
First, search PREFIX in STRING from left to right.
If PREFIX is found in STRING, try to match the following string with
FROM of each pair in the same order of REPLACEMENT-TABLE. If FROM in
a pair is matched, replace the prefix and the matched string with a
string generated from TO.
If TO is a string, the matched string is replaced with TO.
If TO is a function, the matched string is replaced with the
return value of (funcall TO CONTEXT), where CONTEXT is an alist.
Each element of CONTEXT is (KEY . VALUE) and KEY is one of the
following symbols;
  'following-string  --the matched string following the prefix
  'match-data --the match-data for the regexp FROM.
  'prefix --PREFIX.
  'replacement-table --REPLACEMENT-TABLE.
  'from --FROM.
  'processed-string --the already processed string.
"
  (let ((current-pos 0)
	(result "")
	(case-fold-search nil))
    (while (and (string-match prefix string current-pos)
		(not (eq (match-end 0) current-pos)))
      (let ((found nil)
	    (current-table replacement-table)
	    (next-pos (match-end 0))
	    (matched-string (match-string 0 string))
	    (skipped-string
	     (substring string current-pos (match-beginning 0))))
	(setq result (concat result skipped-string))
	(setq current-pos next-pos)
	(while (and (not (null current-table))
		    (not found))
	  (let ((key (caar current-table))
		(value (cdar current-table))
		(following-string (substring string current-pos))
		(case-fold-search nil))
	    (if (string-match (concat "^" key) following-string)
		(let ((next-pos (+ current-pos (match-end 0)))
		      (output
		       (if (stringp value)
			   value
			 (funcall value
				  `((following-string . ,following-string)
				    (match-data . ,(match-data))
				    (prefix . ,prefix)
				    (replacement-table . ,replacement-table)
				    (from . ,key)
				    (processed-string . ,result))))))
		  (setq found t)
		  (setq current-pos next-pos)
		  (setq result (concat result output)))
	      (setq current-table (cdr current-table)))))
	(if (not found)
	    (setq result (concat result matched-string)))))
    (let* ((skipped-string (substring string current-pos)))
      (concat result skipped-string))
    ))

(defun twittering-retweet ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(text (get-text-property (point) 'text))
	(id (get-text-property (point) 'id))
	(retweet-time (current-time))
	(format-str (or twittering-retweet-format
			"RT: %t (via @%s)")))
    (when username
      (let ((prefix "%")
	    (replace-table
	     `(("%" . "%")
	       ("s" . ,username)
	       ("t" . ,text)
	       ("#" . ,id)
	       ("C{\\([^}]*\\)}" .
		(lambda (context)
		  (let ((str (cdr (assq 'following-string context)))
			(match-data (cdr (assq 'match-data context))))
		    (store-match-data match-data)
		    (format-time-string (match-string 1 str) ',retweet-time))))
	       ))
	    )
	(twittering-update-status-from-minibuffer
	 (twittering-format-string format-str prefix replace-table))
	))))

(defun twittering-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twittering-follow (&optional remove)
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(method (if remove "destroy" "create"))
	(mes (if remove "unfollowing" "following")))
    (unless username
      (setq username (twittering-read-username-with-completion
		      "who: " "" 'twittering-user-history)))
    (if (> (length username) 0)
	(when (y-or-n-p (format "%s %s? " mes username))
	  (twittering-manage-friendships method username))
      (message "No user selected"))))

(defun twittering-unfollow ()
  (interactive)
  (twittering-follow t))

(defun twittering-favorite (&optional remove)
  (interactive)
  (let ((id (get-text-property (point) 'id))
	(text (get-text-property (point) 'text))
	(len 25) ;; XXX
	(method (if remove "destroy" "create")))
    (if id
	(let ((mes (format "%s \"%s\"? "
			   (if remove "unfavorite" "favorite")
			   (if (> (length text) len)
			       (concat (substring text 0 len) "...")
			     text))))
	  (when (y-or-n-p mes)
	    (twittering-manage-favorites method id)))
      (message "No status selected"))))

(defun twittering-unfavorite ()
  (interactive)
  (twittering-favorite t))

(defun twittering-other-user-timeline ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(screen-name-in-text
	 (get-text-property (point) 'screen-name-in-text)))
    (cond (screen-name-in-text
	   (twittering-get-timeline
	    (concat "user_timeline/" screen-name-in-text)))
	  (username
	   (twittering-get-timeline (concat "user_timeline/" username)))
	  (t
	   (message "No user selected")))))

(defun twittering-other-user-timeline-interactive ()
  (interactive)
  (let ((username
	 (twittering-read-username-with-completion
	  "user: " nil
	  'twittering-user-history)))
    (if (> (length username) 0)
	(twittering-get-timeline (concat "user_timeline/" username))
      (message "No user selected"))))

(defun twittering-other-user-list-interactive ()
  (interactive)
  (let ((username (twittering-read-username-with-completion
		   "whose list: "
		   (get-text-property (point) 'username)
		   'twittering-user-history)))
    (if (> (length username) 0)
	(progn
	  (setq twittering-list-index-retrieved nil)
	  (twittering-get-list-index username)
	  (while (not twittering-list-index-retrieved)
	    (sit-for 0.1))
	  (cond
	   ((listp twittering-list-index-retrieved)
	    (let ((choice (completing-read
			   (concat username "'s list: ")
			   twittering-list-index-retrieved
			   nil t "")))
	      (when (and choice (not (string= "" choice)))
		(twittering-get-list username choice))))
	   ((stringp twittering-list-index-retrieved)
	    (if (string= "" twittering-list-index-retrieved)
		(message (concat username " have no list"))
	      (message twittering-list-index-retrieved)))))
      (message "No user selected"))))

(defun twittering-direct-message ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(twittering-update-status-from-minibuffer (concat "d " username " ")))))

(defun twittering-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(twittering-update-status-from-minibuffer (concat "@" username " ")))))

(defun twittering-make-list-from-assoc (key data)
  (mapcar (lambda (status)
	    (cdr (assoc key status)))
	  data))

(defun twittering-read-username-with-completion (prompt init-user
							&optional history)
  (completing-read prompt
		   (append (twittering-make-list-from-assoc
			    'user-screen-name twittering-timeline-data)
			   twittering-user-history)
		   nil nil init-user history))

(defun twittering-get-username ()
  (or twittering-username-active
      (setq twittering-username-active (read-string "your twitter username: "))))

(defun twittering-get-password ()
  (or twittering-password-active
      (setq twittering-password-active (read-passwd "your twitter password: "))))

(defun twittering-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos))
    (setq pos (twittering-get-next-username-face-pos (point)))
    (if pos
	(goto-char pos)
      (let ((id (get-text-property (point) 'id)))
        (if id
	    (twittering-get-twits (twittering-last-host)
				  (twittering-last-method)
				  nil id))))))

(defun twittering-get-next-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twittering-username-face)))
	(setq pos (next-single-property-change pos 'face))
	(when (eq pos nil) (throw 'not-found nil))
	(setq prop (get-text-property pos 'face)))
      pos)))

(defun twittering-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let* ((current-pos (point))
         (prev-pos (twittering-get-previous-username-face-pos current-pos)))
    (if (and prev-pos (not (eq current-pos prev-pos)))
        (goto-char prev-pos)
      (message "Start of status."))))

(defun twittering-get-previous-username-face-pos (pos)
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (eq prop twittering-username-face)))
	(setq pos (previous-single-property-change pos 'face))
	(when (eq pos nil)
	  (let ((head-prop (get-text-property (point-min) 'face)))
	    (if (and
		 (not (eq prop twittering-username-face))
		 (eq head-prop twittering-username-face))
		(setq pos (point-min))
	      (throw 'not-found nil)
	      )))
	(setq prop (get-text-property pos 'face)))
      pos)))

(defun twittering-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (twittering-get-username-at-pos (point)))
	(pos (twittering-get-next-username-face-pos (point))))
    (while (and (not (eq pos nil))
		(not (equal (twittering-get-username-at-pos pos) user-name)))
      (setq pos (twittering-get-next-username-face-pos pos)))
    (if pos
	(goto-char pos)
      (if user-name
	  (message "End of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twittering-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (twittering-get-username-at-pos (point)))
        (prev-pos (point))
	(pos (twittering-get-previous-username-face-pos (point))))
    (while (and (not (eq pos nil))
                (not (eq pos prev-pos))
		(not (equal (twittering-get-username-at-pos pos) user-name)))
      (setq prev-pos pos)
      (setq pos (twittering-get-previous-username-face-pos pos)))
    (if (and pos
             (not (eq pos prev-pos))
             (equal (twittering-get-username-at-pos pos) user-name))
	(goto-char pos)
      (if user-name
	  (message "Start of %s's status." user-name)
	(message "Invalid user-name.")))))

(defun twittering-goto-next-thing (&optional backword)
  "Go to next interesting thing. ex) username, URI, ... "
  (interactive)
  (let* ((propety-change-f (if backword
			       'previous-single-property-change
			     'next-single-property-change))
	 (pos (funcall propety-change-f (point) 'face)))
    (while (and pos
		(not 
		 (let* ((current-face (get-text-property pos 'face))
			(face-pred
			 (lambda (face)
			   (cond
			    ((listp current-face) (memq face current-face))
			    ((symbolp current-face) (eq face current-face))
			    (t nil)))))
		   (member-if face-pred
			      '(twittering-username-face
				twittering-uri-face)))))
      (setq pos (funcall propety-change-f pos 'face)))
    (when pos
      (goto-char pos))))

(defun twittering-goto-previous-thing (&optional backword)
  "Go to previous interesting thing. ex) username, URI, ... "
  (interactive)
  (twittering-goto-next-thing (not backword)))

(defun twittering-get-username-at-pos (pos)
  (or (get-text-property pos 'username)
      (get-text-property (max (point-min) (1- pos)) 'username)
      (let* ((border (or (previous-single-property-change pos 'username)
                         (point-min)))
             (pos (max (point-min) (1- border))))
        (get-text-property pos 'username))))

(defun twittering-get-status-url (username id)
  "Generate status URL."
  (format "http://twitter.com/%s/statuses/%s" username id))

(defun twittering-suspend ()
  "Suspend twittering-mode then switch to another buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;###autoload
(defun twit ()
  "Start twittering-mode."
  (interactive)
  (twittering-mode))

(provide 'twittering-mode)
;;; twittering.el ends here
