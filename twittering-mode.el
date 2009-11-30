;;; twittering-mode.el --- Major mode for Twitter

;; Copyright (C) 2007 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO

;; Author: Y. Hayamizu <y.hayamizu@gmail.com>
;;         Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;         Alberto Garcia  <agarcia@igalia.com>
;; Created: Sep 4, 2007
;; Version: 0.4
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

(require 'cl)
(require 'xml)
(require 'parse-time)
(require 'mm-url)

(defconst twittering-mode-version "0.8")
(defconst twittering-max-number-of-tweets-on-retrieval 200
  "The maximum number of `twittering-number-of-tweets-on-retrieval'.")

(defconst tinyurl-service-url "http://tinyurl.com/api-create.php?url="
  "service url for tinyurl")

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

(defvar twittering-current-hashtag nil)

(defvar twittering-idle-time 20)

(defvar twittering-timer-interval 90)

(defvar twittering-username nil)
(defvar twittering-username-active nil)

(defvar twittering-password nil)
(defvar twittering-password-active nil)

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

(defvar twittering-status-format nil)
(setq twittering-status-format "%i %s,  %@:\n  %t // from %f%L%r")
;; %s - screen_name
;; %S - name
;; %i - profile_image
;; %d - description
;; %l - location
;; %L - " [location]"
;; %r - " in reply to user"
;; %u - url
;; %j - user.id
;; %p - protected?
;; %c - created_at (raw UTC string)
;; %C{time-format-str} - created_at (formatted with time-format-str)
;; %@ - X seconds ago
;; %t - text
;; %' - truncated
;; %f - source
;; %# - id

(defvar twittering-retweet-format "RT: %t (via @%s)")
;; %s - screen_name
;; %t - text
;; %% - %

(defvar twittering-notify-successful-http-get t)

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
directory. You should change through function'twittering-icon-mode'")

(make-variable-buffer-local 'twittering-icon-mode)
(defun twittering-icon-mode (&optional arg)
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
  (force-mode-line-update)
  (twittering-render-timeline))

(defun twittering-scroll-mode (&optional arg)
  (interactive)
  (setq twittering-scroll-mode
	(if (null arg)
	    (not twittering-scroll-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defun twittering-jojo-mode (&optional arg)
  (interactive)
  (setq twittering-jojo-mode
	(if (null arg)
	    (not twittering-jojo-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defvar twittering-image-stack nil)
(defvar twittering-image-type-cache nil)
(defvar twittering-convert-program "/usr/bin/convert")
(defvar twittering-convert-fix-size nil)

(defun twittering-image-type (file-name)
  (if (and (not (assoc file-name twittering-image-type-cache))
	   (file-exists-p file-name))
      (if twittering-convert-fix-size
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
      			 ((string-match "bitmap" file-output)
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
	     (insert (prin1-to-string ,obsym))
	     (newline)
	     ,obsym)
	 ,obsym))))

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
      (define-key km [tab] 'twittering-goto-next-thing)
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
  (add-to-list 'minor-mode-alist '(twittering-icon-mode " tw-icon"))
  (add-to-list 'minor-mode-alist '(twittering-scroll-mode " tw-scroll"))
  (add-to-list 'minor-mode-alist '(twittering-jojo-mode " tw-jojo"))
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
(defmacro twittering-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defvar twittering-mode-string "Twittering mode")

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
  (setq mode-name twittering-mode-string)
  (set-syntax-table twittering-mode-syntax-table)
  (run-hooks 'twittering-mode-hook)
  (font-lock-mode -1)
  (twittering-stop)
  (twittering-start))

;;;
;;; Basic HTTP functions
;;;

(defun twittering-make-http-request (host method method-class
					  &optional parameters)
  (let ((nl "\r\n")
	request)
    (setq request
	  (concat method " http://" host "/" method-class ".xml"
	   (when parameters
		    (concat "?"
			    (mapconcat
			     (lambda (param-pair)
			       (format "%s=%s"
				       (twittering-percent-encode
					(car param-pair))
				       (twittering-percent-encode
					(cdr param-pair))))
			     parameters
			     "&")))
		  " HTTP/1.1" nl
		  "Host: " host nl
		  "User-Agent: " (twittering-user-agent) nl
		  "Authorization: Basic "
		  (base64-encode-string
		   (concat
		    (twittering-get-username) ":" (twittering-get-password)))
		  nl
		  (when (string= "GET" method)
		    (concat
		     "Accept: text/xml"
		     ",application/xml"
		     ",application/xhtml+xml"
		     ",application/html;q=0.9"
		     ",text/plain;q=0.8"
		     ",image/png,*/*;q=0.5" nl
		     "Accept-Charset: utf-8;q=0.7,*;q=0.7"
		     nl))
		  (when (string= "POST" method)
		    (concat
		     "Content-Type: text/plain" nl
		     "Content-Length: 0" nl))
		  (when twittering-proxy-use
		    (concat
		     (when twittering-proxy-keep-alive
		       (concat "Proxy-Connection: Keep-Alive" nl))
		     (when (and twittering-proxy-user
				twittering-proxy-password)
		       (concat
			"Proxy-Authorization: Basic "
			(base64-encode-string
			 (concat
			  twittering-proxy-user ":" twittering-proxy-password))
			nl)
		       )))
		  nl))
    (debug-print (concat method "Request\n" request))
    request))

(defun twittering-http-get
  (host method &optional noninteractive parameters sentinel)
  (if (null sentinel) (setq sentinel 'twittering-http-get-default-sentinel))

  (let ((server host)
	(port "80")
	(proxy-user twittering-proxy-user)
	(proxy-password twittering-proxy-password)
	(temp-buffer (generate-new-buffer "*twmode-http-buffer*"))
	proc)
    (condition-case get-error
	(progn
	  (if (and twittering-proxy-use twittering-proxy-server)
	      (setq server twittering-proxy-server
		    port (if (integerp twittering-proxy-port)
			     (int-to-string twittering-proxy-port)
			   twittering-proxy-port))
	    )
	  (setq proc
		(open-network-stream
		 "network-connection-process" temp-buffer
		 server (string-to-number port)))
          (lexical-let ((temp-buffer temp-buffer)
			(sentinel sentinel)
			(noninteractive noninteractive))
            (set-process-sentinel proc (lambda (&rest args) (apply sentinel temp-buffer noninteractive args))))
	  (process-send-string
	   proc
	   (twittering-make-http-request host "GET" method parameters)))
      (error
       (message (format "Failure: HTTP GET: %s" get-error)) nil))))

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
		  (search-forward "\r?\n\r?\n" nil t)
		  (while (re-search-forward
			  "<slug>\\([-a-zA-Z0-9_]+\\)</slug>" nil t)
		    (push (match-string 1) indexes))
		  (if indexes
		      (setq twittering-list-index-retrieved indexes)
		    (setq twittering-list-index-retrieved ""))))
	       (t
		(setq twittering-list-index-retrieved status)))))))
    (kill-buffer temp-buffer)))

(defun twittering-http-get-default-sentinel (temp-buffer noninteractive proc stat &optional suc-msg)
  (unwind-protect
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
    (kill-buffer temp-buffer))
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
	       (save-excursion (beginning-of-line) (point)) (point))
	      (insert "\n"))
	    twittering-timeline-data)
      (if (and twittering-image-stack window-system)
	  (clear-image-cache))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if twittering-scroll-mode (- (point-max) end) 0))))
    ))

(defun twittering-format-status (status format-str)
  ;; Formatting strategy:
  ;; 
  ;; 1. Search the special character '%' in format-str, expand it with
  ;; corresponding string(such as username, image, description, ...),
  ;; and pushes it on 'result' until the end of format-str.
  ;; 2. concat strings in 'result' together
  ;;
  ;; Example:
  ;;  format-str: "%s, %@:\n %t", where screen name is "hayamiz",
  ;;    timestamp is "1 minute ago", and text is "hello, world"
  ;;  result: ("hello, world" ":\n " "1 minute ago" ", " "hayamiz")
  ;;
  (flet ((attr (key)
	       (assocref key status))
	 (profile-image
	  ()
	  (let ((profile-image-url (attr 'user-profile-image-url))
		(icon-string "\n  "))
	    (if (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" profile-image-url)
		(let* ((filename (match-string-no-properties 1
							     profile-image-url))
		       (fullpath (concat twittering-tmp-dir "/" filename)))
		  ;; download icons if does not exist
		  (if (file-exists-p fullpath)
		      t
		    (add-to-list 'twittering-image-stack profile-image-url))

		  (when (and icon-string twittering-icon-mode)
		    (set-text-properties
		     1 2 `(display
			   (image :type ,(twittering-image-type fullpath)
				  :file ,fullpath))
		     icon-string)
		    icon-string)
		  )))))
    (let ((cursor 0)
	  (result ())
	  c
	  found-at)
      (setq cursor 0)
      (setq result '())
      (while (setq found-at (string-match "%\\(C{\\([^}]+\\)}\\|[A-Za-z#@']\\)"
					  format-str cursor))
	(setq c (string-to-char (match-string-no-properties 1 format-str)))
	(if (> found-at cursor)
	    (list-push (substring format-str cursor found-at) result)
	  "|")
	(setq cursor (match-end 1))

	(case c
	  ((?s)                         ; %s - screen_name
	   (list-push (attr 'user-screen-name) result))
	  ((?S)                         ; %S - name
	   (list-push (attr 'user-name) result))
	  ((?i)                         ; %i - profile_image
	   (list-push (profile-image) result))
	  ((?d)                         ; %d - description
	   (list-push (attr 'user-description) result))
	  ((?l)                         ; %l - location
	   (list-push (attr 'user-location) result))
	  ((?L)                         ; %L - " [location]"
	   (let ((location (attr 'user-location)))
	     (unless (or (null location) (string= "" location))
	       (list-push (concat " [" location "]") result)) ))
	  ((?u)                         ; %u - url
	   (list-push (attr 'user-url) result))
	  ((?j)                         ; %j - user.id
	   (list-push (attr 'user-id) result))
	  ((?r)				; %r - in_reply_to_status_id
	   (let ((reply-id (attr 'in-reply-to-status-id))
		 (reply-name (attr 'in-reply-to-screen-name)))
	     (unless (or (null reply-id) (string= "" reply-id)
			 (null reply-name) (string= "" reply-name))
	       (let ((in-reply-to-string (format "in reply to %s" reply-name))
		     (url (twittering-get-status-url reply-name reply-id)))
		 (add-text-properties
		  0 (length in-reply-to-string)
		  `(mouse-face highlight
			       face twittering-uri-face
			       uri ,url)
		  in-reply-to-string)
		 (list-push (concat " " in-reply-to-string) result)))))
	  ((?p)                         ; %p - protected?
	   (let ((protected (attr 'user-protected)))
	     (when (string= "true" protected)
	       (list-push "[x]" result))))
	  ((?c)                     ; %c - created_at (raw UTC string)
	   (list-push (attr 'created-at) result))
	  ((?C)	    ; %C{time-format-str} - created_at (formatted with
					; time-format-str)
	   (list-push (twittering-local-strftime
		       (or (match-string-no-properties 2 format-str) "%H:%M:%S")
		       (attr 'created-at))
		      result))
	  ((?@)                         ; %@ - X seconds ago
	   (let ((created-at
		  (apply
		   'encode-time
		   (parse-time-string (attr 'created-at))))
		 (now (current-time)))
	     (let ((secs (+ (* (- (car now) (car created-at)) 65536)
			    (- (cadr now) (cadr created-at))))
		   time-string url)
	       (setq time-string
		     (cond ((< secs 5) "less than 5 seconds ago")
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
	       (setq url (twittering-get-status-url (attr 'user-screen-name)
						    (attr 'id)))
	       ;; make status url clickable
	       (add-text-properties
		0 (length time-string)
		`(mouse-face highlight
			     face twittering-uri-face
			     uri ,url)
		time-string)
	       (list-push time-string result))))
	  ((?t)                         ; %t - text
	   (list-push                   ;(clickable-text)
	    (attr 'text)
	    result))
	  ((?')                         ; %' - truncated
	   (let ((truncated (attr 'truncated)))
	     (when (string= "true" truncated)
	       (list-push "..." result))))
	  ((?f)                         ; %f - source
	   (list-push (attr 'source) result))
	  ((?#)                         ; %# - id
	   (list-push (attr 'id) result))
	  (t
	   (list-push (char-to-string c) result)))
	)
      (list-push (substring format-str cursor) result)
      (let ((formatted-status (apply 'concat (nreverse result))))
	(add-text-properties 0 (length formatted-status)
			     `(username ,(attr 'user-screen-name)
					id ,(attr 'id)
					text ,(attr 'text))
			     formatted-status)
	formatted-status)
      )))

(defun twittering-http-post
  (host method &optional parameters contents sentinel)
  "Send HTTP POST request to twitter.com (or api.twitter.com)

HOST is hostname of remote side, twitter.com or api.twitter.com.
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (if (null sentinel) (setq sentinel 'twittering-http-post-default-sentinel))

  (let ((server host)
	(port "80")
	(proxy-user twittering-proxy-user)
	(proxy-password twittering-proxy-password)
	(temp-buffer (generate-new-buffer "*twmode-http-buffer*"))
	proc)
    (progn
      (if (and twittering-proxy-use twittering-proxy-server)
	  (setq server twittering-proxy-server
		port (if (integerp twittering-proxy-port)
			 (int-to-string twittering-proxy-port)
		       twittering-proxy-port))
	)
      (setq proc
	    (open-network-stream
	     "network-connection-process" temp-buffer
	     server (string-to-number port)))
      (lexical-let ((temp-buffer temp-buffer)
		    (sentinel sentinel))
	(set-process-sentinel proc
			      (lambda (&rest args)
				(apply sentinel temp-buffer args))))
      (process-send-string
       proc
       (twittering-make-http-request host "POST" method parameters)))))

(defun twittering-http-post-default-sentinel (temp-buffer proc stat &optional suc-msg)

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
    (kill-buffer temp-buffer))
  )

(defun twittering-get-response-header (buffer)
  "Exract HTTP response header from HTTP response.
`buffer' may be a buffer or the name of an existing buffer which contains the HTTP response."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (substring content 0 (string-match "\r?\n\r?\n" content)))))

(defun twittering-get-response-body (buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a
XML tree as list. `buffer' may be a buffer or the name of an existing buffer. "
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
	(xml-parse-region (+ (string-match "\r?\n\r?\n" content)
			     (length (match-string 0 content)))
			  (point-max)))
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

(defun twittering-update-status-if-not-blank (status &optional reply-to-id)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (setq status (concat status (twittering-sign-string)))
    (let ((parameters `(("status" . ,status)
			("source" . "twmode")
			,@(if reply-to-id
			      `(("in_reply_to_status_id"
				 . ,reply-to-id))))))
      (twittering-http-post "twitter.com" "statuses/update" parameters))
    t))

(defun twittering-update-status-from-minibuffer (&optional init-str
							   reply-to-id)
  (when (and (null init-str)
	     twittering-current-hashtag)
    (setq init-str (format " #%s " twittering-current-hashtag)))
  (let ((status init-str) (not-posted-p t) (map minibuffer-local-map))
    (while not-posted-p
      (define-key map (kbd "<f4>") 'twittering-tinyurl-replace-at-point)
      (setq status (read-from-minibuffer "status: " status map nil 'twittering-tweet-history nil t))
      (while (< 140 (length status))
	(setq status (read-from-minibuffer (format "(%d): "
						   (- 140 (length status)))
					   status map nil 'twittering-tweet-history nil t)))
      (setq not-posted-p
	    (not (twittering-update-status-if-not-blank status reply-to-id)))
      )
    ))

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
  (with-temp-buffer
	(mm-url-insert (concat tinyurl-service-url longurl))
	(buffer-substring (point-min) (point-at-eol))))

(defun twittering-tinyurl-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let* ((url-bounds (bounds-of-thing-at-point 'url))
		 (url (thing-at-point 'url))
		 (newurl (twittering-tinyurl-get url)))
	(save-restriction
	  (narrow-to-region (car url-bounds) (cdr url-bounds))
	  (delete-region (point-min) (point-max))
	  (insert newurl))
	newurl))

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

  (if (and twittering-icon-mode window-system)
      (if twittering-image-stack
	  (let ((proc
		 (apply
		  #'start-process
		  "wget-images"
		  (twittering-wget-buffer)
		  "wget"
		  (format "--directory-prefix=%s" twittering-tmp-dir)
		  "--no-clobber"
		  "--quiet"
		  twittering-image-stack)))
	    (set-process-sentinel
	     proc
	     (lambda (proc stat)
	       (clear-image-cache)
	       (save-excursion
		 (set-buffer (twittering-wget-buffer))
		 )))))))

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
  (twittering-http-post
   "twitter.com"
   "statuses/update"
   `(("status" . (string-as-multibyte
                  (if (>= emacs-major-version 23)
                      "\316\273\343\201\213\343\202\217\343\201\204\343\201\204\343\202\210\316\273"
                    "\222\246\313\222\244\253\222\244\357\222\244\244\222\244\244\222\244\350\222\246\313")))
     ("source" . "twmode"))))

(defun twittering-update-jojo (usr msg)
  (if (string-match (string-as-multibyte
                     (if (>= emacs-major-version 23)
                         "\346\254\241\343\201\253\\(\343\201\212\345\211\215\\|\350\262\264\346\247\230\\)\343\201\257\343\200\214\\([^\343\200\215]+\\)\343\200\215\343\201\250\350\250\200\343\201\206"
                       "\222\274\241\222\244\313\\(\222\244\252\222\301\260\\|\222\265\256\222\315\315\\)\222\244\317\222\241\326\\([^\222\241\327]+\\)\222\241\327\222\244\310\222\270\300\222\244\246"))
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
	 ("source" . "twmode")))))

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
return value of (funcall TO the-following-string the-match-data).
"
  (let ((current-pos 0)
	(result "")
	(case-fold-search nil))
    (block nil
      (while (string-match prefix string current-pos)
	(let ((found nil)
	      (current-table replacement-table)
	      (next-pos (match-end 0))
	      (matched-string (match-string 0 string))
	      (skipped-string
	       (substring string current-pos (match-beginning 0))))
	  (when (eq next-pos current-pos)
	    (return result)) ;; no progress. prevent infinite loop
	  
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
			   (funcall value following-string (match-data)))))
		    (setq found t)
		    (setq current-pos next-pos)
		    (setq result (concat result output)))
		(setq current-table (cdr current-table)))))
	  (if (not found)
	      (setq result (concat result matched-string))))))
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
		(lambda (str match-data)
		  (store-match-data match-data)
		  (format-time-string (match-string 1 str) ',retweet-time)))
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
      (setq username (read-from-minibuffer "who: ")))
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
	 (read-from-minibuffer
	  "user: "
	  (or (get-text-property (point) 'screen-name-in-text)
	      (get-text-property (point) 'username))
	  nil nil 'twittering-user-history)))
    (if (> (length username) 0)
	(twittering-get-timeline (concat "user_timeline/" username))
      (message "No user selected"))))

(defun twittering-other-user-list-interactive ()
  (interactive)
  (let ((username (read-from-minibuffer "whose list: " (get-text-property (point) 'username))))
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
	      (when choice
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
