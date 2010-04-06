;;; -*- indent-tabs-mode: t; tab-width: 8 -*-
;;;
;;; twittering-mode.el --- Major mode for Twitter

;; Copyright (C) 2007, 2009, 2010 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO

;; Author: Y. Hayamizu <y.hayamizu@gmail.com>
;;         Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;         Alberto Garcia  <agarcia@igalia.com>
;; Created: Sep 4, 2007
;; Version: HEAD
;; Identity: $Id$
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
(when (> 22 emacs-major-version)
  (add-to-list 'load-path
	       (expand-file-name
               "url-emacs21" (if load-file-name
                                 (or (file-name-directory load-file-name)
                                     ".")
                               ".")))
  (and (require 'un-define nil t)
       ;; the explicitly require 'unicode to update a workaround with
       ;; navi2ch. see a comment of `twittering-ucs-to-char' for more
       ;; details.
       (require 'unicode nil t))
  (set-terminal-coding-system 'utf-8))
(require 'url)

(defconst twittering-mode-version "HEAD")
(defconst twittering-mode-identity "$Id$")
(defvar twittering-api-host "api.twitter.com")
(defvar twittering-api-search-host "search.twitter.com")
(defvar twittering-web-host "twitter.com")

(defun twittering-mode-version ()
  "Display a message for twittering-mode version."
  (interactive)
  (let ((version-string
	 (format "twittering-mode-v%s" twittering-mode-version)))
    (if (interactive-p)
	(message "%s" version-string)
      version-string)))

(defconst twittering-max-number-of-tweets-on-retrieval 200
  "The maximum number of `twittering-number-of-tweets-on-retrieval'.")

(defvar twittering-number-of-tweets-on-retrieval 20
  "*The number of tweets which will be retrieved in one request.
The upper limit is `twittering-max-number-of-tweets-on-retrieval'.")

(defvar twittering-tinyurl-service 'tinyurl
  "The service to use. One of 'tinyurl' or 'toly'.")

(defvar twittering-tinyurl-services-map
  '((tinyurl . "http://tinyurl.com/api-create.php?url=")
    (toly    . "http://to.ly/api.php?longurl="))
  "Alist of tinyfy services.")

(defvar twittering-mode-map (make-sparse-keymap))

(defvar twittering-tweet-history nil)
(defvar twittering-user-history nil)
(defvar twittering-timeline-history nil)
(defvar twittering-hashtag-history nil)
(defvar twittering-search-history nil)

(defvar twittering-current-hashtag nil
  "A hash tag string currently set. You can set it by calling
`twittering-set-current-hashtag'.")

(defvar twittering-timer nil
  "Timer object for timeline refreshing will be stored here.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-timer-interval 90
  "The interval of auto reloading. You should use 60 or more
seconds for this variable because the number of API call is
limited by the hour.")

(defvar twittering-timer-for-redisplaying nil
  "Timer object for timeline redisplay statuses will be stored here.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-timer-interval-for-redisplaying 17
  "The interval of auto redisplaying statuses.")

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

(defvar twittering-initial-timeline-spec-string ":home"
  "The initial timeline spec string.")

(defvar twittering-timeline-spec-alias nil
  "*Alist for aliases of timeline spec.
Each element is (NAME . SPEC-STRING), where NAME is a string and
SPEC-STRING is a string or a function that returns a timeline spec string.

The alias can be referred as \"$NAME\" or \"$NAME(ARG)\" in timeline spec
string. If SPEC-STRING is a string, ARG is simly ignored.
If SPEC-STRING is a function, it is called with a string argument.
For the style \"$NAME\", the function is called with nil.
For the style \"$NAME(ARG)\", the function is called with a string ARG.

For example, if you specify
 '((\"FRIENDS\" . \"(USER1+USER2+USER3)\")
   (\"to_me\" . \"(:mentions+:retweets_of_me+:direct_messages)\")
   (\"related-to\" .
            ,(lambda (username)
               (if username
                   (format \":search/to:%s OR from:%s OR @%s/\"
                           username username username)
                 \":home\")))),
then you can use \"$to_me\" as
\"(:mentions+:retweets_of_me+:direct_messages)\".")

(defvar twittering-current-timeline-spec-string nil
  "The current timeline spec string. This variable should not be referred
directly. Use `twittering-current-timeline-spec-string' or
`twittering-current-timeline-spec'.")
(defvar twittering-list-index-retrieved nil)

(defvar twittering-process-info-alist nil
  "Alist of active process and timeline spec retrieved by the process.")

(defvar twittering-server-info-alist nil
  "Alist of server information.")

(defvar twittering-new-tweets-count 0
  "Number of new tweets when `twittering-new-tweets-hook' is run.")
(defvar twittering-new-tweets-spec nil
  "Timeline spec, which new tweets belong to, when
`twittering-new-tweets-hook' is run.")

(defvar twittering-new-tweets-hook nil
  "*Hook run when new tweets are received.

You can read `twittering-new-tweets-count' or `twittering-new-tweets-spec'
to get the number of new tweets received when this hook is run.")

(defvar twittering-scroll-mode nil)

(defvar twittering-jojo-mode nil)
(defvar twittering-reverse-mode nil
  "*Non-nil means tweets are aligned in reverse order of `http://twitter.com/'.")
(defvar twittering-display-remaining nil
  "*If non-nil, display remaining of rate limit on the mode line.")
(defvar twittering-status-format "%i %s,  %@:\n%FILL{  %T // from %f%L%r%R}\n "
  "Format string for rendering statuses.
Ex. \"%i %s,  %@:\\n%FILL{  %T // from %f%L%r%R}\n \"

Items:
 %s - screen_name
 %S - name
 %i - profile_image
 %d - description
 %l - location
 %L - \" [location]\"
 %r - \" sent to user\" (use on direct_messages{,_sent})
 %r - \" in reply to user\" (use on other standard timeline)
 %R - \" (retweeted by user)\"
 %u - url
 %j - user.id
 %p - protected?
 %c - created_at (raw UTC string)
 %C{time-format-str} - created_at (formatted with time-format-str)
 %@ - X seconds ago
 %T - raw text
 %t - text filled as one paragraph
 %' - truncated
 %FACE[face-name]{...} - strings decorated with the specified face.
 %FILL{...} - strings filled as a paragrah.
              You can use any other specifiers in braces.
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

(defvar twittering-fill-column nil
  "*The fill-column used for \"%FILL{...}\" in `twittering-status-format'.
If nil, the fill-column is automatically calculated.")

(defvar twittering-show-replied-tweets t
  "*The number of replied tweets which will be showed in one tweet.

If the value is not a number and is non-nil, show all replied tweets
which is already fetched.
If the value is nil, doesn't show replied tweets.")

(defvar twittering-default-show-replied-tweets nil
  "*The number of default replied tweets which will be showed in one tweet.
This value will be used only when showing new tweets.

See `twittering-show-replied-tweets' for more details.")

(defvar twittering-use-show-minibuffer-length t
  "*Show current length of minibuffer if this variable is non-nil.

We suggest that you should set to nil to disable the showing function
when it conflict with your input method (such as AquaSKK, etc.)")

(defvar twittering-notify-successful-http-get t)

(defvar twittering-use-ssl t
  "Use SSL connection if this variable is non-nil.

SSL connections use 'curl' command as a backend.")

(defvar twittering-curl-program nil
  "Cache a result of `twittering-find-curl-program'.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-connection-type-order '(curl native))
  "*A list of connection methods in the preferred order."

(defvar twittering-connection-type-table
  '((native (check . t)
	    (https . nil)
	    (start . twittering-start-http-session-native))
    (curl (check . twittering-start-http-session-curl-p)
	  (https . twittering-start-http-session-curl-https-p)
	  (start . twittering-start-http-session-curl)))
  "A list of alist of connection methods.")

(defvar twittering-format-status-function-source ""
  "The status format string that has generated the current
`twittering-format-status-function'.")
(defvar twittering-format-status-function nil
  "The formating function generated from `twittering-format-status-function-source'.")

(defvar twittering-timeline-data-table (make-hash-table :test 'equal))

(defvar twittering-username-face 'twittering-username-face)
(defvar twittering-uri-face 'twittering-uri-face)

(defvar twittering-use-native-retweet nil
  "Post retweets using native retweets if this variable is non-nil.")

(defvar twittering-update-status-function
  'twittering-update-status-from-minibuffer
  "The function used to posting a tweet. It takes two arguments:
the first argument INIT-STR is initial text to be edited and the
second argument REPLY-TO-ID is a user ID of a tweet to which you
are going to reply.

Twittering-mode provides two functions for updating status:
* `twittering-update-status-from-minibuffer': edit tweets in minibuffer
* `twittering-update-status-from-pop-up-buffer': edit tweets in pop-up buffer")

;;;
;;; Proxy setting / functions
;;;

(defvar twittering-proxy-use nil)
(defvar twittering-proxy-keep-alive nil)
(defvar twittering-proxy-server nil
  "*The proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
The port number is specified by `twittering-proxy-port'.")
(defvar twittering-proxy-port nil
  "*The port number of a proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
The server is specified by `twittering-proxy-server'.")
(defvar twittering-proxy-user nil)
(defvar twittering-proxy-password nil)

(defun twittering-find-proxy (scheme)
  "Find proxy server and its port for `twittering-mode' and returns
a cons pair of them.
SCHEME must be \"http\" or \"https\"."
  (cond
   ((require 'url-methods nil t)
    (url-scheme-register-proxy scheme)
    (let* ((proxy-service (assoc scheme url-proxy-services))
           (proxy (if proxy-service (cdr proxy-service) nil)))
      (if (and proxy
               (string-match "^\\([^:]+\\):\\([0-9]+\\)$" proxy))
          (let ((host (match-string 1 proxy))
		(port (string-to-number (match-string 2 proxy))))
            (cons host port))
        nil)))
   (t
    (let* ((env-var (concat scheme "_proxy"))
           (env-proxy (or (getenv (upcase env-var))
                          (getenv (downcase env-var))))
	   (default-port (if (string= "https" scheme) "443" "80")))
      (if (and env-proxy
	       (string-match
		"^\\(https?://\\)?\\([^:/]+\\)\\(:\\([0-9]+\\)\\)?/?$"
		env-proxy))
          (let* ((host (match-string 2 env-proxy))
		 (port-str (or (match-string 4 env-proxy) default-port))
		 (port (string-to-number port-str)))
            (cons host port))
	nil)))))

(defun twittering-setup-proxy ()
  (when (require 'url-methods nil t)
    ;; If `url-scheme-registry' is not initialized,
    ;; `url-proxy-services' will be reset by calling
    ;; `url-insert-file-contents' or `url-retrieve-synchronously', etc.
    ;; To avoid it, initialize `url-scheme-registry' by calling
    ;; `url-scheme-get-property' before calling such functions.
    (url-scheme-get-property "http" 'name)
    (url-scheme-get-property "https" 'name))
  (unless (and twittering-proxy-server twittering-proxy-port)
    (let ((proxy-info (or (if twittering-use-ssl
			      (twittering-find-proxy "https"))
			  (twittering-find-proxy "http"))))
      (when proxy-info
	(let ((host (car proxy-info))
	      (port (cdr proxy-info)))
	  (setq twittering-proxy-server host)
	  (setq twittering-proxy-port port)))))
  (if (and twittering-proxy-use
	   (null twittering-proxy-server)
	   (null twittering-proxy-port))
      (progn
	(message "Disabling proxy due to lack of configuration.")
	(setq twittering-proxy-use nil))
    t))

(defun twittering-toggle-proxy ()
  (interactive)
  (setq twittering-proxy-use
	(not twittering-proxy-use))
  (if (twittering-setup-proxy)
      (message (if twittering-proxy-use "Use Proxy:on" "Use Proxy:off")))
  (twittering-update-mode-line))

;;;
;;; to show image files
;;;

(defvar twittering-icon-mode nil
  "You MUST NOT CHANGE this variable directly.
You should change through function `twittering-icon-mode'.")

(defun twittering-icon-mode (&optional arg)
  "Toggle display of icon images on timelines.
With a numeric argument, if the argument is positive, turn on
icon mode; otherwise, turn off icon mode."
  (interactive "P")
  (let ((prev-mode twittering-icon-mode))
    (setq twittering-icon-mode
	  (if (null arg)
	      (not twittering-icon-mode)
	    (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twittering-icon-mode)
      (twittering-update-mode-line)
      (twittering-render-timeline (current-buffer)))))

(defvar twittering-image-data-table (make-hash-table :test 'equal))

(defvar twittering-image-stack nil)
(defvar twittering-image-type-cache nil)
(defvar twittering-convert-program (executable-find "convert"))
(defvar twittering-convert-fix-size 48)
(defvar twittering-use-convert (not (null twittering-convert-program))
  "*This variable makes a sense only if `twittering-convert-fix-size'
is non-nil. If this variable is non-nil, icon images are converted by
invoking \"convert\". Otherwise, cropped images are displayed.")

(defun twittering-image-type (image-url buffer)
  "Return the type of a given image based on the URL (IMAGE-URL)
and its contents (BUFFER)"
  (let ((type-cache (assoc image-url twittering-image-type-cache))
	(case-fold-search t))
    (if type-cache
	(cdr type-cache)
      (let ((image-type (image-type-from-data (buffer-string))))
	(add-to-list 'twittering-image-type-cache `(,image-url . ,image-type))
	image-type))))

;;;
;;; Utility functions
;;;

(defun twittering-get-or-generate-buffer (buffer)
  (if (bufferp buffer)
      (if (buffer-live-p buffer)
	  buffer
	(generate-new-buffer (buffer-name buffer)))
    (if (stringp buffer)
	(or (get-buffer buffer)
	    (generate-new-buffer buffer)))))

(defun twittering-get-status-url (username &optional id)
  "Generate status URL."
  (if id
      (format "http://%s/%s/status/%s" twittering-web-host username id)
    (format "http://%s/%s" twittering-web-host username)))

(defun twittering-get-search-url (query-string)
  (format "http://%s/search?q=%s"
	  twittering-web-host (twittering-percent-encode query-string)))

(defun twittering-user-agent-default-function ()
  "Twittering mode default User-Agent function."
  (format "Emacs/%d.%d Twittering-mode/%s"
	  emacs-major-version emacs-minor-version
	  twittering-mode-version))

(defvar twittering-sign-simple-string nil)

(defun twittering-sign-string-default-function ()
  "Append sign string to tweet."
  (if twittering-sign-simple-string
      (format " [%s]" twittering-sign-simple-string)
    ""))

(defvar twittering-user-agent-function 'twittering-user-agent-default-function)
(defvar twittering-sign-string-function 'twittering-sign-string-default-function)

(defun twittering-user-agent ()
  "Return User-Agent header string."
  (funcall twittering-user-agent-function))

(defun twittering-sign-string ()
  "Return Tweet sign string."
  (funcall twittering-sign-string-function))

(defvar twittering-mode-string "twittering-mode")

(defun twittering-update-mode-line ()
  "Update mode line."
  (let ((enabled-options
	 `(,@(unless (twittering-buffer-active-p) '("INACTIVE"))
	   ,@(when twittering-jojo-mode '("jojo"))
	   ,@(when twittering-icon-mode '("icon"))
	   ,@(when twittering-reverse-mode '("reverse"))
	   ,@(when twittering-scroll-mode '("scroll"))
	   ,@(when twittering-proxy-use '("proxy"))
	   ,@(when twittering-use-ssl '("ssl")))))
    (setq mode-name
	  (concat twittering-mode-string
		  (if twittering-display-remaining
		      (format " %d/%d"
			      (twittering-get-ratelimit-remaining)
			      (twittering-get-ratelimit-limit))
		    "")
		  (if enabled-options
		      (concat "["
			      (mapconcat 'identity enabled-options ",")
			      "]")
		    ""))))
  (force-mode-line-update)
  )

(defun twittering-status-id< (id1 id2)
  (let ((len1 (length id1))
	(len2 (length id2)))
    (cond
     ((= len1 len2) (string< id1 id2))
     ((< len1 len2) t)
     (t nil))))

(defun twittering-status-id= (id1 id2)
  (equal id1 id2))

(defun twittering-fill-string (str &optional adjustment)
  (when (and (not (boundp 'kinsoku-limit))
	     enable-kinsoku)
    ;; `kinsoku-limit' is defined on loading "international/kinsoku.el".
    ;; Without preloading, "kinsoku.el" will be loaded by auto-loading
    ;; triggered by `fill-region-as-paragraph'.
    ;; In that case, the local binding of `kinsoku-limit' conflicts the
    ;; definition by `defvar' in "kinsoku.el".
    ;; The below warning is displayed;
    ;; "Warning: defvar ignored because kinsoku-limit is let-bound".
    ;; So, we load "kinsoku.el" in advance if necessary.
    (load "international/kinsoku"))
  (let* ((kinsoku-limit 1)
	 (adjustment (+ (or adjustment 0)
			(if (and (boundp 'fill-prefix) (stringp fill-prefix))
			    (string-width fill-prefix)
			  0)
			(if enable-kinsoku
			    kinsoku-limit
			  0)))
	 (min-width
	  (apply 'min
		 (or
		  (mapcar 'window-width
			  (get-buffer-window-list (current-buffer) nil t))
		  ;; Use `(frame-width)' if no windows display
		  ;; the current buffer.
		  `(,(frame-width)))))
	 (temporary-fill-column (- (or twittering-fill-column (1- min-width))
				   adjustment)))
    (with-temp-buffer
      (let ((fill-column temporary-fill-column))
	(insert str)
	(fill-region-as-paragraph (point-min) (point-max))
	(buffer-substring (point-min) (point-max))))))

(defun twittering-set-window-end (window pos)
  (let* ((height (window-text-height window))
         (n (- (- height 1))))
    (while (progn (setq n (1+ n))
		  (set-window-start
		   window
		   (with-current-buffer (window-buffer window)
		     (save-excursion
		       (goto-char pos)
		       (line-beginning-position n))))
		  (not (pos-visible-in-window-p pos window))))))

(defun twittering-make-passed-time-string
  (beg end encoded-created-at &optional additional-properties)
  (let* ((now (current-time))
	 (secs (+ (* (- (car now) (car encoded-created-at)) 65536)
		  (- (cadr now) (cadr encoded-created-at))))
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
				  encoded-created-at))))
	 (properties (append additional-properties
			     (and beg (text-properties-at beg)))))
    ;; Restore properties.
    (when properties
      (add-text-properties 0 (length time-string) properties time-string))
    (if (< secs 84600)
	(put-text-property 0 (length time-string)
			   'need-to-be-updated
			   `(twittering-make-passed-time-string
			     ,encoded-created-at)
			   time-string)
      ;; Remove the property required no longer.
      (remove-text-properties 0 (length time-string) '(need-to-be-updated nil)
			      time-string))
    time-string))

(defun twittering-update-filled-string (beg end formater status prefix)
  (let* ((str (twittering-fill-string (funcall formater status prefix)
				      (length prefix)))
	 (next (next-single-property-change 0 'need-to-be-updated str))
	 (properties
	  (and beg
	       (apply 'append
		      (mapcar (lambda (prop)
				(let ((value (get-text-property beg prop)))
				  (when value
				    `(,prop ,value))))
			      '(id text username))))))
    ;; Restore properties.
    (when properties
      (add-text-properties 0 (length str) properties str))
    (if (or (get-text-property 0 'need-to-be-updated str)
	    (and next (< next (length str))))
	(put-text-property 0 (length str) 'need-to-be-updated
			   `(twittering-update-filled-string
			     ,formater ,status ,prefix)
			   str)
      ;; Remove the property required no longer.
      (remove-text-properties 0 (length str) '(need-to-be-updated nil) str))
    str))

;;;
;;; Utility functions for portability
;;;

(defun twittering-ucs-to-char (num)
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
      (ucs-to-char num)
    ;; Emacs21 have a partial support for UTF-8 text, so it can decode
    ;; only parts of a text with Japanese.
    (or (decode-char 'ucs num)
	??)))

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

(eval-when-compile ;; shut up the byte compiler.
  (defvar twittering-debug-buffer)
  (defvar twittering-edit-buffer))

(defun twittering-buffer-related-p ()
  "Return t if current buffer is twittering-mode related buffer."
  (or (twittering-buffer-p)
      (member (buffer-name (current-buffer))
	      (list twittering-debug-buffer
		    twittering-edit-buffer))))

(defun assocref (item alist)
  (cdr (assoc item alist)))

(defmacro list-push (value listvar)
  `(setq ,listvar (cons ,value ,listvar)))

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

(defvar twittering-regexp-hash
  (let ((full-width-number-sign (twittering-ucs-to-char #xff03)))
    ;; Unicode Character 'FULLWIDTH NUMBER SIGN' (U+FF03)
    (concat "\\(?:#\\|" (char-to-string full-width-number-sign) "\\)")))

(defvar twittering-regexp-atmark
  (let ((full-width-commercial-at (twittering-ucs-to-char #xff20)))
    ;; Unicode Character 'FULLWIDTH COMMERCIAL AT' (U+FF20)
    (concat "\\(?:@\\|" (char-to-string full-width-commercial-at) "\\)")))

;;;
;;; Timeline spec functions
;;;

;;; Timeline spec as S-expression
;;; - (user USER): timeline of the user whose name is USER. USER is a string.
;;; - (list USER LIST):
;;;     the list LIST of the user USER. LIST and USER are strings.
;;;
;;; - (direct_messages): received direct messages.
;;; - (direct_messages_sent): sent direct messages.
;;; - (friends): friends timeline.
;;; - (home): home timeline.
;;; - (mentions): mentions timeline.
;;;     mentions (status containing @username) for the authenticating user.
;;; - (public): public timeline.
;;; - (replies): replies.
;;; - (retweeted_by_me): retweets posted by the authenticating user.
;;; - (retweeted_to_me): retweets posted by the authenticating user's friends.
;;; - (retweets_of_me):
;;;     tweets of the authenticated user that have been retweeted by others.
;;;
;;; - (search STRING): the result of searching with query STRING.
;;; - (merge SPEC1 SPEC2 ...): result of merging timelines SPEC1 SPEC2 ...
;;; - (filter REGEXP SPEC): timeline filtered with REGEXP.
;;;

;;; Timeline spec string
;;;
;;; SPEC ::= PRIMARY | COMPOSITE
;;; PRIMARY ::= USER | LIST | DIRECT_MESSSAGES | DIRECT_MESSSAGES_SENT
;;;             | FRIENDS | HOME | MENTIONS | PUBLIC | REPLIES
;;;             | RETWEETED_BY_ME | RETWEETED_TO_ME | RETWEETS_OF_ME
;;;             | SEARCH
;;; COMPOSITE ::= MERGE | FILTER
;;;
;;; USER ::= /[a-zA-Z0-9_-]+/
;;; LIST ::= USER "/" LISTNAME
;;; LISTNAME ::= /[a-zA-Z0-9_-]+/
;;; DIRECT_MESSSAGES ::= ":direct_messages"
;;; DIRECT_MESSSAGES_SENT ::= ":direct_messages_sent"
;;; FRIENDS ::= ":friends"
;;; HOME ::= ":home" | "~"
;;; MENTIONS ::= ":mentions"
;;; PUBLIC ::= ":public"
;;; REPLIES ::= ":replies" | "@"
;;; RETWEETED_BY_ME ::= ":retweeted_by_me"
;;; RETWEETED_TO_ME ::= ":retweeted_to_me"
;;; RETWEETS_OF_ME ::= ":retweets_of_me"
;;;
;;; SEARCH ::= ":search/" QUERY_STRING "/"
;;; QUERY_STRING ::= any string, where "/" is escaped by a backslash.
;;; MERGE ::= "(" MERGED_SPECS ")"
;;; MERGED_SPECS ::= SPEC | SPEC "+" MERGED_SPECS
;;; FILTER ::= ":filter/" REGEXP "/" SPEC
;;;

(defun twittering-timeline-spec-to-string (timeline-spec &optional shorten)
  "Convert TIMELINE-SPEC into a string.
If SHORTEN is non-nil, the abbreviated expression will be used."
  (let ((type (car timeline-spec))
	(value (cdr timeline-spec)))
    (cond
     ;; user
     ((eq type 'user) (car value))
     ;; list
     ((eq type 'list) (concat (car value) "/" (cadr value)))
     ;; simple
     ((eq type 'direct_messages) ":direct_messages")
     ((eq type 'direct_messages_sent) ":direct_messages_sent")
     ((eq type 'friends) ":friends")
     ((eq type 'home) (if shorten "~" ":home"))
     ((eq type 'mentions) ":mentions")
     ((eq type 'public) ":public")
     ((eq type 'replies) (if shorten "@" ":replies"))
     ((eq type 'retweeted_by_me) ":retweeted_by_me")
     ((eq type 'retweeted_to_me) ":retweeted_to_me")
     ((eq type 'retweets_of_me) ":retweets_of_me")
     ((eq type 'search)
      (let ((query (car value)))
	(concat ":search/"
		(replace-regexp-in-string "/" "\\/" query nil t)
		"/")))
     ;; composite
     ((eq type 'filter)
      (let ((regexp (car value))
	    (spec (cadr value)))
	(concat ":filter/"
		(replace-regexp-in-string "/" "\\/" regexp nil t)
		"/"
		(twittering-timeline-spec-to-string spec))))
     ((eq type 'merge)
      (concat "("
	      (mapconcat 'twittering-timeline-spec-to-string value "+")
	      ")"))
     (t
      nil))))

(defun twittering-extract-timeline-spec (str &optional unresolved-aliases)
  "Extract one timeline spec from STR.
Return cons of the spec and the rest string."
  (cond
   ((null str)
    (error "STR is nil")
    nil)
   ((string-match "^\\([a-zA-Z0-9_-]+\\)/\\([a-zA-Z0-9_-]+\\)" str)
    (let ((user (match-string 1 str))
	  (listname (match-string 2 str))
	  (rest (substring str (match-end 0))))
      `((list ,user ,listname) . ,rest)))
   ((string-match "^\\([a-zA-Z0-9_-]+\\)" str)
    (let ((user (match-string 1 str))
	  (rest (substring str (match-end 0))))
      `((user ,user) . ,rest)))
   ((string-match "^~" str)
    `((home) . ,(substring str (match-end 0))))
   ((string-match (concat "^" twittering-regexp-atmark) str)
    `((replies) . ,(substring str (match-end 0))))
   ((string-match (concat "^" twittering-regexp-hash "\\([a-zA-Z0-9_-]+\\)")
		  str)
    (let* ((tag (match-string 1 str))
	   (query (concat "#" tag))
	   (rest (substring str (match-end 0))))
      `((search ,query) . ,rest)))
   ((string-match "^:\\([a-z_-]+\\)" str)
    (let ((type (match-string 1 str))
	  (following (substring str (match-end 0)))
	  (alist '(("direct_messages" . direct_messages)
		   ("direct_messages_sent" . direct_messages_sent)
		   ("friends" . friends)
		   ("home" . home)
		   ("mentions" . mentions)
		   ("public" . public)
		   ("replies" . replies)
		   ("retweeted_by_me" . retweeted_by_me)
		   ("retweeted_to_me" . retweeted_to_me)
		   ("retweets_of_me" . retweets_of_me))))
      (cond
       ((assoc type alist)
	(let ((first-spec (list (cdr (assoc type alist)))))
	  (cons first-spec following)))
       ((string= type "search")
	(if (string-match "^:search/\\(\\(.*?[^\\]\\)??\\(\\\\\\\\\\)*\\)??/"
			  str)
	    (let* ((escaped-query (or (match-string 1 str) ""))
		   (query (replace-regexp-in-string "\\\\/" "/"
						    escaped-query nil t))
		   (rest (substring str (match-end 0))))
	      (if (not (string= "" escaped-query))
		  `((search ,query) . ,rest)
		(error "\"%s\" has no valid regexp" str)
		nil))))
       ((string= type "filter")
	(if (string-match "^:filter/\\(\\(.*?[^\\]\\)??\\(\\\\\\\\\\)*\\)??/"
			  str)
	    (let* ((escaped-regexp (or (match-string 1 str) ""))
		   (regexp (replace-regexp-in-string "\\\\/" "/"
						     escaped-regexp nil t))
		   (following (substring str (match-end 0)))
		   (pair (twittering-extract-timeline-spec
			  following unresolved-aliases))
		   (spec (car pair))
		   (rest (cdr pair)))
	      `((filter ,regexp ,spec) . ,rest))
	  (error "\"%s\" has no valid regexp" str)
	  nil))
       (t
	(error "\"%s\" is invalid as a timeline spec" str)
	nil))))
   ((string-match "^\\$\\([a-zA-Z0-9_-]+\\)\\(?:(\\([^)]*\\))\\)?" str)
    (let* ((name (match-string 1 str))
	   (rest (substring str (match-end 0)))
	   (value (cdr-safe (assoc name twittering-timeline-spec-alias)))
	   (arg (match-string 2 str)))
      (if (member name unresolved-aliases)
	  (error "Alias \"%s\" includes a recursive reference" name)
	(cond
	 ((stringp value)
	  (twittering-extract-timeline-spec
	   (concat value rest)
	   (cons name unresolved-aliases)))
	 ((functionp value)
	  (twittering-extract-timeline-spec
	   (funcall value arg)
	   (cons name unresolved-aliases)))
	 (t
	  (error "Alias \"%s\" is undefined" name))))))
   ((string-match "^(" str)
    (let ((rest (concat "+" (substring str (match-end 0))))
	  (result '()))
      (while (and rest (string-match "^\\+" rest))
	(let* ((spec-string (substring rest (match-end 0)))
	       (pair (twittering-extract-timeline-spec
		      spec-string unresolved-aliases))
	       (spec (car pair))
	       (next-rest (cdr pair)))
	  (setq result (cons spec result))
	  (setq rest next-rest)))
      (if (and rest (string-match "^)" rest))
	  (let ((spec-list
		 (apply 'append
			(mapcar (lambda (x) (if (eq 'merge (car x))
						(cdr x)
					      (list x)))
				(reverse result)))))
	    (if (= 1 (length spec-list))
		`(,(car spec-list) . ,(substring rest 1))
	      `((merge ,@spec-list) . ,(substring rest 1))))
	(if rest
	    ;; The string following the opening parenthesis `('
	    ;; can be interpreted without errors,
	    ;; but there is no corresponding closing parenthesis.
	    (error "\"%s\" lacks a closing parenthesis" str))
	;; Does not display additional error messages if an error
	;; occurred on interpreting the string following
	;; the opening parenthesis `('.
	nil)))
   (t
    (error "\"%s\" is invalid as a timeline spec" str)
    nil)
   ))

(defun twittering-string-to-timeline-spec (spec-str)
  "Convert SPEC-STR into a timeline spec.
Return nil if SPEC-STR is invalid as a timeline spec."
  (let ((result-pair (twittering-extract-timeline-spec spec-str)))
    (if (and result-pair (string= "" (cdr result-pair)))
	(car result-pair)
      nil)))

(defun twittering-timeline-spec-primary-p (spec)
  "Return non-nil if SPEC is a primary timeline spec.
`primary' means that the spec is not a composite timeline spec such as
`filter' and `merge'."
  (let ((primary-spec-types
	 '(user list
		direct_messages direct_messages_sent
		friends home mentions public replies
		search
		retweeted_by_me retweeted_to_me retweets_of_me))
	(type (car spec)))
    (memq type primary-spec-types)))

(defun twittering-timeline-spec-is-direct-messages-p (spec)
  "Return non-nil if SPEC is a timeline spec which is related of
direct_messages."
  (and spec
       (memq (car spec) '(direct_messages direct_messages_sent))))

(defun twittering-equal-string-as-timeline (spec-str1 spec-str2)
  "Return non-nil if SPEC-STR1 equals SPEC-STR2 as a timeline spec."
  (if (and (stringp spec-str1) (stringp spec-str2))
      (let ((spec1 (twittering-string-to-timeline-spec spec-str1))
	    (spec2 (twittering-string-to-timeline-spec spec-str2)))
	(equal spec1 spec2))
    nil))

(defun twittering-timeline-spec-to-host-method (spec)
  (if (twittering-timeline-spec-primary-p spec)
      (let ((api-host twittering-api-host)
	    (search-host twittering-api-search-host)
	    (type (car spec))
	    (value (cdr spec)))
	(cond
	 ((eq type 'user)
	  (let ((username (car value)))
	    `(,api-host ,(concat "1/statuses/user_timeline/" username))))
	 ((eq type 'list)
	  (let ((username (car value))
		(list-name (cadr value)))
	    `(,api-host
	      ,(concat "1/" username "/lists/" list-name "/statuses"))))
	 ((eq type 'direct_messages)
	  `(,api-host "1/direct_messages"))
	 ((eq type 'direct_messages_sent)
	  `(,api-host "1/direct_messages/sent"))
	 ((eq type 'friends)
	  `(,api-host "1/statuses/friends_timeline"))
	 ((eq type 'home)
	  `(,api-host "1/statuses/home_timeline"))
	 ((eq type 'mentions)
	  `(,api-host "1/statuses/mentions"))
	 ((eq type 'public)
	  `(,api-host "1/statuses/public_timeline"))
	 ((eq type 'replies)
	  `(,api-host "1/statuses/replies"))
	 ((eq type 'retweeted_by_me)
	  `(,api-host "1/statuses/retweeted_by_me"))
	 ((eq type 'retweeted_to_me)
	  `(,api-host "1/statuses/retweeted_to_me"))
	 ((eq type 'retweets_of_me)
	  `(,api-host "1/statuses/retweets_of_me"))
	 ((eq type 'search)
	  (let ((word (car value)))
	    `(,search-host "search" ,word)))
	 (t
	  (error "Invalid timeline spec")
	  nil)))
    nil))

(defun twittering-host-method-to-timeline-spec (host method &optional word)
  (cond
   ((or (not (stringp host)) (not (stringp method))) nil)
   ((string= host twittering-api-host)
    (cond
     ((string= method "1/statuses/direct_messages") '(direct_messages))
     ((string= method "1/statuses/direct_messages/sent")
      '(direct_messages_sent))
     ((string= method "1/statuses/public_timeline") '(public_timeline))
     ((string= method "1/statuses/home_timeline") '(home))
     ((string= method "1/statuses/friends_timeline") '(friends))
     ((string= method "1/statuses/user_timeline")
      `(user ,(twittering-get-username)))
     ((string-match "^1/statuses/user_timeline/\\(.+\\)$" method)
      `(user ,(match-string-no-properties 1 method)))
     ((string= method "1/statuses/mentions") '(mentions))
     ((string= method "1/statuses/replies") '(replies))
     ((string= method "1/statuses/retweeted_by_me") '(retweeted_by_me))
     ((string= method "1/statuses/retweeted_to_me") '(retweeted_to_me))
     ((string= method "1/statuses/retweets_of_me") '(retweets_of_me))
     ((string-match "^1/\\([^/]+\\)/lists/\\([^/]+\\)/statuses" method)
      (let ((username (match-string-no-properties 1 method))
	    (listname (match-string-no-properties 2 method)))
	`(list ,username ,listname)))
     (t nil)))
   ((string= host twittering-api-search-host)
    `(search ,word))
   (t nil)))

(defun twittering-add-timeline-history (spec-string)
  (when (or (null twittering-timeline-history)
	    (not (string= spec-string (car twittering-timeline-history))))
    (if (functionp 'add-to-history)
	(add-to-history 'twittering-timeline-history spec-string)
      (setq twittering-timeline-history
	    (cons spec-string twittering-timeline-history)))))

;;;
;;; Timeline info
;;;

(defun twittering-current-timeline-id-table (&optional spec)
  (let ((spec (or spec (twittering-current-timeline-spec))))
    (if spec
	(elt (gethash spec twittering-timeline-data-table) 0)
      nil)))

(defun twittering-current-timeline-referring-id-table (&optional spec)
  "Return the hash from a ID to the ID of the first observed status
referring the former ID."
  (let ((spec (or spec (twittering-current-timeline-spec))))
    (if spec
	(elt (gethash spec twittering-timeline-data-table) 1)
      nil)))

(defun twittering-current-timeline-data (&optional spec)
  (let ((spec (or spec (twittering-current-timeline-spec))))
    (if spec
	(elt (gethash spec twittering-timeline-data-table) 2)
      nil)))

(defun twittering-remove-timeline-data (&optional spec)
  (let ((spec (or spec (twittering-current-timeline-spec))))
    (remhash spec twittering-timeline-data-table)))

(defun twittering-find-status (id)
  (let ((result nil))
    (maphash
     (lambda (spec pair)
       (let* ((id-table (car pair))
	      (entry (gethash id id-table)))
	 ;; Take the most detailed status.
	 (when (and entry
		    (or (null result) (< (length result) (length entry))))
	   (setq result entry))))
     twittering-timeline-data-table)
    result))

(defun twittering-get-replied-statuses (id &optional count)
  "Return a list of replied statuses starting from the status specified by ID.
Statuses are stored in ascending-order with respect to their IDs."
  (let ((result nil)
	(status (twittering-find-status id)))
    (while
	(and (if (numberp count)
		 (<= 0 (setq count (1- count)))
	       t)
	     (let ((replied-id (or (cdr (assq 'in-reply-to-status-id status))
				   "")))
	       (unless (string= "" replied-id)
		 (let ((replied-status (twittering-find-status replied-id)))
		   (when replied-status
		     (setq result (cons replied-status result))
		     (setq status replied-status)
		     t))))))
    result))

(defun twittering-have-replied-statuses-p (id)
  (let ((status (twittering-find-status id)))
    (when status
      (let ((replied-id (cdr (assq 'in-reply-to-status-id status))))
	(and replied-id (not (string= "" replied-id)))))))

(defun twittering-add-statuses-to-timeline-data (statuses &optional spec)
  (let* ((spec (or spec (twittering-current-timeline-spec)))
	 (id-table
	  (or (twittering-current-timeline-id-table spec)
	      (make-hash-table :test 'equal)))
	 (referring-id-table
	  (or (twittering-current-timeline-referring-id-table spec)
	      (make-hash-table :test 'equal)))
	 (timeline-data (twittering-current-timeline-data spec)))
    (let ((new-statuses
	   (remove nil
		   (mapcar
		    (lambda (status)
		      (let ((id (cdr (assq 'id status)))
			    (source-id (cdr-safe (assq 'source-id status))))
			(unless (or (not source-id)
				    (gethash source-id referring-id-table))
			  ;; Store the id of the first observed tweet
			  ;; that refers `source-id'.
			  (puthash source-id id referring-id-table))
			(if (gethash id id-table)
			    nil
			  (puthash id status id-table)
			  (puthash id id referring-id-table)
			  status)))
		    statuses))))
      (when new-statuses
	(let ((new-timeline-data
	       (sort (append new-statuses timeline-data)
		     (lambda (status1 status2)
		       (let ((id1 (cdr (assq 'id status1)))
			     (id2 (cdr (assq 'id status2))))
			 (twittering-status-id< id2 id1))))))
	  (puthash spec `(,id-table ,referring-id-table ,new-timeline-data)
		   twittering-timeline-data-table))
	(when twittering-jojo-mode
	  (mapc (lambda (status)
		  (twittering-update-jojo (cdr (assq 'user-screen-name status))
					  (cdr (assq 'text status))))
		new-statuses))
	(let ((twittering-new-tweets-spec spec)
	      (twittering-new-tweets-count (length new-statuses)))
	  (run-hooks 'twittering-new-tweets-hook))
	new-statuses))))

;;;
;;; Process info
;;;

(defun twittering-register-process (proc spec &optional str)
  (let ((str (or str (twittering-timeline-spec-to-string spec))))
    (add-to-list 'twittering-process-info-alist `(,proc ,spec ,str))))

(defun twittering-release-process (proc)
  (let ((pair (assoc proc twittering-process-info-alist)))
    (when pair
      (setq twittering-process-info-alist
	    (delq pair twittering-process-info-alist)))))

(defun twittering-get-timeline-spec-from-process (proc)
  (let ((entry (assoc proc twittering-process-info-alist)))
    (if entry
	(elt entry 1)
      nil)))

(defun twittering-get-timeline-spec-string-from-process (proc)
  (let ((entry (assoc proc twittering-process-info-alist)))
    (if entry
	(elt entry 2)
      nil)))

(defun twittering-find-processes-for-timeline-spec (spec)
  (apply 'append
	 (mapcar
	  (lambda (pair)
	    (let ((proc (car pair))
		  (spec-info (cadr pair)))
	      (if (equal spec-info spec)
		  `(,proc)
		nil)))
	  twittering-process-info-alist)))

(defun twittering-remove-inactive-processes ()
  (let ((inactive-statuses '(nil closed exit failed signal)))
    (setq twittering-process-info-alist
	  (apply 'append
		 (mapcar
		  (lambda (pair)
		    (let* ((proc (car pair))
			   (info (cdr pair))
			   (status (process-status proc)))
		      (if (memq status inactive-statuses)
			  nil
			`((,proc ,@info)))))
		  twittering-process-info-alist)))))

(defun twittering-process-active-p (&optional spec)
  (twittering-remove-inactive-processes)
  (if spec
      (twittering-find-processes-for-timeline-spec spec)
    twittering-process-info-alist))

;;;
;;; Server info
;;;

(defun twittering-make-header-info-alist (header-str)
  "Make HTTP header alist from HEADER-STR.
The alist consists of pairs of field-name and field-value, such as
'((\"Content-Type\" . \"application/xml\; charset=utf-8\")
  (\"Content-Length\" . \"2075\"))."
  (let* ((lines (split-string header-str "\r?\n"))
         (status-line (car lines))
         (header-lines (cdr lines)))
    (when (string-match
	   "^\\(HTTP/1\.[01]\\) \\([0-9][0-9][0-9]\\) \\(.*\\)$"
	   status-line)
      (append `((status-line . ,status-line)
		(http-version . ,(match-string 1 status-line))
		(status-code . ,(match-string 2 status-line))
		(reason-phrase . ,(match-string 3 status-line)))
	      (remove nil
		      (mapcar
		       (lambda (line)
			 (when (string-match "^\\([^: ]*\\): *\\(.*\\)$" line)
			   (cons (match-string 1 line) (match-string 2 line))))
		       header-lines))))))

(defun twittering-update-server-info (header-str)
  (let* ((header-info (twittering-make-header-info-alist header-str))
	 (new-entry-list (mapcar 'car header-info)))
    (when (remove t (mapcar
		     (lambda (entry)
		       (equal (assoc entry header-info)
			      (assoc entry twittering-server-info-alist)))
		     new-entry-list))
      (setq twittering-server-info-alist
	    (append header-info
		    (remove nil (mapcar
				 (lambda (entry)
				   (if (member (car entry) new-entry-list)
				       nil
				     entry))
				 twittering-server-info-alist))))
      (when twittering-display-remaining
	(mapc (lambda (buffer)
		(with-current-buffer buffer
		  (twittering-update-mode-line)))
	      (twittering-get-buffer-list))))
    header-info))

(defun twittering-get-server-info (field)
  (let* ((table
	  '((ratelimit-remaining . "X-RateLimit-Remaining")
	    (ratelimit-limit . "X-RateLimit-Limit")
	    (ratelimit-reset . "X-RateLimit-Reset")))
	 (numeral-field '(ratelimit-remaining ratelimit-limit))
	 (unix-epoch-time-field '(ratelimit-reset))
	 (field-name (cdr (assq field table)))
	 (field-value (cdr (assoc field-name twittering-server-info-alist))))
    (when (and field-name field-value)
      (cond
       ((memq field numeral-field)
	(string-to-number field-value))
       ((memq field unix-epoch-time-field)
	(seconds-to-time (string-to-number (concat field-value ".0"))))
       (t
	nil)))))

(defun twittering-get-ratelimit-remaining ()
  (or (twittering-get-server-info 'ratelimit-remaining)
      0))

(defun twittering-get-ratelimit-limit ()
  (or (twittering-get-server-info 'ratelimit-limit)
      0))

;;;
;;; Buffer info
;;;

(defvar twittering-buffer-info-list nil
  "List of (buffer timeline-spec timeline-spec-string active-flag).")

(defun twittering-get-buffer-list ()
  "Return buffers managed by `twittering-mode'."
  (twittering-unregister-killed-buffer)
  (mapcar 'car twittering-buffer-info-list))

(defun twittering-get-active-buffer-list ()
  "Return active buffers managed by `twittering-mode', where statuses are
retrieved periodically."
  (twittering-unregister-killed-buffer)
  (remove nil (mapcar (lambda (entry) (when (elt entry 3) (car entry)))
		      twittering-buffer-info-list)))

(defun twittering-buffer-p (&optional buffer)
  "Return t if BUFFER is managed by `twittering-mode'.
BUFFER defaults to the the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (assq buffer twittering-buffer-info-list)))

(defun twittering-buffer-active-p (&optional buffer)
  "Return t if BUFFER is an active buffer managed by `twittering-mode'.
BUFFER defaults to the the current buffer."
  (let* ((buffer (or buffer (current-buffer)))
	 (entry (assq buffer twittering-buffer-info-list)))
    (and entry (elt entry 3))))

(defun twittering-get-buffer-from-spec (spec)
  "Return the buffer bound to SPEC. If no buffers are bound to SPEC,
return nil."
  (let* ((spec-string (twittering-timeline-spec-to-string spec))
	 (spec ;; normalized spec
	  (twittering-string-to-timeline-spec spec-string))
	 (rest twittering-buffer-info-list))
    (while (and rest (not (equal spec (elt (car rest) 1))))
      (setq rest (cdr rest)))
    (if rest
	(elt (car rest) 0)
      nil)))

(defun twittering-get-buffer-from-spec-string (spec-string)
  "Return the buffer bound to SPEC-STRING. If no buffers are bound to it,
return nil."
  (let ((spec (twittering-string-to-timeline-spec spec-string)))
    (and spec (twittering-get-buffer-from-spec spec))))

(defun twittering-get-timeline-spec-for-buffer (buffer)
  "Return the timeline spec bound to BUFFER. If BUFFER is not managed by
`twittering-mode', return nil."
  (elt (assq buffer twittering-buffer-info-list) 1))

(defun twittering-get-timeline-spec-string-for-buffer (buffer)
  "Return the timeline spec string bound to BUFFER. If BUFFER is not managed
by `twittering-mode', return nil."
  (elt (assq buffer twittering-buffer-info-list) 2))

(defun twittering-current-timeline-spec ()
  "Return the timeline spec bound to the current buffer. If it is not managed
by `twittering-mode', return nil."
  (twittering-get-timeline-spec-for-buffer (current-buffer)))

(defun twittering-current-timeline-spec-string ()
  "Return the timeline spec string bound to the current buffer. If it is not
managed by `twittering-mode', return nil."
  (twittering-get-timeline-spec-string-for-buffer (current-buffer)))

(defun twittering-register-buffer (buffer spec-string &optional inactive)
  "Register BUFFER to `twittering-buffer-info-list' as a buffer managed
by `twittering-mode'. It is bound to SPEC-STRING. If INACTIVE is non-nil,
BUFFER is regsitered as an inactive buffer.
If BUFFER is the first managed buffer, call `twittering-start' to start
timers."
  (let ((spec (twittering-string-to-timeline-spec spec-string))
	(buffer-list (twittering-get-active-buffer-list)))
    (setq twittering-buffer-info-list
	  (cons `(,buffer ,spec ,spec-string ,(not inactive))
		twittering-buffer-info-list))
    (when (null buffer-list)
      (twittering-start))))

(defun twittering-unregister-buffer (buffer &optional keep-timer)
  "Unregister BUFFER from `twittering-buffer-info-list'.
If BUFFER is the last managed buffer and KEEP-TIMER is nil, call
`twittering-stop' to stop timers."
  (let ((entry (assq buffer twittering-buffer-info-list)))
    (when entry
      (setq twittering-buffer-info-list
	    (delq entry twittering-buffer-info-list))
      (when (null twittering-buffer-info-list)
	(twittering-stop)))))

(defun twittering-unregister-killed-buffer ()
  "Unregister buffers which has been killed."
  (mapc (lambda (entry)
	  (let ((buffer (car entry)))
	    (unless (buffer-live-p buffer)
	      (twittering-unregister-buffer buffer))))
	twittering-buffer-info-list))

(defun twittering-replace-spec-string-for-buffer (buffer spec-string)
  "Replace the timeline spec string for BUFFER with SPEC-STRING when
BUFFER is managed by `twittering-mode' and SPEC-STRING is equivalent
to the current one."
  (when (twittering-buffer-p buffer)
    (let ((current (twittering-get-timeline-spec-string-for-buffer buffer)))
      (when (and (not (string= current spec-string))
		 (twittering-equal-string-as-timeline current spec-string))
	(twittering-unregister-buffer buffer)
	(with-current-buffer buffer
	  (rename-buffer spec-string t))
	(twittering-register-buffer buffer spec-string)))))

(defun twittering-set-active-flag-for-buffer (buffer active)
  "Set ACTIVE to active-flag for BUFFER."
  (when (twittering-buffer-p buffer)
    (let ((current (twittering-buffer-active-p buffer)))
      (when (or (and active (not current))
		(and (not active) current))
	(let ((spec-string
	       (twittering-get-timeline-spec-string-for-buffer buffer)))
	  (twittering-unregister-buffer buffer t)
	  (twittering-register-buffer buffer spec-string (not active))
	  (with-current-buffer buffer
	    (twittering-update-mode-line)))))))

(defun twittering-toggle-activate-buffer ()
  "Toggle whether to retrieve timeline for the current buffer periodically."
  (interactive)
  (when (twittering-buffer-p)
    (cond
     ((twittering-buffer-active-p)
      (twittering-deactivate-buffer))
     (t
      (twittering-activate-buffer)))))

(defun twittering-activate-buffer (&optional buffer)
  "Activate BUFFER to retrieve timeline for it periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (twittering-set-active-flag-for-buffer buffer t)))

(defun twittering-deactivate-buffer (&optional buffer)
  "Deactivate BUFFER not to retrieve timeline for it periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (twittering-set-active-flag-for-buffer buffer nil)))

(defun twittering-kill-buffer (&optional buffer)
  "Kill BUFFER managed by `twittering-mode'."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (twittering-buffer-p buffer)
      (twittering-deactivate-buffer buffer)
      (kill-buffer buffer)
      (twittering-unregister-killed-buffer))))

(defun twittering-get-managed-buffer (spec)
  "Return the buffer bound to SPEC. If no buffers are bound to SPEC, return
newly generated buffer.
SPEC may be a timeline spec or a timeline spec string."
  (let* ((original-spec spec)
	 (spec-string (if (stringp spec)
			  spec
			(twittering-timeline-spec-to-string spec)))
	 ;; `spec-string' without text properites is required because
	 ;; Emacs21 displays `spec-string' with its properties on mode-line.
	 ;; In addition, copying `spec-string' keeps timeline-data from
	 ;; being modified by `minibuf-isearch.el'.
	 (spec-string (copy-sequence spec-string))
	 (spec (if (stringp spec-string)
		   (twittering-string-to-timeline-spec spec-string)
		 nil)))
    (when (null spec)
      (error "\"%s\" is invalid as a timeline spec"
	     (or spec-string original-spec)))
    (set-text-properties 0 (length spec-string) nil spec-string)
    (let ((buffer (twittering-get-buffer-from-spec spec)))
      (if buffer
	  (progn
	    (twittering-replace-spec-string-for-buffer buffer spec-string)
	    (twittering-render-timeline buffer t)
	    buffer)
	(let ((buffer (generate-new-buffer spec-string)))
	  (with-current-buffer buffer
	    (twittering-mode-setup)
	    (twittering-register-buffer buffer spec-string)
	    (twittering-render-timeline buffer)
	    (twittering-get-and-render-timeline))
	  buffer)))))

;;;
;;; Unread statuses info
;;;

(defvar twittering-unread-status-info nil
  "A list of (buffer unread-statuses-counter), where `unread-statuses-counter'
means the number of statuses retrieved after the last visiting of the buffer.")

(defun twittering-reset-unread-status-info-if-necessary ()
  (when (twittering-buffer-p)
    (twittering-set-number-of-unread (current-buffer) 0)))

(defun twittering-set-number-of-unread (buffer number)
  (let* ((entry (assq buffer twittering-unread-status-info))
	 (current (or (cadr entry) 0)))
    (unless (= number current)
      (setq twittering-unread-status-info
	    (cons
	     `(,buffer ,number)
	     (if entry
		 (remq entry twittering-unread-status-info)
	       twittering-unread-status-info)))
      (force-mode-line-update))))

(defun twittering-make-unread-status-notifier-string ()
  "Generate a string that displays unread statuses."
  (setq twittering-unread-status-info
	(remove nil
		(mapcar (lambda (entry)
			  (when (buffer-live-p (car entry))
			    entry))
			twittering-unread-status-info)))
  (let ((sum (apply '+ (mapcar 'cadr twittering-unread-status-info))))
    (if (= 0 sum)
	""
      (format "tw(%d)" sum))))

(defun twittering-update-unread-status-info ()
  "Update `twittering-unread-status-info' with new tweets."
  (let* ((buffer (twittering-get-buffer-from-spec twittering-new-tweets-spec))
	 (current (or (cadr (assq buffer twittering-unread-status-info)) 0))
	 (result (+ current twittering-new-tweets-count)))
    (unless (eq buffer (current-buffer))
      (twittering-set-number-of-unread buffer result))))

(defun twittering-enable-unread-status-notifier ()
  "Enable a notifier of unread statuses on `twittering-mode'."
  (interactive)
  (setq twittering-unread-status-info
	(mapcar (lambda (buffer) `(,buffer ,0))
		(twittering-get-buffer-list)))
  (add-hook 'twittering-new-tweets-hook 'twittering-update-unread-status-info)
  (add-hook 'post-command-hook
	    'twittering-reset-unread-status-info-if-necessary)
  (add-to-list 'global-mode-string
	       '(:eval (twittering-make-unread-status-notifier-string))
	       t))

(defun twittering-disable-unread-status-notifier ()
  "Disable a notifier of unread statuses on `twittering-mode'."
  (interactive)
  (setq twittering-unread-status-info nil)
  (remove-hook 'twittering-new-tweets-hook
	       'twittering-update-unread-status-info)
  (remove-hook 'post-command-hook
	       'twittering-reset-unread-status-info-if-necessary)
  (setq global-mode-string
	(remove '(:eval (twittering-make-unread-status-notifier-string))
		global-mode-string)))

;;;
;;; Debug mode
;;;

(defvar twittering-debug-mode nil)
(defvar twittering-debug-buffer "*debug*")

(defun twittering-debug-buffer ()
  (twittering-get-or-generate-buffer twittering-debug-buffer))

(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if twittering-debug-mode
	   (with-current-buffer (twittering-debug-buffer)
	     (insert "[debug] " (prin1-to-string ,obsym))
	     (newline)
	     ,obsym)
	 ,obsym))))

(defun debug-printf (fmt &rest args)
  (when twittering-debug-mode
    (with-current-buffer (twittering-debug-buffer)
      (insert "[debug] " (apply 'format fmt args))
      (newline))))

(defun twittering-debug-mode ()
  (interactive)
  (setq twittering-debug-mode
	(not twittering-debug-mode))
  (message (if twittering-debug-mode "debug mode:on" "debug mode:off")))

;;;
;;; keymap
;;;

(if twittering-mode-map
    (let ((km twittering-mode-map))
      (define-key km (kbd "C-c C-f") 'twittering-friends-timeline)
      (define-key km (kbd "C-c C-r") 'twittering-replies-timeline)
      (define-key km (kbd "C-c C-g") 'twittering-public-timeline)
      (define-key km (kbd "C-c C-u") 'twittering-user-timeline)
      (define-key km (kbd "C-c C-s") 'twittering-update-status-interactive)
      (define-key km (kbd "C-c C-e") 'twittering-erase-old-statuses)
      (define-key km (kbd "C-c C-m") 'twittering-retweet)
      (define-key km (kbd "C-c C-h") 'twittering-set-current-hashtag)
      (define-key km (kbd "C-m") 'twittering-enter)
      (define-key km (kbd "C-c C-l") 'twittering-update-lambda)
      (define-key km (kbd "<mouse-1>") 'twittering-click)
      (define-key km (kbd "C-c C-v") 'twittering-view-user-page)
      (define-key km (kbd "a") 'twittering-toggle-activate-buffer)
      (define-key km (kbd "g") 'twittering-current-timeline)
      (define-key km (kbd "u") 'twittering-update-status-interactive)
      (define-key km (kbd "d") 'twittering-direct-message)
      (define-key km (kbd "v") 'twittering-other-user-timeline)
      (define-key km (kbd "V") 'twittering-visit-timeline)
      (define-key km (kbd "L") 'twittering-other-user-list-interactive)
      ;; (define-key km (kbd "j") 'next-line)
      ;; (define-key km (kbd "k") 'previous-line)
      (define-key km (kbd "j") 'twittering-goto-next-status)
      (define-key km (kbd "k") 'twittering-goto-previous-status)
      (define-key km (kbd "l") 'forward-char)
      (define-key km (kbd "h") 'backward-char)
      (define-key km (kbd "0") 'beginning-of-line)
      (define-key km (kbd "^") 'beginning-of-line-text)
      (define-key km (kbd "$") 'end-of-line)
      (define-key km (kbd "n") 'twittering-goto-next-status-of-user)
      (define-key km (kbd "p") 'twittering-goto-previous-status-of-user)
      (define-key km (kbd "C-i") 'twittering-goto-next-thing)
      (define-key km (kbd "M-C-i") 'twittering-goto-previous-thing)
      (define-key km (kbd "<backtab>") 'twittering-goto-previous-thing)
      (define-key km (kbd "<backspace>") 'twittering-scroll-down)
      (define-key km (kbd "M-v") 'twittering-scroll-down)
      (define-key km (kbd "SPC") 'twittering-scroll-up)
      (define-key km (kbd "C-v") 'twittering-scroll-up)
      (define-key km (kbd "G") 'end-of-buffer)
      (define-key km (kbd "H") 'twittering-goto-first-status)
      (define-key km (kbd "i") 'twittering-icon-mode)
      (define-key km (kbd "r") 'twittering-toggle-show-replied-statuses)
      (define-key km (kbd "s") 'twittering-scroll-mode)
      (define-key km (kbd "t") 'twittering-toggle-proxy)
      (define-key km (kbd "C-c C-p") 'twittering-toggle-proxy)
      (define-key km (kbd "q") 'twittering-kill-buffer)
      (define-key km (kbd "C-c C-q") 'twittering-search)
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

(unless twittering-mode-syntax-table
  (setq twittering-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" twittering-mode-syntax-table)
  (modify-syntax-entry ?\" "w" twittering-mode-syntax-table)
  )

(defun twittering-mode-init-global ()
  "Initialize global variables for `twittering-mode'."
  (defface twittering-username-face
    `((t nil)) "" :group 'faces)
  (if (facep 'font-lock-string-face)
      (copy-face 'font-lock-string-face 'twittering-username-face)
    (copy-face 'bold 'twittering-username-face))
  (set-face-attribute 'twittering-username-face nil :underline t)
  (defface twittering-uri-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'twittering-uri-face nil :underline t)
  (twittering-update-status-format)
  (setq twittering-username-active
	(or twittering-username (read-string "your twitter username: ")))
  (setq twittering-password-active
	(or twittering-password (read-passwd "your twitter password: ")))
  (when twittering-use-convert
    (if (null twittering-convert-program)
	(setq twittering-use-convert nil)
      (with-temp-buffer
	(call-process twittering-convert-program nil (current-buffer) nil
		      "-version")
	(goto-char (point-min))
	(if (null (search-forward-regexp "\\(Image\\|Graphics\\)Magick" nil t))
	    (setq twittering-use-convert nil)))))
  (twittering-setup-proxy)
  )

(defvar twittering-mode-hook nil
  "Twittering-mode hook.")

(defvar twittering-initialized nil)

(defun twittering-mode-setup ()
  (kill-all-local-variables)
  (setq major-mode 'twittering-mode)
  (setq buffer-read-only t)

  ;; Prevent `global-font-lock-mode' enabling `font-lock-mode'.
  ;; This technique is derived from `lisp/bs.el' distributed with Emacs 22.2.
  (make-local-variable 'font-lock-global-modes)
  (setq font-lock-global-modes '(not twittering-mode))

  (make-local-variable 'twittering-icon-mode)
  (make-local-variable 'twittering-jojo-mode)
  (make-local-variable 'twittering-reverse-mode)
  (make-local-variable 'twittering-scroll-mode)
  (use-local-map twittering-mode-map)
  (twittering-update-mode-line)
  (set-syntax-table twittering-mode-syntax-table)
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (font-lock-mode -1))
  (run-hooks 'twittering-mode-hook))

(defun twittering-mode ()
  "Major mode for Twitter
\\{twittering-mode-map}"
  (interactive)
  (unless twittering-initialized
    (twittering-mode-init-global)
    (setq twittering-initialized t))
  (twittering-visit-timeline twittering-initial-timeline-spec-string))

;;;
;;; Edit mode
;;;

(defvar twittering-edit-buffer "*twittering-edit*")
(defvar twittering-pre-edit-window-configuration nil)
(defvar twittering-edit-history nil)
(defvar twittering-edit-local-history nil)
(defvar twittering-edit-local-history-idx nil)
(defvar twittering-help-overlay nil)
(defvar twittering-warning-overlay nil)

(define-derived-mode twittering-edit-mode text-mode "twmode-status-edit"
  (use-local-map twittering-edit-mode-map)

  (make-local-variable 'twittering-help-overlay)
  (setq twittering-help-overlay nil)
  (make-local-variable 'twittering-warning-overlay)
  (setq twittering-warning-overlay (make-overlay 1 1 nil nil nil))
  (overlay-put twittering-warning-overlay 'face 'font-lock-warning-face)

  (make-local-variable 'twittering-edit-local-history)
  (setq twittering-edit-local-history (cons (buffer-string)
					    twittering-edit-history))
  (make-local-variable 'twittering-edit-local-history-idx)
  (setq twittering-edit-local-history-idx 0)

  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 'twittering-edit-length-check)
  )

(when twittering-edit-mode-map
  (let ((km twittering-edit-mode-map))
    (define-key km (kbd "C-c C-c") 'twittering-edit-post-status)
    (define-key km (kbd "C-c C-k") 'twittering-edit-cancel-status)
    (define-key km (kbd "M-n") 'twittering-edit-next-history)
    (define-key km (kbd "M-p") 'twittering-edit-previous-history)
    (define-key km (kbd "<f4>") 'twittering-edit-replace-at-point)))

(defun twittering-edit-length-check (&optional beg end len)
  (let* ((status (twittering-edit-extract-status))
	 (sign-str (twittering-sign-string))
	 (maxlen (- 140 (length sign-str)))
	 (length (length status)))
    (setq mode-name
	  (format "twmode-status-edit[%d/%d/140]" length maxlen))
    (force-mode-line-update)
    (if (< maxlen length)
	(move-overlay twittering-warning-overlay (1+ maxlen) (1+ length))
      (move-overlay twittering-warning-overlay 1 1))))

(defun twittering-edit-extract-status ()
  (if (eq major-mode 'twittering-edit-mode)
      (buffer-string)
    ""))

(defun twittering-edit-setup-help (&optional username spec)
  (let* ((item (if (twittering-timeline-spec-is-direct-messages-p spec)
		   (format "a direct message to %s" username)
		 "a tweet"))
	 (help-str (format "Keymap:
  C-c C-c: send %s
  C-c C-k: cancel %s
  M-n    : next history element
  M-p    : previous history element

---- text above this line is ignored ----
" item item))
	 (help-overlay
	  (or twittering-help-overlay
	      (make-overlay 1 1 nil nil nil))))
    (add-text-properties 0 (length help-str) '(face font-lock-comment-face)
			 help-str)
    (overlay-put help-overlay 'before-string help-str)
    (setq twittering-help-overlay help-overlay)))

(defun twittering-edit-close ()
  (kill-buffer (current-buffer))
  (when twittering-pre-edit-window-configuration
    (set-window-configuration twittering-pre-edit-window-configuration)
    (setq twittering-pre-edit-window-configuration nil)))

(defvar twittering-reply-recipient nil)

(defun twittering-update-status-from-pop-up-buffer (&optional init-str reply-to-id username spec)
  (interactive)
  (let ((buf (generate-new-buffer twittering-edit-buffer)))
    (setq twittering-pre-edit-window-configuration
	  (current-window-configuration))
    (pop-to-buffer buf)
    (twittering-edit-mode)
    (twittering-edit-setup-help username spec)
    (if (twittering-timeline-spec-is-direct-messages-p spec)
	(message "C-c C-c to send, C-c C-k to cancel")
      (and (null init-str)
	   twittering-current-hashtag
	   (setq init-str (format " #%s " twittering-current-hashtag)))
      (message "C-c C-c to post, C-c C-k to cancel"))
    (when init-str
      (insert init-str)
      (set-buffer-modified-p nil))
    (make-local-variable 'twittering-reply-recipient)
    (setq twittering-reply-recipient `(,reply-to-id ,username ,spec))))

(defun twittering-edit-post-status ()
  (interactive)
  (let ((status (twittering-edit-extract-status))
	(reply-to-id (nth 0 twittering-reply-recipient))
	(username (nth 1 twittering-reply-recipient))
	(spec (nth 2 twittering-reply-recipient)))
    (cond
     ((not (twittering-status-not-blank-p status))
      (message "Empty tweet!"))
     ((< 140 (length status))
      (message "Too long tweet!"))
     (t
      (setq twittering-edit-history
	    (cons status twittering-edit-history))
      (cond
       ((twittering-timeline-spec-is-direct-messages-p spec)
	(if username
	    (let ((parameters `(("user" . ,username)
				("text" . ,status))))
	      (twittering-http-post twittering-api-host "1/direct_messages/new"
				    parameters))
	  (message "No username specified")))
       (t
	(let ((parameters `(("status" . ,status))))
	  ;; Add in_reply_to_status_id only when a posting status
	  ;; begins with @username.
	  (when (and reply-to-id
		     (string-match
		      (concat "^@" username "\\(?:[\n\r \t]+\\)*")
		      status))
	    (add-to-list 'parameters
			 `("in_reply_to_status_id" .
			   ,(format "%s" reply-to-id))))
	  (twittering-http-post twittering-api-host "1/statuses/update"
				parameters))))
      (twittering-edit-close)))))

(defun twittering-edit-cancel-status ()
  (interactive)
  (when (or (not (buffer-modified-p))
	    (prog1 (if (y-or-n-p "Cancel this tweet? ")
		       (message "Request canceled")
		     (message nil))))
    (twittering-edit-close)))

(defun twittering-edit-next-history ()
  (interactive)
  (if (>= 0 twittering-edit-local-history-idx)
      (message "End of history.")
    (let ((current-history (nthcdr twittering-edit-local-history-idx
				   twittering-edit-local-history)))
      (setcar current-history (buffer-string))
      (decf twittering-edit-local-history-idx)
      (erase-buffer)
      (insert (nth twittering-edit-local-history-idx
		   twittering-edit-local-history))
      (twittering-edit-setup-help)
      (goto-char (point-min)))))

(defun twittering-edit-previous-history ()
  (interactive)
  (if (>= twittering-edit-local-history-idx
	  (- (length twittering-edit-local-history) 1))
      (message "Beginning of history.")
    (let ((current-history (nthcdr twittering-edit-local-history-idx
				   twittering-edit-local-history)))
      (setcar current-history (buffer-string))
      (incf twittering-edit-local-history-idx)
      (erase-buffer)
      (insert (nth twittering-edit-local-history-idx
		   twittering-edit-local-history))
      (twittering-edit-setup-help)
      (goto-char (point-min))))
  )

(defun twittering-edit-replace-at-point ()
  (interactive)
  (when (eq major-mode 'twittering-edit-mode)
    (twittering-tinyurl-replace-at-point)
    (twittering-edit-length-check)))

;;;
;;; Functions for URL library
;;;

(defun twittering-url-wrapper (func &rest args)
  (let ((url-proxy-services
	 (when twittering-proxy-use
	   (let ((proxy-str (format "%s:%s" twittering-proxy-server
				    twittering-proxy-port)))
	     (if twittering-use-ssl
		 `(("http" . ,proxy-str)
		   ("https" . ,proxy-str))
	       `(("http" . ,proxy-str))))))
	(url-show-status nil))
    (apply func args)))

(defun twittering-url-insert-file-contents (url)
  (twittering-url-wrapper 'url-insert-file-contents url))

(defun twittering-url-retrieve-synchronously (url)
  (twittering-url-wrapper 'url-retrieve-synchronously url))

;;;
;;; Basic HTTP functions
;;;

(defun twittering-find-curl-program ()
  "Returns an appropriate `curl' program pathname or nil if not found."
  (or (executable-find "curl")
      (let ((windows-p (memq system-type '(windows-nt cygwin)))
	    (curl.exe
	     (expand-file-name
	      "curl.exe"
	      (expand-file-name
	       "win-curl"
	       (file-name-directory (symbol-file 'twit))))))
	(and windows-p
	     (file-exists-p curl.exe) curl.exe))))

(defun twittering-start-http-session-curl-p ()
  "Return t if curl was installed, otherwise nil."
  (and (setq twittering-curl-program (twittering-find-curl-program))
       t))

(defun twittering-start-http-session-curl-https-p ()
  "Return t if curl was installed and the curl support HTTPS, otherwise nil."
  (if twittering-curl-program
      (with-temp-buffer
	(call-process twittering-curl-program
		      nil (current-buffer) nil
		      "--version")
	(goto-char (point-min))
	(and (search-forward-regexp "^Protocols: .*https" nil t)
	     t))
    nil))

(defun twittering-lookup-http-start-function (order table)
  "Decide a connection method from currently available methods."
  (let ((rest order)
	(result nil)
	(msg-format "A function \"%s\" (referred from %s.%s) was not found"))
    (while rest
      (let* ((candidate (car rest))
	     (entry (assq candidate table))
	     (entry-sym (car-safe entry))
	     (check-func (cdr (assq 'check entry)))
	     (https-func (if twittering-use-ssl
			     (cdr (assq 'https entry))
			   ;; Ignore `https' when `twittering-use-ssl' is nil.
			   t))
	     (start-func (cdr (assq 'start entry))))
	(if (and (cond
		  ((null check-func) nil)
		  ((eq t check-func) t)
		  ((functionp check-func) (funcall check-func))
		  (t (message msg-format check-func entry-sym 'check)
		     (error msg-format check-func entry-sym 'check)))
		 (cond
		  ((null https-func) nil)
		  ((eq t https-func) t)
		  ((functionp https-func) (funcall https-func))
		  (t (message msg-format https-func entry-sym 'https)
		     (error msg-format https-func entry-sym 'https)))
		 (cond
		  ((functionp start-func) t)
		  (t (message msg-format start-func entry-sym 'start)
		     (error msg-format start-func entry-sym 'start))))
	    (setq result start-func
		  rest nil)
	  (setq rest (cdr rest)))))
    (unless result
      (if twittering-use-ssl
	  ;; Fall back on connection without SSL.
	  (when (yes-or-no-p "HTTPS(SSL) is not available because your 'cURL' cannot use HTTPS. Use HTTP instead? ")
	    (setq twittering-use-ssl nil)
	    (twittering-update-mode-line)
	    (setq result (twittering-lookup-http-start-function order table)))
	(message "All connection methods are unavailable.")))
    result))

(defun twittering-start-http-session (method headers host port path parameters &optional noninteractive sentinel)
  "METHOD    : http method
HEADERS   : http request heades in assoc list
HOST      : remote host name
PORT      : destination port number. nil means default port (http: 80, https: 443)
PATH      : http request path
PARAMETERS: http request parameters (query string)"
  (unless (member method '("POST" "GET"))
    (error "Unknown HTTP method: %s" method))
  (unless (string-match "^/" path)
    (error "Invalid HTTP path: %s" path))

  (unless (assoc "Host" headers)
    (setq headers (cons `("Host" . ,host) headers)))
  (unless (assoc "User-Agent" headers)
    (setq headers (cons `("User-Agent" . ,(twittering-user-agent))
			headers)))

  (let ((func (twittering-lookup-http-start-function
	       twittering-connection-type-order
	       twittering-connection-type-table)))
    (if (and func (fboundp func))
	(funcall func method headers host port path parameters
		 noninteractive sentinel)
      nil)))

(defvar twittering-cert-file nil)

(defun twittering-delete-ca-cert-file ()
  (when (and twittering-cert-file
	     (file-exists-p twittering-cert-file))
    (delete-file twittering-cert-file)
    (setq twittering-cert-file nil)))

;;; FIXME: file name is hard-coded. More robust way is desired.
(defun twittering-ensure-ca-cert ()
  "Create a CA certificate file if it does not exist, and return
its file name."
  (if twittering-cert-file
      twittering-cert-file
    (let ((file-name (make-temp-file "twmode-cacert")))
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
      (add-hook 'kill-emacs-hook 'twittering-delete-ca-cert-file)
      (setq twittering-cert-file file-name))))

(defun twittering-start-http-session-curl (method headers host port path parameters &optional noninteractive sentinel)
  ;; TODO: use curl
  (let* ((request (twittering-make-http-request
		   method headers host port path parameters))
	 (temp-buffer (generate-new-buffer "*twmode-http-buffer*"))
	 (headers (if (assoc "Expect" headers)
		      headers
		    (cons '("Expect" . "") headers)))
	 (curl-args
	  `("--include" "--silent"
	    ,@(mapcan (lambda (pair)
			;; Do not overwrite internal headers `curl' would use.
			;; Thanks to William Xu.
			;; "cURL - How To Use"
			;; http://curl.haxx.se/docs/manpage.html
			(unless (string= (car pair) "Host")
			  `("-H" ,(format "%s: %s" (car pair) (cdr pair)))))
		      headers)
	    ,@(when twittering-use-ssl
		`("--cacert" ,(twittering-ensure-ca-cert)))
	    ,@(when twittering-proxy-use
		`("-x" ,(format "%s:%s" twittering-proxy-server
				twittering-proxy-port)))
	    ,@(when (and twittering-proxy-use
			 twittering-proxy-user twittering-proxy-password)
		`("-U" ,(format "%s:%s" twittering-proxy-user
				twittering-proxy-password)))
	    ,@(when (string= "POST" method)
		(mapcan (lambda (pair)
			  (list
			   "-d"
			   (format "%s=%s"
				   (twittering-percent-encode (car pair))
				   (twittering-percent-encode (cdr pair)))))
			parameters))
	    ,(concat (funcall request :uri)
		     (when parameters
		       (concat "?" (funcall request :query-string)))))))
    (debug-print curl-args)
    (lexical-let ((noninteractive noninteractive)
		  (sentinel sentinel))
      (let ((curl-process
	     (apply 'start-process
		    "*twmode-curl*"
		    temp-buffer
		    twittering-curl-program
		    curl-args)))
	(set-process-sentinel
	 curl-process
	 (lambda (&rest args)
	   (apply #'twittering-http-default-sentinel
		  sentinel noninteractive args)))
	curl-process)))
  )

;; TODO: proxy
(defun twittering-start-http-session-native (method headers host port path parameters &optional noninteractive sentinel)
  (let ((request (twittering-make-http-request
		  method headers host port path parameters))
	(temp-buffer (generate-new-buffer "*twmode-http-buffer*")))
    (flet ((request (key)
		    (funcall request key)))
      (let* ((request-str
	      (format "%s %s%s HTTP/1.1\r\n%s\r\n\r\n"
		      (request :method)
		      (request :uri)
		      (if parameters
			  (concat "?" (request :query-string))
			"")
		      (request :headers-string)))
	     (server (if twittering-proxy-use
			 twittering-proxy-server
		       (request :host)))
	     (port (if twittering-proxy-use
		       twittering-proxy-port
		     (request :port)))
	     (proc (open-network-stream
		    "network-connection-process" temp-buffer server port))
	     )
	(lexical-let ((sentinel sentinel)
		      (noninteractive noninteractive))
	  (set-process-sentinel
	   proc
	   (lambda (&rest args)
	     (apply #'twittering-http-default-sentinel
		    sentinel noninteractive args))))
	(debug-print request-str)
	(process-send-string proc request-str)
	proc)))
  )

;;; TODO: proxy
(defun twittering-make-http-request (method headers host port path parameters)
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
  :query-string"
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
	    (error "No such key in HTTP request data: %s" key))))
      )))

(defun twittering-http-application-headers (&optional method headers)
  "Retuns an assoc list of HTTP headers for twittering-mode."
  (unless method
    (setq method "GET"))

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
    (when (string= "GET" method)
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
    (when (string= "POST" method)
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

(defun twittering-get-error-message (buffer)
  (if buffer
      (let ((xmltree (twittering-get-response-body buffer
						   'xml-parse-region)))
	(car (cddr (assq 'error (or (assq 'errors xmltree)
				    (assq 'hash xmltree))))))
    nil))

(defun twittering-http-get (host method &optional noninteractive parameters format sentinel)
  (if (null format)
      (setq format "xml"))
  (if (null sentinel)
      (setq sentinel 'twittering-http-get-default-sentinel))

  (twittering-start-http-session
   "GET" (twittering-http-application-headers "GET")
   host nil (concat "/" method "." format) parameters noninteractive sentinel))

(defun twittering-created-at-to-seconds (created-at)
  (let ((encoded-time (apply 'encode-time (parse-time-string created-at))))
    (+ (* (car encoded-time) 65536)
       (cadr encoded-time))))

(defun twittering-http-default-sentinel (func noninteractive proc stat &optional suc-msg)
  (debug-printf "http-default-sentinel: proc=%s stat=%s" proc stat)
  (let ((temp-buffer (process-buffer proc))
	(status (process-status proc))
	(mes nil))
    (cond
     ((null status)
      (setq mes "Failure: no such process exists."))
     ;; If a process is running, the processing sentinel has been postponed.
     ((memq status '(run stop open listen connect))
      (debug-printf "http-default-sentinel: postponed by status `%s'" status)
      t)
     ((memq status '(exit signal closed failed))
      (unwind-protect
	  (let* ((header (twittering-get-response-header temp-buffer))
		 (header-info
		  (and header (twittering-update-server-info header))))
	    (setq mes
		  (cond
		   ((null header-info)
		    "Failure: Bad http response.")
		   ((and func (fboundp func))
		    (with-current-buffer temp-buffer
		      (funcall func header-info proc noninteractive suc-msg)))
		   (t
		    nil))))
	;; unwindforms
	(twittering-release-process proc)
	(when (and (not twittering-debug-mode) (buffer-live-p temp-buffer))
	  (kill-buffer temp-buffer))))
     (t
      (setq mes (format "Failure: unknown condition: %s" status))))
    (when (and mes (twittering-buffer-related-p))
      (message mes))))

(defun twittering-http-get-default-sentinel (header-info proc noninteractive &optional suc-msg)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info))))
    (case-string
     status-code
     (("200")
      (let* ((spec (twittering-get-timeline-spec-from-process proc))
	     (spec-string
	      (twittering-get-timeline-spec-string-from-process proc))
	     (statuses (twittering-get-status-from-http-response
			spec (process-buffer proc))))
	(when statuses
	  (let ((new-statuses
		 (twittering-add-statuses-to-timeline-data statuses spec))
		(buffer (twittering-get-buffer-from-spec spec)))
	    ;; FIXME: We should retrieve un-retrieved statuses until
	    ;; statuses is nil. twitter server returns nil as
	    ;; xmltree with HTTP status-code is "200" when we
	    ;; retrieved all un-retrieved statuses.
	    (when new-statuses
	      (twittering-render-timeline buffer t new-statuses))
	    (twittering-add-timeline-history spec-string)))
	(if twittering-notify-successful-http-get
	    (if suc-msg suc-msg (format "Success: Get %s." spec-string))
	  nil)))
     (t
      (let ((error-mes (twittering-get-error-message (process-buffer proc))))
	(if error-mes
	    (format "Response: %s (%s)" status-line error-mes)
	  (format "Response: %s" status-line)))))))

(defun twittering-http-get-list-index-sentinel (header-info proc noninteractive &optional suc-msg)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(indexes nil)
	(mes nil))
    (case-string
     status-code
     (("200")
      (let ((xmltree (twittering-get-response-body (process-buffer proc)
						   'xml-parse-region)))
	(when xmltree
	  (setq indexes
		(mapcar
		 (lambda (c-node)
		   (caddr (assq 'slug c-node)))
		 (remove nil
			 (mapcar
			  (lambda (node)
			    (and (consp node) (eq 'list (car node))
				 node))
			  (cdr-safe
			   (assq 'lists (assq 'lists_list xmltree))))
			 ))
		))))
     (t
      (let ((error-mes (twittering-get-error-message (process-buffer proc))))
	(if error-mes
	    (setq mes (format "Response: %s (%s)" status-line error-mes))
	  (setq mes (format "Response: %s" status-line))))))
    (setq twittering-list-index-retrieved
	  (or indexes
	      mes
	      "")) ;; set "" explicitly if user does not have a list.
    nil))

(defun twittering-http-post (host method &optional parameters format sentinel)
  "Send HTTP POST request to api.twitter.com (or search.twitter.com)

HOST is hostname of remote side, api.twitter.com (or search.twitter.com).
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6
FORMAT is a response data format (\"xml\", \"atom\", \"json\")"
  (if (null format)
      (setq format "xml"))
  (if (null sentinel)
      (setq sentinel 'twittering-http-post-default-sentinel))

  (add-to-list 'parameters '("source" . "twmode"))

  (twittering-start-http-session
   "POST" (twittering-http-application-headers "POST")
   host nil (concat "/" method "." format) parameters noninteractive sentinel))

(defun twittering-http-post-default-sentinel (header-info proc noninteractive &optional suc-msg)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info))))
    (case-string
     status-code
     (("200")
      (if suc-msg suc-msg "Success: Post."))
     (t
      (let ((error-mes (twittering-get-error-message (process-buffer proc))))
	(if error-mes
	    (format "Response: %s (%s)" status-line error-mes)
	  (format "Response: %s" status-line)))))))

(defun twittering-get-response-header (buffer)
  "Exract HTTP response header from HTTP response.
BUFFER may be a buffer or the name of an existing buffer which contains the HTTP response."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      ;; FIXME: curl prints HTTP proxy response header, so strip it
      (when (search-forward-regexp
	     "HTTP/1\\.[01] 200 Connection established\r\n\r\n" nil t)
	(delete-region (point-min) (point)))
      (if (search-forward-regexp "\r?\n\r?\n" nil t)
	  (buffer-substring (point-min) (match-end 0))
	nil))))

(defun twittering-get-response-body (buffer &optional func)
  "Exract HTTP response body from HTTP response.
If FUNC is non-nil, parse a response body by FUNC and return it.
Return nil when parse failed.
BUFFER may be a buffer or the name of an existing buffer."
  (if (null func)
      (setq func 'buffer-substring))

  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp "\r?\n\r?\n" nil t)
	  (condition-case error-str
	      (funcall func (match-end 0) (point-max))
	    (error
	     (when (twittering-buffer-related-p)
	       (message "Failure: %s" error-str))
	     nil))
	(error "Failure: invalid HTTP response"))
      )))

(defun twittering-get-status-from-http-response (spec buffer)
  "Exract statuses from HTTP response, and return a list.
Return nil when parse failed.

SPEC is timeline-spec which was used to retrieve BUFFER.
BUFFER may be a buffer or the name of an existing buffer."
  (let ((body (twittering-get-response-body buffer 'xml-parse-region)))
    (when body
      (if (eq 'search (car spec))
	  (twittering-atom-xmltree-to-status body)
	(twittering-xmltree-to-status body)))))

(defun twittering-atom-xmltree-to-status-datum (atom-xml-entry)
  (let ((id-str (car (cddr (assq 'id atom-xml-entry))))
	(time-str (car (cddr (assq 'updated atom-xml-entry))))
	(author-str (car (cddr (assq 'name (assq 'author atom-xml-entry))))))
    `((created-at
       . ,(if (string-match "\\(.*\\)T\\(.*\\)Z" time-str)
	      ;; time-str is formatted as
	      ;; "Combined date and time in UTC:" in ISO 8601.
	      (format "%s %s +0000"
		      (match-string 1 time-str) (match-string 2 time-str))
	    ;; unknown format?
	    time-str))
      (id . ,(progn
	       (string-match ":\\([0-9]+\\)$" id-str)
	       (match-string 1 id-str)))
      (source
       . ,(let ((html (twittering-decode-html-entities
		       (car (cddr (assq 'twitter:source atom-xml-entry))))))
	    (when (string-match
		   "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>" html)
	      (let ((uri (match-string-no-properties 1 html))
		    (caption (match-string-no-properties 2 html)))
		caption))))
      (text . ,(twittering-decode-html-entities
		(car (cddr (assq 'title atom-xml-entry)))))
      ,@(progn
	  (string-match "^\\([^ ]+\\) (\\(.*\\))$" author-str)
	  `((user-screen-name . ,(match-string 1 author-str))
	    (user-name . ,(match-string 2 author-str))))
      (user-profile-image-url
       . ,(let* ((link-items
		  (mapcar
		   (lambda (item)
		     (when (eq 'link (car-safe item))
		       (cadr item)))
		   atom-xml-entry))
		 (image-urls
		  (mapcar
		   (lambda (item)
		     (when (member '(rel . "image") item)
		       (cdr (assq 'href item))))
		   link-items)))
	    (car-safe (remq nil image-urls)))))))

(defun twittering-atom-xmltree-to-status (atom-xmltree)
  (let ((entry-list
	 (apply 'append
		(mapcar (lambda (x)
		 	  (if (eq (car-safe x) 'entry) `(,x) nil))
			(cdar atom-xmltree)))))
    (mapcar (lambda (entry)
	      (twittering-make-clickable-status-datum
	       (twittering-atom-xmltree-to-status-datum entry)))
	    entry-list)))

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
	   regex-index
	   (retweeted-status-data (cddr (assq 'retweeted_status status-data)))
	   original-created-at ;; need not export
	   original-user-name
	   original-user-screen-name
	   source-id
	   source-created-at)

      ;; save original status and adjust data if status was retweeted
      (cond
       (retweeted-status-data
	(setq original-user-screen-name (twittering-decode-html-entities
					 (assq-get 'screen_name user-data))
	      original-user-name (twittering-decode-html-entities
				  (assq-get 'name user-data))
	      original-created-at (assq-get 'created_at status-data))

	;; use id and created-at issued when retweeted.
	(setq id (assq-get 'id status-data))
	(setq created-at (assq-get 'created_at status-data))

	(setq status-data retweeted-status-data
	      user-data (cddr (assq 'user retweeted-status-data)))

	;; id and created-at of source tweet.
	(setq source-id (assq-get 'id status-data))
	(setq source-created-at (assq-get 'created_at status-data)))
       (t
	(setq id (assq-get 'id status-data))
	(setq created-at (assq-get 'created_at status-data))))

      (setq text (twittering-decode-html-entities
		  (assq-get 'text status-data)))
      (setq source (twittering-decode-html-entities
		    (assq-get 'source status-data)))
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

      (twittering-make-clickable-status-datum
       (mapcar (lambda (sym)
                 `(,sym . ,(symbol-value sym)))
               '(id text source created-at truncated
                    in-reply-to-status-id
                    in-reply-to-screen-name
                    user-id user-name user-screen-name user-location
                    user-description
                    user-profile-image-url
                    user-url
                    user-protected
                    original-user-name
                    original-user-screen-name))))))

(defun twittering-make-clickable-status-datum (status)
  (flet ((assq-get (item seq)
		   (cdr (assq item seq))))
    (let ((user-name (assq-get 'user-name status))
	  (id (assq-get 'id status))
	  (text (assq-get 'text status))
	  (source (assq-get 'source status))
	  (created-at (assq-get 'created-at status))
	  (truncated (assq-get 'truncated status))
	  (in-reply-to-status-id (assq-get 'in-reply-to-status-id status))
	  (in-reply-to-screen-name (assq-get 'in-reply-to-screen-name status))
	  (user-id (assq-get 'user-id status))
	  (user-name (assq-get 'user-name status))
	  (user-screen-name (assq-get 'user-screen-name status))
	  (user-location (assq-get 'user-location status))
	  (user-description (assq-get 'user-description status))
	  (user-profile-image-url (assq-get 'user-profile-image-url status))
	  (user-url (assq-get 'user-url status))
	  (user-protected (assq-get 'user-protected status)))

      ;; make user-name clickable
      (add-text-properties
       0 (length user-name)
       `(mouse-face highlight
		    uri ,(twittering-get-status-url user-screen-name)
		    screen-name-in-text ,user-screen-name
		    goto-spec ,(twittering-string-to-timeline-spec
				user-screen-name)
		    face twittering-username-face)
       user-name)

      ;; make user-screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
		    uri ,(twittering-get-status-url user-screen-name)
		    screen-name-in-text ,user-screen-name
		    goto-spec ,(twittering-string-to-timeline-spec
				user-screen-name)
		    face twittering-username-face)
       user-screen-name)

      ;; make hashtag, listname, screenname, and URI in text clickable
      (let ((pos 0)
	    (regexp-str
	     (concat twittering-regexp-hash
		     "\\([a-zA-Z0-9_-]+\\)\\|"
		     twittering-regexp-atmark
		     "\\([a-zA-Z0-9_-]+/[a-zA-Z0-9_-]+\\)\\|"
		     twittering-regexp-atmark
		     "\\([a-zA-Z0-9_-]+\\)\\|"
		     "\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)")))
	(while
	    (and (string-match regexp-str text pos)
		 (let ((next-pos (match-end 0))
		       (hashtag (match-string-no-properties 1 text))
		       (listname (match-string-no-properties 2 text))
		       (screenname (match-string-no-properties 3 text))
		       (uri (match-string-no-properties 4 text))
		       beg end prop)
		   (if (eq next-pos pos)
		       nil
		     (cond
		      (hashtag
		       (setq beg (match-beginning 0) ;; XXX: not 1.
			     end (match-end 1))
		       (let ((spec (twittering-string-to-timeline-spec
				    (concat "#" hashtag)))
			     (url (twittering-get-search-url
				   (concat "#" hashtag))))
			 (setq prop
			       `(mouse-face
				 highlight
				 uri ,url goto-spec ,spec
				 face twittering-username-face))))
		      (listname
		       (setq beg (match-beginning 2)
			     end (match-end 2)
			     prop `(mouse-face
				    highlight
				    uri ,(twittering-get-status-url listname)
				    goto-spec
				    ,(twittering-string-to-timeline-spec
				      listname)
				    face twittering-username-face)))
		      (screenname
		       (setq beg (match-beginning 3)
			     end (match-end 3)
			     prop `(mouse-face
				    highlight
				    uri ,(twittering-get-status-url
					  screenname)
				    screen-name-in-text ,screenname
				    goto-spec
				    ,(twittering-string-to-timeline-spec
				      screenname)
				    face twittering-uri-face)))
		      (uri
		       (setq beg (match-beginning 4)
			     end (match-end 4)
			     prop `(mouse-face
				    highlight
				    uri ,uri
				    face twittering-uri-face)))
		      (t
		       (setq prop nil)))
		     (when prop
		       (add-text-properties beg end prop text))
		     (setq pos next-pos))))))

      ;; make source pretty and clickable
      (when (and source
		 (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>" source))
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
	  (add-to-list 'status (cons 'source source))))
      status)))

(defun twittering-xmltree-to-status (xmltree)
  (setq xmltree
	(cond
	 ((eq 'direct-messages (caar xmltree))
	  `(,@(mapcar
	       (lambda (c-node)
		 `(status nil
			  (created_at
			   nil ,(caddr (assq 'created_at c-node)))
			  (id nil ,(caddr (assq 'id c-node)))
			  (text nil ,(caddr (assq 'text c-node)))
			  (source nil ,(format "%s" (car c-node))) ;; fake
			  (truncated nil "false")
			  (in_reply_to_status_id nil)
			  (in_reply_to_user_id
			   nil ,(caddr (assq 'recipient_id c-node)))
			  (favorited nil "false")
			  (in_reply_to_screen_name
			   nil ,(caddr (assq 'recipient_screen_name c-node)))
			  (user nil ,@(cdddr (assq 'sender c-node)))))
	       (remove nil
		       (mapcar
			(lambda (node)
			  (and (consp node) (eq 'direct_message (car node))
			       node))
			(cdr-safe (assq 'direct-messages xmltree))))
	       )))
	 ((eq 'statuses (caar xmltree))
	  (cddr (car xmltree)))
	 (t ;; unknown format?
	  nil)))

  (mapcar #'twittering-status-to-status-datum
 	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
 	  ;; On Emacs22, there may be blank strings
	  (remove nil (mapcar (lambda (x)
				(if (consp x) x))
			      xmltree))))

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
      (t (format "%%%02x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun twittering-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?Z))
      (and (<= ?a ch) (<= ch ?z))
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
		     (string-match "&\\(#\\([0-9]+\\)\\|\\([a-zA-Z]+\\)\\);"
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
			 ((string= "quot" letter-entity) (list-push "\"" result))
			 (t (list-push "?" result))))
		  (t (list-push "?" result)))
	    (setq cursor (match-end 0))))
	(list-push (substring encoded-str cursor) result)
	(apply 'concat (nreverse result)))
    ""))

;;;
;;; Statuses on buffer
;;;

(defun twittering-for-each-property-region (prop func &optional buffer interrupt)
  "Apply FUNC to each region, where property PROP is non-nil, on BUFFER.
If INTERRUPT is non-nil, the iteration is stopped if FUNC returns nil."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
	  (end-marker (make-marker)))
      (set-marker-insertion-type end-marker t)
      (while
	  (let ((value (get-text-property beg prop)))
	    (if value
		(let* ((end (next-single-property-change beg prop))
		       (end (or end (point-max)))
		       (end-marker (set-marker end-marker end))
		       (func-result (funcall func beg end value))
		       (end (marker-position end-marker)))
		  (when (or (null interrupt) func-result)
		    (if (get-text-property end prop)
			(setq beg end)
		      (setq beg (next-single-property-change end prop)))))
	      (setq beg (next-single-property-change beg prop)))))
      (set-marker end-marker nil))))

;;;
;;; Automatic redisplay of statuses on buffer
;;;

(defun twittering-redisplay-status-on-buffer ()
  (mapc 'twittering-redisplay-status-on-each-buffer
	(twittering-get-buffer-list)))

(defun twittering-redisplay-status-on-each-buffer (buffer)
  (let ((deactivate-mark deactivate-mark))
    (with-current-buffer buffer
      (save-excursion
	(twittering-for-each-property-region
	 'need-to-be-updated
	 (lambda (beg end value)
	   (let* ((func (car value))
		  (args (cdr value))
		  (updated-str (apply func beg end args))
		  (buffer-read-only nil))
	     (delete-region beg end)
	     (goto-char beg)
	     (insert updated-str)))
	 buffer)))))

;;;
;;; display functions
;;;

(defun twittering-render-timeline (buffer &optional additional timeline-data)
  (with-current-buffer buffer
    (let* ((spec (twittering-get-timeline-spec-for-buffer buffer))
	   (referring-id-table
	    (twittering-current-timeline-referring-id-table spec))
	   (timeline-data (or timeline-data
			      (twittering-current-timeline-data spec)))
	   (timeline-data
	    ;; Collect visible statuses.
	    (remove nil
		    (mapcar
		     (lambda (status)
		       (let ((id (cdr (assq 'id status)))
			     (source-id (cdr (assq 'source-id status))))
			 (cond
			  ((not source-id)
			   ;; `status' is not a retweet.
			   status)
			  ((and source-id
				(twittering-status-id=
				 id (gethash source-id referring-id-table)))
			   ;; `status' is the first retweet.
			   status)
			  (t
			   nil))))
		     timeline-data)))
	   (timeline-data (if twittering-reverse-mode
			      (reverse timeline-data)
			    timeline-data))
	   (empty (null (twittering-get-first-status-head)))
	   (rendering-entire (or empty (not additional)))
	   (window-list (get-buffer-window-list (current-buffer) nil t))
	   (point-window-list
	    (mapcar (lambda (window)
		      (cons (window-point window) window))
		    window-list))
	   (original-pos (point))
	   (original-buf-end (point-max))
	   (buffer-read-only nil))
      (twittering-update-status-format)
      (twittering-update-mode-line)
      (save-excursion
	(when rendering-entire
	  (erase-buffer))
	(let ((pos (if rendering-entire
		       (point-min)
		     (twittering-get-first-status-head))))
	  (mapc
	   (lambda (status)
	     (let ((id (cdr (assq 'id status))))
	       ;; Find where the status should be inserted.
	       (while
		   (let ((buf-id (get-text-property pos 'id)))
		     (if (and buf-id
			      (if twittering-reverse-mode
				  (twittering-status-id< buf-id id)
				(twittering-status-id< id buf-id)))
			 (let ((next-pos
				(twittering-get-next-status-head pos)))
			   (setq pos (or next-pos (point-max)))
			   next-pos)
		       nil)))
	       (unless (twittering-status-id= id (get-text-property pos 'id))
		 (let ((formatted-status (twittering-format-status status))
		       (separator "\n"))
		   (add-text-properties 0 (length formatted-status)
					`(belongs-spec ,spec)
					formatted-status)
		   (goto-char pos)
		   (cond
		    ((eq pos (point-max))
		     ;; Insert a status after the current position.
		     (insert formatted-status separator))
		    (t
		     ;; Use `insert-before-markers' in order to keep
		     ;; which status is pointed by each marker.
		     (insert-before-markers formatted-status separator)))
		   ;; Now, `pos' points the head of the status.
		   ;; It must be moved to the current point
		   ;; in order to skip the status inserted just now.
		   (setq pos (point))
		   (when twittering-default-show-replied-tweets
		     (twittering-show-replied-statuses
		      twittering-default-show-replied-tweets))))))
	   timeline-data)))
      (if (and twittering-image-stack window-system)
	  (clear-image-cache))
      (debug-print (current-buffer))
      (cond
       (rendering-entire
	;; Go to the latest status of buffer after full insertion.
	(let ((dest (if twittering-reverse-mode
			(point-max)
		      (point-min))))
	  (if window-list
	      (mapc
	       (lambda (window)
		 (set-window-point window dest)
		 (if twittering-reverse-mode
		     (twittering-set-window-end window (point-max))
		   (set-window-start window (point-min))))
	       window-list)
	    ;; Move the buffer position if the buffer is invisible.
	    (goto-char dest))))
       ((not twittering-scroll-mode)
	;; After additional insertion, the current position exists
	;; on the same status.
	;; Go to the original position.
	(if point-window-list
	    (mapc (lambda (pair)
		    (let* ((point (car pair))
			   (window (cdr pair))
			   (dest (if twittering-reverse-mode
				     (- (point-max)
					(- original-buf-end point))
				   point)))
		      (set-window-point window dest)))
		  point-window-list)
	  ;; Move the buffer position if the buffer is invisible.
	  (goto-char (if twittering-reverse-mode
			 (- (point-max)
			    (- original-buf-end original-pos))
		       original-pos))))
       ))
    ))

(defun twittering-replied-statuses-visible-p ()
  (let* ((pos (twittering-get-current-status-head))
	 (id (get-text-property pos 'id))
	 (prev (twittering-get-previous-status-head pos))
	 (next (twittering-get-next-status-head pos)))
    (or (get-text-property pos 'original-id)
	(and prev
	     (twittering-status-id= id (get-text-property prev 'id))
	     (get-text-property prev 'original-id))
	(and next
	     (twittering-status-id= id (get-text-property next 'id))
	     (get-text-property next 'original-id)))))

(defun twittering-show-replied-statuses (&optional count interactive)
  (interactive)
  (if (twittering-replied-statuses-visible-p)
      (when interactive
	(message "The replied statuses were already showed."))
    (let* ((base-id (twittering-get-id-at))
	   (statuses (twittering-get-replied-statuses base-id
						      (if (numberp count)
							  count)))
	   (statuses (if twittering-reverse-mode
			 statuses
		       (reverse statuses))))
      (if statuses
	  (let ((beg (if twittering-reverse-mode
			 (twittering-get-current-status-head)
		       (or (twittering-get-next-status-head)
			   (point-max))))
		(separtor "\n")
		(prefix "  ")
		(buffer-read-only nil))
	    (save-excursion
	      (goto-char beg)
	      (mapc
	       (lambda (status)
		 (let ((id (cdr (assq 'id status)))
		       (formatted-status (twittering-format-status status
								   prefix)))
		   (add-text-properties 0 (length formatted-status)
					`(id ,base-id original-id ,id)
					formatted-status)
		   (if twittering-reverse-mode
		       (insert-before-markers formatted-status separtor)
		     (insert formatted-status separtor))))
	       statuses)
	      t))
	(when interactive
	  (if (twittering-have-replied-statuses-p base-id)
	      (message "The replied statuses does not fetched yet.")
	    (message "This status does not seem having a replied status.")))
	nil))))

(defun twittering-hide-replied-statuses (&optional interactive)
  (interactive)
  (if (twittering-replied-statuses-visible-p)
      (let* ((pos (point))
	     (id (twittering-get-id-at pos))
	     (beg
	      (let ((pos pos))
		(while
		    (let* ((prev (or (twittering-get-previous-status-head pos)
				     (point-min)))
			   (prev-id (get-text-property prev 'id)))
		      (when (twittering-status-id= id prev-id)
			(not (eq (setq pos prev) (point-min))))))
		pos))
	     (buffer-read-only nil))
	(when (get-text-property pos 'original-id)
	  (goto-char beg))
	(while (when (twittering-status-id= id (get-text-property beg 'id))
		 (let ((end (or (twittering-get-next-status-head beg)
				(point-max))))
		   (if (get-text-property beg 'original-id)
		       (delete-region beg end)
		     (setq beg end)))
		 t))
	t)
    (when interactive
      (message "The replied statuses were already hided."))
    nil))

(defun twittering-toggle-show-replied-statuses ()
  (interactive)
  (if (twittering-replied-statuses-visible-p)
      (twittering-hide-replied-statuses (interactive-p))
    (twittering-show-replied-statuses twittering-show-replied-tweets
				      (interactive-p))))

(defun twittering-make-display-spec-for-icon (image-url)
  "Return the specification for `display' text property, which
limits the size of an icon image IMAGE-URL up to FIXED-LENGTH. If
the type of the image is not supported, nil is returned.

If the size of the image exceeds FIXED-LENGTH, the center of the
image are displayed."
  (let ((image-data (or (gethash `(,image-url . ,twittering-convert-fix-size)
				 twittering-image-data-table)
			(twittering-retrieve-image image-url))))
    (and image-data (image-type-available-p (car image-data))
	 (let ((image-spec
		(if (fboundp 'create-animated-image) ;; Emacs24 or later
		    (create-animated-image (cdr image-data) (car image-data)
					   t :margin 2)
		  (create-image (cdr image-data) (car image-data)
				t :margin 2))))
	   (if (and twittering-convert-fix-size (not twittering-use-convert))
	       (let* ((size (if (cdr image-data)
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
	     `(display ,image-spec))))))

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
  'processed-string --the already processed string."
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
	    (if (string-match (concat "\\`" key) following-string)
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
    (let ((skipped-string (substring string current-pos)))
      (concat result skipped-string))
    ))

(defun twittering-parse-format-string (format-str escape specifiers)
  "Split FORMAT-STR into a list consisting of fixed strings and specifiers
with match-data. For example:
 (twittering-parse-format-string
 \"%s>>%r @%C{%m/%d %H:%M:%S} %@\\n %t\" \"%\"
 '(\"%\" \"s\" \"r\" \"@\" \"t\" \"i\" \"C\\\\({\\\\([^}]*\\\\)}\\\\)?\"))
returns
 ((\"s\" \"s\" (0 1))
 \">>\"
 (\"r\" \"r\" (0 1))
 \" @\"
 (\"C\\\\({\\\\([^}]*\\\\)}\\\\)?\" \"C{%m/%d %H:%M:%S}\" (0 17 1 17 2 16))
 \" \"
 (\"@\" \"@\" (0 1))
 \"\\n \"
 (\"t\" \"t\" (0 1)))
.
In the result list, a fixed string is stored as a normal string.
A format specifier is stored as a list of the regexp for the specifier,
the matched string in FORMAT-STR and its match data."
  (let ((pos 0)
        (result nil)
        (case-fold-search nil))
    (while (string-match escape format-str pos)
      (let ((beg (match-beginning 0)))
        (unless (eq pos beg)
          (setq result (cons (substring format-str pos beg) result)))
        (let* ((rest specifiers)
               (head (+ beg (length escape)))
               (following (substring format-str head)))
          (while
              (unless (or (null rest)
                          (string-match (concat "\\`" (car rest)) following))
                (setq rest (cdr rest))))
          (let ((end (if rest
                         (+ head (match-end 0))
                       head)))
            (if rest
                (setq result (cons (list (car rest)
                                         (match-string 0 following)
                                         (match-data))
                                   result))
              (setq result (cons (substring format-str beg end) result)))
            (setq pos end)))))
    (when (< pos (length format-str))
      (setq result (cons (substring format-str pos) result)))
    (nreverse result)))

(defun twittering-generate-formater (format-str-exp escape status-sym prefix-sym specifier-sym specifiers)
  "Generate lambda expression from FORMAT-STR-EXP.
The result expression does not include specifiers except those appeared
in FORMAT-STR-EXP.
ESCAPE means the escape string of specifiers.
SPECIFIERS is a list of '(SPEC-REGEXP (STATIC-BINDINGS) FORMS), where
SPEC-REGEXP is a regexp for the specifier without ESCAPE,
STATIC-BINDINGS means bindings determined on expanding this macro
and FORMS generates a string for the specifier by using the argument
of the lambda.
In FORMS, the first argument of the lambda can be refered by STATUS-SYM.
And the second argument of the lambda can be refered by PREFIX-SYM.
In STATIC-BINDINGS and FORMS, the string matched with SPEC-REGEXP can be
refered by SPECIFIER-SYM.
Example:
 (twittering-generate-formater
  \"HEAD%# %CsampleSAMPLE TEST\"
  \"%\" 'status 'prefix 'fmt-following
  '((\"%\" () \"%\")
    (\"#\" () (cdr (assq 'id status)))
    (\"s\" () (cdr (assq 'user-screen-name status)))
    (\"C\\\\([a-z]+\\\\)\"
     ((sample (match-string 1 fmt-following))
      (sample2 sample))
     sample)
    ))
 returns
 (lambda
   (status prefix)
   (concat \"HEAD\"
           (let
               ((fmt-following \"#\"))
             (let nil
               (store-match-data
                '(0 1))
               (cdr
                (assq 'id status))))
           \" \"
           (let
               ((fmt-following \"Csample\"))
             (let
                 ((sample \"sample\")
                  (sample2 \"sample\"))
               (store-match-data
                '(0 7 1 7))
               sample))
           \"SAMPLE TEST\"))
 "
  (let* ((format-str (eval format-str-exp))
         (seq (twittering-parse-format-string format-str escape
                                              (mapcar 'car specifiers))))
    `(lambda (,status-sym ,prefix-sym)
       (concat
	,@(mapcar
	   (lambda (entry)
	     (let ((static-def (cadr (assoc (elt entry 0) specifiers)))
		   (dynamic-def (cddr (assoc (elt entry 0) specifiers))))
	       (if (not (listp entry))
		   entry
		 (store-match-data (elt entry 2))
		 (let* ((acc `((,specifier-sym (elt entry 1))))
			(static-binding
			 (mapcar
			  (lambda (bind)
			    (let* ((var (car bind))
				   (value (eval `(let ,acc ,(cadr bind))))
				   (new-bind
				    (if (or (stringp value) (functionp value)
					    (numberp value))
					`(,var ,value)
				      `(,var ',value))))
			      (setq acc (cons new-bind acc))
			      new-bind))
			  static-def)))
		   `(let ((,specifier-sym ,(elt entry 1)))
		      (let ,static-binding
			(store-match-data ',(elt entry 2))
			,@dynamic-def))))))
	   seq)))))

(defun twittering-generate-status-formater-base (format-str)
  (twittering-generate-formater
   format-str "%" 'status 'prefix 'fmt-following
   '(("%" () "%")
     ("#" () (cdr (assq 'id status)))
     ("'" () (if (string= "true" (cdr (assq 'truncated status)))
		 "..."
	       ""))
     ("@" ()
      (let* ((created-at-str (cdr (assq 'created-at status)))
	     (created-at
	      (apply 'encode-time
		     (parse-time-string created-at-str)))
	     (url
	      (twittering-get-status-url
	       (cdr (assq 'user-screen-name status))
	       (cdr (assq 'id status))))
	     (properties
	      `(mouse-face highlight face twittering-uri-face uri ,url)))
	(twittering-make-passed-time-string nil nil created-at properties)))
     ("C\\({\\([^}]*\\)}\\)?"
      ((time-format (or (match-string 2 fmt-following) "%H:%M:%S")))
      (let* ((created-at-str (cdr (assq 'created-at status)))
	     (created-at (apply 'encode-time
				(parse-time-string created-at-str))))
	(format-time-string time-format created-at)))
     ("c" () (cdr (assq 'created-at status)))
     ("d" () (cdr (assq 'user-description status)))
     ("FACE\\[\\([a-zA-Z0-9:-]+\\)\\]{\\(\\([^{}]*?\\|{.*?[^%]}\\|%}\\)*\\)}"
      ((face-name-str (match-string 1 fmt-following))
       (face-sym (intern face-name-str))
       (braced-str (match-string 2 fmt-following))
       (formater (twittering-generate-status-formater-base braced-str)))
      (let ((formated-str (funcall formater status prefix)))
	(add-text-properties 0 (length formated-str) `(face ,face-sym)
			     formated-str)
	formated-str))
     ("FILL{\\(\\([^{}]*?\\|{.*?[^%]}\\|%}\\)*\\)}"
      ((braced-str (match-string 1 fmt-following))
       (formater (twittering-generate-status-formater-base braced-str)))
      (twittering-update-filled-string nil nil formater status prefix))
     ("f" () (cdr (assq 'source status)))
     ("i" ()
      (when (and twittering-icon-mode window-system)
	(let* ((url (cdr (assq 'user-profile-image-url status)))
	       (display-spec (twittering-make-display-spec-for-icon url)))
	  (when display-spec
	    (let ((icon-string (copy-sequence " ")))
	      (set-text-properties 0 (length icon-string)
				   display-spec icon-string)
	      (add-to-list 'twittering-image-stack url)
	      icon-string)))))
     ("j" () (cdr (assq 'user-id status)))
     ("L" ()
      (let ((location (or (cdr (assq 'user-location status)) "")))
	(unless (string= "" location)
	  (concat "[" location "]"))))
     ("l" () (cdr (assq 'user-location status)))
     ("p" () (if (string= "true" (cdr (assq 'user-protected status)))
		 "[x]"
	       ""))
     ("r" ()
      (let ((reply-id (or (cdr (assq 'in-reply-to-status-id status)) ""))
	    (reply-name (or (cdr (assq 'in-reply-to-screen-name status)) "")))
	(if (string= "" reply-name)
	    ""
	  (let ((in-reply-to-string nil)
		(url nil)
		;; FIXME: If multiple buffers are implemented, `spec'
		;; must be setted as independent to current timeline.
		(spec (twittering-current-timeline-spec)))
	    (if (twittering-timeline-spec-is-direct-messages-p spec)
		;; for direct_messages{,_sent}.
		(setq in-reply-to-string (format "sent to %s" reply-name)
		      url (twittering-get-status-url reply-name))
	      ;; for other standard timeline.
	      (unless (string= "" reply-id)
		(setq in-reply-to-string (format "in reply to %s" reply-name)
		      url (twittering-get-status-url reply-name reply-id)))
	    (when (and in-reply-to-string url)
	      (concat
	       " "
	       (progn
		 (add-text-properties
		  0 (length in-reply-to-string)
		  `(mouse-face highlight face twittering-uri-face uri ,url)
		  in-reply-to-string)
		 in-reply-to-string))))))))
     ("R" ()
      (let ((retweeted-by (or (cdr (assq 'original-user-screen-name status))
			      "")))
	(unless (string= "" retweeted-by)
	  (concat " (retweeted by " retweeted-by ")"))))
     ("S" () (cdr (assq 'user-name status)))
     ("s" () (cdr (assq 'user-screen-name status)))
     ("T" () (cdr (assq 'text status)))
     ("t" () (cdr (assq 'text status)))
     ("u" () (cdr (assq 'user-url status)))
     )))

(defun twittering-generate-format-status-function (format-str)
  `(lambda (status prefix)
     (let* ((username (cdr (assq 'user-screen-name status)))
	    (id (cdr (assq 'id status)))
	    (text (cdr (assq 'text status)))
	    (common-properties (list 'username username 'id id 'text text))
	    (str (funcall
		  ,(twittering-generate-status-formater-base format-str)
		  status prefix))
	    (str (if prefix
		     (replace-regexp-in-string "^" prefix str)
		   str))
	    (next (next-single-property-change 0 'need-to-be-updated str))
	    (need-to-be-updated
	     (or (get-text-property 0 'need-to-be-updated str)
		 (and next (< next (length str))))))
       (add-text-properties 0 (length str) common-properties str)
       (when (and prefix need-to-be-updated)
	 ;; With a prefix, redisplay the total status instead of
	 ;; redisplaying partially.
	 (remove-text-properties 0 (length str) '(need-to-be-updated nil) str)
	 (put-text-property 0 (length str) 'need-to-be-updated
			    `(twittering-format-status-for-redisplay
			      ,status ,prefix)
			    str))
       str)))

(defun twittering-update-status-format (&optional format-str)
  (let ((format-str (or format-str twittering-status-format)))
    (unless (string= format-str twittering-format-status-function-source)
      (setq twittering-format-status-function-source format-str)
      (let ((before (get-buffer "*Compile-Log*")))
	(setq twittering-format-status-function
	      (byte-compile
	       (twittering-generate-format-status-function format-str)))
	(let ((current (get-buffer "*Compile-Log*")))
	  (when (and (null before) current (= 0 (buffer-size current)))
	    (kill-buffer current)))))
    (setq twittering-status-format format-str)))

(defun twittering-format-status (status &optional prefix)
  "Format a STATUS by using `twittering-format-status-function'.
Specification of FORMAT-STR is described in the document for the
variable `twittering-status-format'."
  (funcall twittering-format-status-function status prefix))

(defun twittering-format-status-for-redisplay (beg end status &optional prefix)
  (let* ((properties
	  (and beg
	       (apply 'append
		      (mapcar (lambda (prop)
				(let ((value (get-text-property beg prop)))
				  (when value
				    `(,prop ,value))))
			      '(id original-id)))))
	 (str (twittering-format-status status prefix)))
    ;; Restore properties.
    (when properties
      (add-text-properties 0 (length str) properties str))
    str))

(defun twittering-timer-action (func)
  (let ((buf (twittering-get-active-buffer-list)))
    (if (null buf)
	(twittering-stop)
      (funcall func)
      )))

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
	  (minibuffer-message mes) ;; Emacs23 or later
	(minibuffer-message (concat " (" mes ")")))
      )))

(defun twittering-setup-minibuffer ()
  (add-hook 'post-command-hook 'twittering-show-minibuffer-length t t))

(defun twittering-finish-minibuffer ()
  (remove-hook 'post-command-hook 'twittering-show-minibuffer-length t))

(defun twittering-status-not-blank-p (status)
  (with-temp-buffer
    (insert status)
    (goto-char (point-min))
    ;; skip user name
    (re-search-forward "^[\n\r \t]*@[a-zA-Z0-9_-]+\\([\n\r \t]+@[a-zA-Z0-9_-]+\\)*" nil t)
    (re-search-forward "[^\n\r \t]+" nil t)))

(defun twittering-update-status-from-minibuffer (&optional init-str reply-to-id username spec)
  (and (not (twittering-timeline-spec-is-direct-messages-p spec))
       (null init-str)
       twittering-current-hashtag
       (setq init-str (format " #%s " twittering-current-hashtag)))
  (let ((status init-str)
	(sign-str (if (twittering-timeline-spec-is-direct-messages-p spec)
		      nil
		    (twittering-sign-string)))
	(not-posted-p t)
	(prompt "status: ")
	(map minibuffer-local-map)
	(minibuffer-message-timeout nil))
    (define-key map (kbd "<f4>") 'twittering-tinyurl-replace-at-point)
    (when twittering-use-show-minibuffer-length
      (add-hook 'minibuffer-setup-hook 'twittering-setup-minibuffer t)
      (add-hook 'minibuffer-exit-hook 'twittering-finish-minibuffer t))
    (unwind-protect
	(while not-posted-p
	  (setq status (read-from-minibuffer prompt status map nil 'twittering-tweet-history nil t))
	  (let ((status-with-sign (concat status sign-str)))
	    (if (< 140 (length status-with-sign))
		(setq prompt "status (too long): ")
	      (setq prompt "status: ")
	      (when (twittering-status-not-blank-p status)
		(cond
		 ((twittering-timeline-spec-is-direct-messages-p spec)
		  (if username
		      (let ((parameters `(("user" . ,username)
					  ("text" . ,status))))
			(twittering-http-post twittering-api-host
					      "1/direct_messages/new"
					      parameters))
		    (message "No username specified")))
		 (t
		  (let ((parameters `(("status" . ,status-with-sign))))
		    ;; Add in_reply_to_status_id only when a posting
		    ;; status begins with @username.
		    (when (and reply-to-id
			       username
			       (string-match
				(concat "^@" username "\\(?:[\n\r \t]+\\)*")
				status))
		      (add-to-list 'parameters
				   `("in_reply_to_status_id" . ,reply-to-id)))
		    (twittering-http-post twittering-api-host
					  "1/statuses/update"
					  parameters))))
		(setq not-posted-p nil))
	      )))
      ;; unwindforms
      (when (memq 'twittering-setup-minibuffer minibuffer-setup-hook)
	(remove-hook 'minibuffer-setup-hook 'twittering-setup-minibuffer))
      (when (memq 'twittering-finish-minibuffer minibuffer-exit-hook)
	(remove-hook 'minibuffer-exit-hook 'twittering-finish-minibuffer))
      )))

(defun twittering-get-list-index (username)
  (twittering-http-get twittering-api-host
		       (concat "1/" username "/lists")
		       t nil nil
		       'twittering-http-get-list-index-sentinel))

(defun twittering-get-list-index-sync (username)
  (setq twittering-list-index-retrieved nil)
  (twittering-get-list-index username)
  (while (not twittering-list-index-retrieved)
    (sit-for 0.1))
  (cond
   ((stringp twittering-list-index-retrieved)
    (if (string= "" twittering-list-index-retrieved)
	(message "%s does not have a list." username)
      (message twittering-list-index-retrieved))
    nil)
   ((listp twittering-list-index-retrieved)
    twittering-list-index-retrieved)))

(defun twittering-manage-friendships (method username)
  (twittering-http-post twittering-api-host
			(concat "1/friendships/" method)
			`(("screen_name" . ,username))))

(defun twittering-manage-favorites (method id)
  (twittering-http-post twittering-api-host
			(concat "1/favorites/" method "/" id)))

(defun twittering-get-tweets (host method &optional noninteractive id since_id word)
  (let* ((default-count 20)
	 (do-search-flag (not (null word)))
	 (max-count (if do-search-flag
			100 ;; FIXME: refer to defconst.
		      twittering-max-number-of-tweets-on-retrieval))
	 (count twittering-number-of-tweets-on-retrieval)
	 (count (cond
		 ((integerp count) count)
		 ((string-match "^[0-9]+$" count)
		  (string-to-number count 10))
		 (t default-count)))
	 (count (min (max 1 count) max-count))
	 (regexp-list-method "^1/[^/]*/lists/[^/]*/statuses$")
	 (parameters nil))
    (cond ((stringp id)
	   (add-to-list 'parameters `("max_id" . ,id)))
	  ((stringp since_id)
	   (add-to-list 'parameters `("since_id" . ,since_id))))
    (cond
     (do-search-flag
      (add-to-list 'parameters `("q" . ,word))
      (add-to-list 'parameters `("rpp" . ,(number-to-string count)))
      (twittering-http-get host method noninteractive parameters "atom"))
     (t
      (add-to-list 'parameters
		   (cons (if (string-match regexp-list-method method)
			     "per_page"
			   "count")
			 (number-to-string count)))
      (twittering-http-get host method noninteractive parameters)))))

(defun twittering-get-and-render-timeline (&optional noninteractive id)
  (let ((spec (twittering-current-timeline-spec))
	(spec-string (twittering-current-timeline-spec-string)))
    (cond
     ((and noninteractive (twittering-process-active-p spec))
      ;; ignore non-interactive request if a process is waiting for responses.
      t)
     ((twittering-timeline-spec-primary-p spec)
      (let ((info (twittering-timeline-spec-to-host-method spec))
	    (is-search-spec (eq 'search (car spec))))
	(when info
	  (let* ((host (elt info 0))
		 (method (elt info 1))
		 ;; Assume that a list which was returned by
		 ;; `twittering-current-timeline-data' is sorted.
		 (since_id (or is-search-spec (cdr-safe (assq 'id (car (twittering-current-timeline-data spec))))))
		 (word (and is-search-spec (cadr spec)))
		 (proc (twittering-get-tweets host method noninteractive
					      id since_id word)))
	    (when proc
	      (twittering-register-process proc spec spec-string))))))
     (t
      (let ((type (car spec)))
	(error "%s has not been supported yet" type))))))

(defun twittering-retrieve-image (image-url)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'binary)
	  (coding-system-for-write 'binary)
	  (require-final-newline nil))
      (twittering-url-insert-file-contents image-url)
      (let ((image-type (twittering-image-type image-url (current-buffer))))
	(and twittering-use-convert
	     (or (not (image-type-available-p image-type))
		 (and (integerp twittering-convert-fix-size)
		      (not (let ((image-spec
				  (create-image (buffer-string) image-type t))
				 (converted-image-size
				  `(,twittering-convert-fix-size
				    . ,twittering-convert-fix-size)))
			     (equal (image-size image-spec t)
				    converted-image-size)))))
	     (let ((exit-status
		    (call-process-region
		     (point-min) (point-max)
		     twittering-convert-program
		     t t nil
		     (if image-type (format "%s:-" image-type) "-")
		     (when (integerp twittering-convert-fix-size)
		       "-resize")
		     (when (integerp twittering-convert-fix-size)
		       (format "%dx%d" twittering-convert-fix-size
			       twittering-convert-fix-size))
		     "xpm:-")))
	       (setq image-type (and (integerp exit-status)
				     (= 0 exit-status)
				     'xpm))))
	(if image-type
	    (let ((image-data `(,image-type . ,(buffer-string))))
	      (puthash `(,image-url . ,twittering-convert-fix-size)
		       image-data
		       twittering-image-data-table)
	      image-data)
	  nil)))))

(defun twittering-tinyurl-get (longurl)
  "Tinyfy LONGURL."
  (let ((api (cdr (assoc twittering-tinyurl-service
			 twittering-tinyurl-services-map))))
    (unless api
      (error "Invalid `twittering-tinyurl-service'. try one of %s"
	     (mapconcat (lambda (x)
			  (symbol-name (car x)))
			twittering-tinyurl-services-map ", ")))
    (if longurl
	(let ((buffer
	       (twittering-url-retrieve-synchronously (concat api longurl))))
	  (with-current-buffer buffer
	    (goto-char (point-min))
	    (prog1
		(if (search-forward-regexp "\n\r?\n\\([^\n\r]*\\)" nil t)
		    (match-string-no-properties 1)
		  (error "TinyURL failed: %s" longurl))
	      (kill-buffer buffer))))
      nil)))

;;;
;;; Commands
;;;

(defun twittering-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'twittering-update-active-buffers))
  (unless twittering-timer
    (setq twittering-timer
	  (run-at-time "0 sec"
		       twittering-timer-interval
		       #'twittering-timer-action action)))
  (unless twittering-timer-for-redisplaying
    (setq twittering-timer-for-redisplaying
	  (run-at-time "0 sec"
		       twittering-timer-interval-for-redisplaying
		       #'twittering-redisplay-status-on-buffer))))

(defun twittering-stop ()
  (interactive)
  (when twittering-timer
    (cancel-timer twittering-timer)
    (setq twittering-timer nil))
  (when twittering-timer-for-redisplaying
    (cancel-timer twittering-timer-for-redisplaying)
    (setq twittering-timer-for-redisplaying nil)))

(defun twittering-scroll-mode (&optional arg)
  (interactive "P")
  (let ((prev-mode twittering-scroll-mode))
    (setq twittering-scroll-mode
	  (if (null arg)
	      (not twittering-scroll-mode)
	    (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twittering-scroll-mode)
      (twittering-update-mode-line))))

(defun twittering-jojo-mode (&optional arg)
  (interactive "P")
  (let ((prev-mode twittering-jojo-mode))
    (setq twittering-jojo-mode
	  (if (null arg)
	      (not twittering-jojo-mode)
	    (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twittering-jojo-mode)
      (twittering-update-mode-line))))

(defun twittering-toggle-reverse-mode (&optional arg)
  (interactive "P")
  (let ((prev-mode twittering-reverse-mode))
    (setq twittering-reverse-mode
	  (if (null arg)
	      (not twittering-reverse-mode)
	    (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twittering-reverse-mode)
      (twittering-update-mode-line)
      (twittering-render-timeline (current-buffer)))))

(defun twittering-friends-timeline ()
  (interactive)
  (twittering-visit-timeline '(friends)))

(defun twittering-home-timeline ()
  (interactive)
  (twittering-visit-timeline '(home)))

(defun twittering-replies-timeline ()
  (interactive)
  (twittering-visit-timeline '(replies)))

(defun twittering-public-timeline ()
  (interactive)
  (twittering-visit-timeline '(public)))

(defun twittering-user-timeline ()
  (interactive)
  (twittering-visit-timeline `(user ,(twittering-get-username))))

(defun twittering-update-active-buffers (&optional noninteractive)
  "Invoke `twittering-get-and-render-timeline' for each active buffer
managed by `twittering-mode'."
  (let ((buffer-list (twittering-get-active-buffer-list)))
    (mapc (lambda (buffer)
	    (with-current-buffer buffer
	      (twittering-get-and-render-timeline noninteractive)))
	  buffer-list)))

(defun twittering-current-timeline-noninteractive ()
  (twittering-current-timeline t))

(defun twittering-current-timeline (&optional noninteractive)
  (interactive)
  (when (twittering-buffer-p)
    (let ((spec-string (twittering-current-timeline-spec-string)))
      (twittering-get-and-render-timeline noninteractive))))

(defun twittering-update-status-interactive ()
  (interactive)
  (funcall twittering-update-status-function))

(defun twittering-update-lambda ()
  (interactive)
  (when (and (string= "Japanese" current-language-environment)
	     (or (< 21 emacs-major-version)
		 (eq 'utf-8 (terminal-coding-system))))
    (twittering-http-post
     twittering-api-host "1/statuses/update"
     `(("status" . ,(mapconcat
		     'char-to-string
		     (mapcar 'twittering-ucs-to-char
			     '(955 12363 12431 12356 12356 12424 955)) ""))
       ))))

(defun twittering-update-jojo (usr msg)
  (when (and (string= "Japanese" current-language-environment)
	     (or (< 21 emacs-major-version)
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
	 twittering-api-host "1/statuses/update"
	 `(("status" . ,(concat
			 "@" usr " "
			 (match-string-no-properties 2 msg)
			 (mapconcat
			  'char-to-string
			  (mapcar 'twittering-ucs-to-char
				  '(12288 12399 12387 33 63)) "")))
	   )))))

(defun twittering-set-current-hashtag (&optional tag)
  (interactive)
  (unless tag
    (setq tag (twittering-completing-read "hashtag (blank to clear): #"
					  twittering-hashtag-history
					  nil nil
					  twittering-current-hashtag
					  'twittering-hashtag-history))
    (message
     (if (eq 0 (length tag))
	 (progn (setq twittering-current-hashtag nil)
		"Current hashtag is not set.")
       (progn
	 (setq twittering-current-hashtag tag)
	 (format "Current hashtag is #%s" twittering-current-hashtag))))))

(defun twittering-erase-old-statuses ()
  (interactive)
  (when (twittering-buffer-p)
    (let ((spec (twittering-current-timeline-spec)))
      (twittering-remove-timeline-data spec) ;; clear current timeline.
      (twittering-render-timeline (current-buffer) nil) ;; clear buffer.
      (twittering-get-and-render-timeline))))

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
	(spec (get-text-property (point) 'belongs-spec))
	(screen-name-in-text
	 (get-text-property (point) 'screen-name-in-text)))
    (cond (screen-name-in-text
	   (funcall twittering-update-status-function
		    (if (twittering-timeline-spec-is-direct-messages-p spec)
			nil
		      (concat "@" screen-name-in-text " "))
		    id screen-name-in-text spec))
	  (uri
	   (browse-url uri))
	  (username
	   (funcall twittering-update-status-function
		    (if (twittering-timeline-spec-is-direct-messages-p spec)
			nil
		      (concat "@" username " "))
		    id username spec)))))

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

(defun twittering-retweet (&optional arg)
  (interactive "P")
  (let ((use-native-retweet-flag (if arg
				     (not twittering-use-native-retweet)
				   twittering-use-native-retweet)))
    (if use-native-retweet-flag
	(twittering-native-retweet)
      (twittering-organic-retweet))))

(defun twittering-organic-retweet ()
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
	(funcall twittering-update-status-function
	 (twittering-format-string format-str prefix replace-table))
	))))

(defun twittering-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twittering-follow (&optional remove)
  (interactive "P")
  (let ((username (copy-sequence (get-text-property (point) 'username)))
	(method (if remove "destroy" "create"))
	(mes (if remove "Unfollowing" "Following")))
    (unless username
      (setq username (or (twittering-read-username-with-completion
			  "who: " "" 'twittering-user-history)
			 "")))
    (if (string= "" username)
	(message "No user selected")
      (set-text-properties 0 (length username) nil username)
      (if (y-or-n-p (format "%s %s? " mes username))
	  (twittering-manage-friendships method username)
	(message "Request canceled")))))

(defun twittering-unfollow ()
  (interactive)
  (twittering-follow t))

(defun twittering-native-retweet ()
  (interactive)
  (let ((id (get-text-property (point) 'id))
	(text (copy-sequence (get-text-property (point) 'text)))
	(width (max 40 ;; XXX
		    (- (frame-width)
		       1 ;; margin for wide characters
		       12 ;; == (length (concat "Retweet \"" "\"? "))
		       9) ;; == (length "(y or n) ")
		    )))
    (set-text-properties 0 (length text) nil text)
    (if id
	(let ((mes (format "Retweet \"%s\"? "
			   (if (< width (string-width text))
			       (concat
				(truncate-string-to-width text (- width 3))
				"...")
			     text))))
	  (if (y-or-n-p mes)
	      (twittering-http-post twittering-api-host
				    (concat "1/statuses/retweet/" id))
	    (message "Request canceled")))
      (message "No status selected"))))

(defun twittering-favorite (&optional remove)
  (interactive "P")
  (let ((id (get-text-property (point) 'id))
	(text (copy-sequence (get-text-property (point) 'text)))
	(width (max 40 ;; XXX
		    (- (frame-width)
		       1 ;; margin for wide characters
		       15 ;; == (length (concat "Unfavorite \"" "\"? "))
		       9) ;; == (length "(y or n) ")
		    ))
	(method (if remove "destroy" "create")))
    (set-text-properties 0 (length text) nil text)
    (if id
	(let ((mes (format "%s \"%s\"? "
			   (if remove "Unfavorite" "Favorite")
			   (if (< width (string-width text))
			       (concat
				(truncate-string-to-width text (- width 3))
				"...")
			     text))))
	  (if (y-or-n-p mes)
	      (twittering-manage-favorites method id)
	    (message "Request canceled")))
      (message "No status selected"))))

(defun twittering-unfavorite ()
  (interactive)
  (twittering-favorite t))

(defun twittering-visit-timeline (&optional timeline-spec initial)
  (interactive)
  (let ((timeline-spec
	 (or timeline-spec
	     (twittering-read-timeline-spec-with-completion
	      "timeline: " initial t))))
    (when timeline-spec
      (switch-to-buffer (twittering-get-managed-buffer timeline-spec)))))

(defun twittering-other-user-timeline ()
  (interactive)
  (let* ((username (get-text-property (point) 'username))
	 (goto-spec (get-text-property (point) 'goto-spec))
	 (screen-name-in-text
	  (get-text-property (point) 'screen-name-in-text))
	 (spec (cond (goto-spec goto-spec)
		     (screen-name-in-text `(user ,screen-name-in-text))
		     (username `(user ,username))
		     (t nil))))
    (if spec
	(twittering-visit-timeline spec)
      (message "No user selected"))))

(defun twittering-other-user-timeline-interactive ()
  (interactive)
  (let ((username (or (twittering-read-username-with-completion
		       "user: " nil
		       'twittering-user-history)
		      "")))
    (if (string= "" username)
	(message "No user selected")
      (twittering-visit-timeline `(user ,username)))))

(defun twittering-other-user-list-interactive ()
  (interactive)
  (let* ((username (copy-sequence (get-text-property (point) 'username)))
	 (username (progn
		     (set-text-properties 0 (length username) nil username)
		     (or (twittering-read-username-with-completion
			  "whose list: "
			  username
			  'twittering-user-history)
			 ""))))
    (if (string= "" username)
	(message "No user selected")
      (let* ((list-name (twittering-read-list-name username))
	     (spec `(list ,username ,list-name)))
	(if list-name
	    (twittering-visit-timeline spec)
	  ;; Don't show message here to prevent an overwrite of a
	  ;; message which is outputted by `twittering-read-list-name'.
	  )))))

(defun twittering-direct-message ()
  (interactive)
  (let ((username (twittering-read-username-with-completion
		   "who receive your message: "
		   (get-text-property (point) 'username)
		   'twittering-user-history))
	(spec (or (get-text-property (point) 'belongs-spec)
		  '(direct_messages))))
    (if (string= "" username)
	(message "No user selected")
      (funcall twittering-update-status-function
	       (if (twittering-timeline-spec-is-direct-messages-p spec)
		   nil
		 (concat "d " username " "))
	       nil username spec))))

(defun twittering-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(funcall twittering-update-status-function (concat "@" username " "))
      (message "No user selected"))))

(defun twittering-search (&optional word)
  (interactive)
  (let ((word (or word
		  (read-from-minibuffer "search: " nil nil nil
					'twittering-search-history nil t)
		  "")))
    (if (string= "" word)
	(message "No query string")
      (let ((spec `(search ,word)))
	(twittering-visit-timeline spec)))))

(defun twittering-get-usernames-from-timeline (&optional timeline-data)
  (let ((timeline-data (or timeline-data (twittering-current-timeline-data))))
    (twittering-remove-duplicates
     (mapcar
      (lambda (status)
	(let* ((base-str (cdr (assq 'user-screen-name status)))
	       ;; `copied-str' is independent of the string in timeline-data.
	       ;; This isolation is required for `minibuf-isearch.el',
	       ;; which removes the text properties of strings in history.
	       (copied-str (copy-sequence base-str)))
	  (set-text-properties 0 (length copied-str) nil copied-str)
	  copied-str))
      timeline-data))))

(defun twittering-read-username-with-completion (prompt init-user &optional history)
  (let ((collection (append twittering-user-history
			    (twittering-get-usernames-from-timeline))))
    (twittering-completing-read prompt collection nil nil init-user history)))

(defun twittering-read-list-name (username &optional list-index)
  (let* ((list-index (or list-index
			 (twittering-get-list-index-sync username)))
	 (username (prog1 (copy-sequence username)
		     (set-text-properties 0 (length username) nil username)))
	 (prompt (format "%s's list: " username))
	 (listname
	  (if list-index
	      (twittering-completing-read prompt list-index nil t nil)
	    nil)))
    (if (string= "" listname)
	nil
      listname)))

(defun twittering-read-timeline-spec-with-completion (prompt initial &optional as-string)
  (let* ((dummy-hist (append twittering-timeline-history
			     (twittering-get-usernames-from-timeline)))
	 (spec-string (twittering-completing-read prompt dummy-hist
						  nil nil initial 'dummy-hist))
	 (spec-string
	  (cond
	   ((string-match "^\\([a-zA-Z0-9_-]+\\)/$" spec-string)
	    (let* ((username (match-string 1 spec-string))
		   (list-index (twittering-get-list-index-sync username))
		   (listname
		    (if list-index
			(twittering-read-list-name username list-index)
		      nil)))
	      (if listname
		  (concat username "/" listname)
		nil)))
	   (t
	    spec-string)))
	 (spec (if (stringp spec-string)
		   (condition-case error-str
		       (twittering-string-to-timeline-spec spec-string)
		     (error
		      (message "Invalid timeline spec: %s" error-str)
		      nil))
		 nil)))
    (cond
     ((null spec)
      nil)
     (spec (if as-string
	       spec-string
	     spec))
     ((string= "" spec-string)
      (message "No timeline specs are specified.")
      nil)
     (t
      (message "\"%s\" is invalid as a timeline spec." spec-string)
      nil))))

(defun twittering-get-username ()
  twittering-username-active)

(defun twittering-get-password ()
  twittering-password-active)

(defun twittering-get-id-at (&optional pos)
  "Return ID of the status at POS. If a separator is rendered at POS, return
the ID of the status rendered before the separator. The default value of POS
is `(point)'."
  (let ((pos (or pos (point))))
    (or (get-text-property pos 'id)
	(let ((prev (or (twittering-get-previous-status-head pos)
			(point-min))))
	  (and prev (get-text-property prev 'id))))))

(defun twittering-get-current-status-head (&optional pos)
  "Return the head position of the status at POS. The default value of POS
is `(point)'."
  (let* ((pos (or pos (point)))
	 (id (twittering-get-id-at pos))
	 (prev-head (twittering-get-previous-status-head pos)))
    (if (null prev-head)
	(point-min)
      (let ((prev-id (and prev-head (twittering-get-id-at prev-head))))
	(if (twittering-status-id= id prev-id)
	    prev-head
	  (twittering-get-next-status-head prev-head))))))

(defun twittering-goto-first-status ()
  "Go to the first status."
  (interactive)
  (goto-char (or (twittering-get-first-status-head)
		 (point-min))))

(defun twittering-get-first-status-head ()
  "Return the head position of the first status in the current buffer.
Return nil if no statuses are rendered."
  (if (get-text-property (point-min) 'id)
      (point-min)
    (twittering-get-next-status-head (point-min))))

(defun twittering-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos (twittering-get-next-status-head)))
    (cond
     (pos
      (goto-char pos))
     (twittering-reverse-mode
      (message "The latest status."))
     (t
      (let ((id (or (get-text-property (point) 'id)
		    (let ((prev (twittering-get-previous-status-head)))
		      (when prev
			(get-text-property prev 'id))))))
        (when id
	  (message "Get more previous timeline...")
	  (twittering-get-and-render-timeline nil id)))))))

(defun twittering-get-next-status-head (&optional pos)
  "Search forward from POS for the nearest head of a status.
The return value is nil or a positive integer greater than POS."
  (let* ((pos (or pos (point)))
	 (pos (next-single-property-change pos 'id)))
    (if pos
	(if (get-text-property pos 'id)
	    pos
	  (next-single-property-change pos 'id))
	nil)))

(defun twittering-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((prev-pos (twittering-get-previous-status-head)))
    (cond
     (prev-pos
      (goto-char prev-pos))
     (twittering-reverse-mode
      (let ((id (or (get-text-property (point) 'id)
		    (let ((next (twittering-get-next-status-head)))
		      (when next
			(get-text-property next 'id))))))
	(when id
	  (message "Get more previous timeline...")
	  (twittering-get-and-render-timeline nil id))))
     (t
      (message "The latest status.")))))

(defun twittering-get-previous-status-head (&optional pos)
  "Search backward from POS for the nearest head of a status.
The return value is nil or a positive integer less than POS."
  (let ((current (or pos (point))))
    (if (eq current (point-min))
	nil
      (let ((previous (previous-single-property-change current 'id)))
	(cond
	 ((null previous)
	  (if (get-text-property (point-min) 'id)
	      (point-min)
	    nil))
	 ((get-text-property previous 'id) previous)
	 (t
	  ;; `previous' is not placed on either a status or (point-min).
	  ;; So, `previous-single-property-change' necessarily returns the
	  ;; position on a status if it succeeds.
	  (let ((previous (previous-single-property-change previous 'id)))
	    (if (null previous)
		(if (get-text-property (point-min) 'id)
		    (point-min)
		  nil)
	      previous))))))))

(defun twittering-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (twittering-get-username-at-pos (point)))
	(pos (twittering-get-next-status-head (point))))
    (while (and (not (eq pos nil))
		(not (equal (twittering-get-username-at-pos pos) user-name)))
      (setq pos (twittering-get-next-status-head pos)))
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
	(pos (twittering-get-previous-status-head (point))))
    (while (and (not (eq pos nil))
                (not (eq pos prev-pos))
		(not (equal (twittering-get-username-at-pos pos) user-name)))
      (setq prev-pos pos)
      (setq pos (twittering-get-previous-status-head pos)))
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
		   (remove nil (mapcar face-pred '(twittering-username-face
						   twittering-uri-face))))))
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

(defun twittering-suspend ()
  "Suspend twittering-mode then switch to another buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun twittering-scroll-up()
  "Scroll up if possible; otherwise invoke `twittering-goto-next-status',
which fetch older tweets on non reverse-mode."
  (interactive)
  (cond
   ((= (point) (point-max))
    (twittering-goto-next-status))
   ((= (window-end) (point-max))
    (goto-char (point-max)))
   (t
    (scroll-up))))

(defun twittering-scroll-down()
  "Scroll down if possible; otherwise invoke `twittering-goto-previous-status',
which fetch older tweets on reverse-mode."
  (interactive)
  (cond
   ((= (point) (point-min))
    (twittering-goto-previous-status))
   ((= (window-start) (point-min))
    (goto-char (point-min)))
   (t
    (scroll-down))))

;;;###autoload
(defun twit ()
  "Start twittering-mode."
  (interactive)
  (twittering-mode))

(provide 'twittering-mode)
;;; twittering.el ends here
