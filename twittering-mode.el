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
  (setq load-path
	(append (mapcar (lambda (dir)
			  (expand-file-name
			   dir
			   (if load-file-name
			       (or (file-name-directory load-file-name)
				   ".")
			     ".")))
			'("url-emacs21" "emacs21"))
		load-path))
  (and (require 'un-define nil t)
       ;; the explicitly require 'unicode to update a workaround with
       ;; navi2ch. see a comment of `twittering-ucs-to-char' for more
       ;; details.
       (require 'unicode nil t)))
(require 'url)

(defconst twittering-mode-version "HEAD")
(defconst twittering-mode-identity "$Id$")
(defvar twittering-api-host "api.twitter.com")
(defvar twittering-api-search-host "search.twitter.com")
(defvar twittering-web-host "twitter.com")
(defvar twittering-oauth-request-token-url
  "https://api.twitter.com/oauth/request_token")
(defvar twittering-oauth-authorization-url-base
  "https://api.twitter.com/oauth/authorize?oauth_token=")
(defvar twittering-oauth-access-token-url
  "https://api.twitter.com/oauth/access_token")

(defun twittering-mode-version ()
  "Display a message for twittering-mode version."
  (interactive)
  (let ((version-string
	 (format "twittering-mode-v%s" twittering-mode-version)))
    (if (interactive-p)
	(message "%s" version-string)
      version-string)))

(defvar twittering-auth-method 'oauth
  "*Authentication method for `twittering-mode'.
The symbol `basic' means Basic Authentication. The symbol `oauth' means
OAuth Authentication. OAuth Authentication requires
`twittering-oauth-consumer-key' and `twittering-oauth-consumer-secret'.
Additionally, it requires an external command `curl' or another command
included in `tls-program', which may be `openssl' or `gnutls-cli', for SSL.")

(defvar twittering-oauth-use-ssl t
  "*Whether to use SSL on authentication via OAuth. Twitter requires SSL
on authorization via OAuth.")
(defvar twittering-oauth-consumer-key nil)
(defvar twittering-oauth-consumer-secret nil)
(defvar twittering-oauth-access-token-alist nil)

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
  "*An username of your Twitter account.")

(defvar twittering-password nil
  "*A password of your Twitter account. Leave it blank is the
recommended way because writing a password in .emacs file is so
dangerous.")

(defvar twittering-initial-timeline-spec-string ":home"
  "*The initial timeline spec string. If the value of the variable is a
list of timeline spec strings, the timelines are rendered on their own
buffers.")

(defvar twittering-timeline-spec nil
  "The timeline spec for the current buffer.")
(defvar twittering-timeline-spec-string ""
  "The timeline spec string for the current buffer.")

(defvar twittering-timeline-spec-alias nil
  "*Alist for aliases of timeline spec.
Each element is (NAME . SPEC-STRING), where NAME is a string and
SPEC-STRING is a string or a function that returns a timeline spec string.

The alias can be referred as \"$NAME\" or \"$NAME(ARG)\" in timeline spec
string. If SPEC-STRING is a string, ARG is simply ignored.
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

(defvar twittering-active-mode nil
  "Non-nil if new statuses should be retrieved periodically.
Do not modify this variable directly. Use `twittering-activate-buffer',
`twittering-deactivate-buffer', `twittering-toggle-activate-buffer' or
`twittering-set-active-flag-for-buffer'.")
(defvar twittering-scroll-mode nil)

(defvar twittering-jojo-mode nil)
(defvar twittering-reverse-mode nil
  "*Non-nil means tweets are aligned in reverse order of `http://twitter.com/'.")
(defvar twittering-display-remaining nil
  "*If non-nil, display remaining of rate limit on the mode line.")
(defvar twittering-status-format "%i %s,  %@:\n%FILL[  ]{%T // from %f%L%r%R}\n "
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
 %FILL[prefix]{...} - strings filled as a paragraph. The prefix is optional.
                      You can use any other specifiers in braces.
 %FOLD[prefix]{...} - strings folded within the frame width.
                      The prefix is optional. This keeps newlines.
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
(defvar twittering-curl-program-https-capability nil
  "Cache a result of `twittering-start-http-session-curl-https-p'.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-tls-program nil
  "*List of strings containing commands to start TLS stream to a host.
Each entry in the list is tried until a connection is successful.
%h is replaced with server hostname, %p with port to connect to.
Also see `tls-program'.
If nil, this is initialized with a list of valied entries extracted from
`tls-program'.")

(defvar twittering-connection-type-order '(curl native))
  "*A list of connection methods in the preferred order."

(defvar twittering-connection-type-table
  '((native (check . t)
	    (https . twittering-start-http-session-native-tls-p)
	    (start . twittering-start-http-session-native)
	    (oauth-get-token . native))
    (curl (check . twittering-start-http-session-curl-p)
	  (https . twittering-start-http-session-curl-https-p)
	  (start . twittering-start-http-session-curl)
	  (oauth-get-token . curl)))
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
  'twittering-update-status-from-pop-up-buffer
  "The function used to posting a tweet. It takes two arguments:
the first argument INIT-STR is initial text to be edited and the
second argument REPLY-TO-ID is a user ID of a tweet to which you
are going to reply.

Twittering-mode provides two functions for updating status:
* `twittering-update-status-from-minibuffer': edit tweets in minibuffer
* `twittering-update-status-from-pop-up-buffer': edit tweets in pop-up buffer")

(defvar twittering-request-confirmation-on-posting nil
  "*If *non-nil*, confirmation will be requested on posting a tweet edited in
pop-up buffer.")

(defvar twittering-use-master-password nil
  "*Wheter to store private information encrypted with a master password.")
(defvar twittering-private-info-file
  (expand-file-name "~/.twittering-mode.gpg")
  "*File for storing encrypted private information when
`twittering-use-master-password' is non-nil.")
(defvar twittering-variables-stored-with-encryption
  '(twittering-oauth-access-token-alist))

;;;
;;; Abstract layer for Twitter API
;;;

(defun twittering-call-api (command args-alist &optional noninteractive)
  "Call Twitter API and return the process object for the request."
  (cond
   ((eq command 'retrieve-timeline)
    ;; Retrieve a timeline.
    (let* ((spec (cdr (assq 'timeline-spec args-alist)))
	   (spec-string (cdr (assq 'timeline-spec-string args-alist)))
	   (spec-type (car-safe spec))
	   (number (cdr (assq 'number args-alist)))
	   (number-str (number-to-string number))
	   (max_id (cdr (assq 'max_id args-alist)))
	   (since_id (cdr (assq 'since_id args-alist)))
	   (word (cdr (assq 'word args-alist)))
	   (parameters
	    `(,@(when max_id `(("max_id" . ,max_id)))
	      ,@(when since_id `(("since_id" . ,since_id)))
	      ,@(cond
		 ((eq spec-type 'search)
		  `(("q" . ,word)
		    ("rpp" . ,number-str)))
		 ((eq spec-type 'list)
		  `(("per_page" . ,number-str)))
		 (t
		  `(("count" . ,number-str))))))
	   (format (if (eq spec-type 'search)
		       "atom"
		     "xml"))
	   (simple-spec-list
	    '((direct_messages . "1/direct_messages")
	      (direct_messages_sent . "1/direct_messages/sent")
	      (friends . "1/statuses/friends_timeline")
	      (home . "1/statuses/home_timeline")
	      (mentions . "1/statuses/mentions")
	      (public . "1/statuses/public_timeline")
	      (replies . "1/statuses/replies")
	      (retweeted_by_me . "1/statuses/retweeted_by_me")
	      (retweeted_to_me . "1/statuses/retweeted_to_me")
	      (retweets_of_me . "1/statuses/retweets_of_me")
	      (search . "search")))
	   (host (cond ((eq spec-type 'search) twittering-api-search-host)
		       (t twittering-api-host)))
	   (method
	    (cond
	     ((eq spec-type 'user)
	      (let ((username (elt spec 1)))
		(concat "1/statuses/user_timeline/" username)))
	     ((eq spec-type 'list)
	      (let ((username (elt spec 1))
		    (list-name (elt spec 2)))
		(concat "1/" username "/lists/" list-name "/statuses")))
	     ((assq spec-type simple-spec-list)
	      (cdr (assq spec-type simple-spec-list)))
	     (t nil))))
      (if (and host method)
	  (twittering-http-get host method noninteractive parameters format)
	(error "Invalid timeline spec"))))
   ((eq command 'get-list-index)
    ;; Get list names.
    (let ((username (cdr (assq 'username args-alist)))
	  (sentinel (cdr (assq 'sentinel args-alist))))
      (twittering-http-get twittering-api-host
			   (concat "1/" username "/lists")
			   t nil nil sentinel)))
   ((eq command 'create-friendships)
    ;; Create a friendship.
    (let ((username (cdr (assq 'username args-alist))))
      (twittering-http-post twittering-api-host
			    "1/friendships/create"
			    `(("screen_name" . ,username)))))
   ((eq command 'destroy-friendships)
    ;; Destroy a friendship
    (let ((username (cdr (assq 'username args-alist))))
      (twittering-http-post twittering-api-host
			    "1/friendships/destroy"
			    `(("screen_name" . ,username)))))
   ((eq command 'create-favorites)
    ;; Create a favorite.
    (let ((id (cdr (assq 'id args-alist))))
      (twittering-http-post twittering-api-host
			    (concat "1/favorites/create/" id))))
   ((eq command 'destroy-favorites)
    ;; Destroy a favorite.
    (let ((id (cdr (assq 'id args-alist))))
      (twittering-http-post twittering-api-host
			    (concat "1/favorites/destroy/" id))))
   ((eq command 'update-status)
    ;; Post a tweet.
    (let* ((status (cdr (assq 'status args-alist)))
	   (id (cdr (assq 'in-reply-to-status-id args-alist)))
	   (parameters
	    `(("status" . ,status)
	      ,@(when id `(("in_reply_to_status_id" . ,id))))))
      (twittering-http-post twittering-api-host "1/statuses/update"
			    parameters)))
   ((eq command 'destroy-status)
    ;; Destroy a status.
    (let ((id (cdr (assq 'id args-alist))))
      (twittering-http-post twittering-api-host
			    "1/statuses/destroy"
			    `(("id" . ,id)))))
   ((eq command 'retweet)
    ;; Post a retweet.
    (let ((id (cdr (assq 'id args-alist))))
      (twittering-http-post twittering-api-host
			    (concat "1/statuses/retweet/" id))))
   ((eq command 'verify-credentials)
    ;; Verify the account.
    (let ((sentinel (cdr (assq 'sentinel args-alist))))
      (twittering-http-get twittering-api-host
			   "1/account/verify_credentials"
			   t nil nil
			   sentinel)))
   ((eq command 'send-direct-message)
    ;; Send a direct message.
    (let ((parameters
	   `(("user" . ,(cdr (assq 'username args-alist)))
	     ("text" . ,(cdr (assq 'status args-alist))))))
      (twittering-http-post twittering-api-host "1/direct_messages/new"
			    parameters)))
   (t
    nil)))

;;;
;;; Proxy setting / functions
;;;

(defvar twittering-proxy-use nil)
(defvar twittering-proxy-server nil
  "*Proxy server for `twittering-mode'.
If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.

To use individual proxies for HTTP and HTTPS, both `twittering-proxy-server'
and `twittering-proxy-port' must be nil.")
(defvar twittering-proxy-port nil
  "*Port number for `twittering-mode'.
If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.

To use individual proxies for HTTP and HTTPS, both `twittering-proxy-server'
and `twittering-proxy-port' must be nil.")
(defvar twittering-proxy-keep-alive nil)
(defvar twittering-proxy-user nil
  "*Username for `twittering-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-proxy-password nil
  "*Password for `twittering-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")

(defvar twittering-http-proxy-server nil
  "*HTTP proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
The port number is specified by `twittering-http-proxy-port'.
For HTTPS connection, the proxy specified by `twittering-https-proxy-server'
and `twittering-https-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-http-proxy-port nil
  "*Port number of a HTTP proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
The server is specified by `twittering-http-proxy-server'.
For HTTPS connection, the proxy specified by `twittering-https-proxy-server'
and `twittering-https-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-http-proxy-keep-alive nil
  "*If non-nil, the Keep-alive is enabled. This is experimental.")
(defvar twittering-http-proxy-user nil
  "*Username for `twittering-http-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-http-proxy-password nil
  "*Password for `twittering-http-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")

(defvar twittering-https-proxy-server nil
  "*HTTPS proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
The port number is specified by `twittering-https-proxy-port'.
For HTTP connection, the proxy specified by `twittering-http-proxy-server'
and `twittering-http-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-https-proxy-port nil
  "*Port number of a HTTPS proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
The server is specified by `twittering-https-proxy-server'.
For HTTP connection, the proxy specified by `twittering-http-proxy-server'
and `twittering-http-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-https-proxy-keep-alive nil
  "*If non-nil, the Keep-alive is enabled. This is experimental.")
(defvar twittering-https-proxy-user nil
  "*Username for `twittering-https-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-https-proxy-password nil
  "*Password for `twittering-https-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")

(defun twittering-normalize-proxy-vars ()
  "Normalize the type of `twittering-http-proxy-port' and
`twittering-https-proxy-port'."
  (mapc (lambda (sym)
	  (let ((value (symbol-value sym)))
	    (cond
	     ((null value)
	      nil)
	     ((integerp value)
	      nil)
	     ((stringp value)
	      (set sym (string-to-number value)))
	     (t
	      (set sym nil)))))
	'(twittering-proxy-port
	  twittering-http-proxy-port
	  twittering-https-proxy-port)))

(defun twittering-proxy-info (scheme &optional item)
  "Return an alist for proxy configuration registered for SCHEME.
SCHEME must be a string \"http\", \"https\" or a symbol 'http or 'https.
The server name is a string and the port number is an integer."
  (twittering-normalize-proxy-vars)
  (let ((scheme (if (symbolp scheme)
		    (symbol-name scheme)
		  scheme))
	(info-list
	 `((("http" "https")
	    . ((server . ,twittering-proxy-server)
	       (port . ,twittering-proxy-port)
	       (keep-alive . ,twittering-proxy-keep-alive)
	       (user . ,twittering-proxy-user)
	       (password . ,twittering-proxy-password)))
	   (("http")
	    . ((server . ,twittering-http-proxy-server)
	       (port . ,twittering-http-proxy-port)
	       (keep-alive . ,twittering-http-proxy-keep-alive)
	       (user . ,twittering-http-proxy-user)
	       (password . ,twittering-http-proxy-password)))
	   (("https")
	    . ((server . ,twittering-https-proxy-server)
	       (port . ,twittering-https-proxy-port)
	       (keep-alive . ,twittering-https-proxy-keep-alive)
	       (user . ,twittering-https-proxy-user)
	       (password . ,twittering-https-proxy-password))))))
    (let ((info
	   (car (remove nil
			(mapcar
			 (lambda (entry)
			   (when (member scheme (car entry))
			     (let ((info (cdr entry)))
			       (when (and (cdr (assq 'server info))
					  (cdr (assq 'port info)))
				 info))))
			 info-list)))))
      (if item
	  (cdr (assq item info))
	info))))

(defun twittering-url-proxy-services ()
  "Return the current proxy configuration for `twittering-mode' in the format
of `url-proxy-services'."
  (remove nil (mapcar
	       (lambda (scheme)
		 (let ((server (twittering-proxy-info scheme 'server))
		       (port (twittering-proxy-info scheme 'port)))
		   (when (and server port)
		     `(,scheme . ,(format "%s:%s" server port)))))
	       '("http" "https"))))

(defun twittering-find-proxy (scheme)
  "Find proxy server and its port from the environmental variables and return
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
  (unless (and twittering-http-proxy-server
	       twittering-http-proxy-port)
    (let ((info (twittering-find-proxy "http")))
      (setq twittering-http-proxy-server (car-safe info))
      (setq twittering-http-proxy-port (cdr-safe info))))
  (unless (and twittering-https-proxy-server
	       twittering-https-proxy-port)
    (let ((info (twittering-find-proxy "https")))
      (setq twittering-https-proxy-server (car-safe info))
      (setq twittering-https-proxy-port (cdr-safe info))))
  (if (and twittering-proxy-use
	   (null (twittering-proxy-info "http"))
	   (null (twittering-proxy-info "https")))
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
;;; OAuth
;;;

(defvar twittering-oauth-get-token-function-table
  '((native . twittering-oauth-get-token-alist-native)
    (curl . twittering-oauth-get-token-alist-curl)
    (url . twittering-oauth-get-token-alist-url))
  "Alist of functions used for getting a token on OAuth.")

(defvar twittering-oauth-get-token-function-type 'curl
  "*Symbol specifying function type used for getting a token on OAuth.
The function corresponding to the symbol is determined by
`twittering-oauth-get-token-function-table'.")

(defvar twittering-oauth-invoke-browser nil
  "*Whether to invoke a browser on authorization of access key automatically.")

(defun twittering-oauth-url-encode (str &optional coding-system)
  "Encode string according to Percent-Encoding defined in RFC 3986."
  (let ((coding-system (or (when (and coding-system
				      (coding-system-p coding-system))
			     coding-system)
			   'utf-8)))
    (mapconcat
     (lambda (c)
       (cond
	((or (and (<= ?A c) (<= c ?Z))
	     (and (<= ?a c) (<= c ?z))
	     (and (<= ?0 c) (<= c ?9))
	     (eq ?. c)
	     (eq ?- c)
	     (eq ?_ c)
	     (eq ?~ c))
	 (char-to-string c))
	(t (format "%%%02X" c))))
     (encode-coding-string str coding-system)
     "")))

(defun twittering-oauth-unhex (c)
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (- c ?0))
   ((and (<= ?A c) (<= c ?F))
    (+ 10 (- c ?A)))
   ((and (<= ?a c) (<= c ?f))
    (+ 10 (- c ?a)))
   ))

(defun twittering-oauth-url-decode (str &optional coding-system)
  (let* ((coding-system (or (when (and coding-system
				       (coding-system-p coding-system))
			      coding-system)
			    'utf-8))
	 (substr-list (split-string str "%"))
	 (head (car substr-list))
	 (tail (cdr substr-list)))
    (decode-coding-string
     (concat
      head
      (mapconcat
       (lambda (substr)
	 (if (string-match "\\`\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\(.*\\)\\'"
			   substr)
	     (let* ((c1 (string-to-char (match-string 1 substr)))
		    (c0 (string-to-char (match-string 2 substr)))
		    (tail (match-string 3 substr))
		    (ch (+ (* 16 (twittering-oauth-unhex c1))
			   (twittering-oauth-unhex c0))))
	       (concat (char-to-string ch) tail))
	   substr))
       tail
       ""))
     coding-system)))

(defun twittering-oauth-make-signature-base-string (method base-url parameters)
  ;; "OAuth Core 1.0a"
  ;; http://oauth.net/core/1.0a/#anchor13
  (let* ((sorted-parameters (copy-sequence parameters))
	 (sorted-parameters
	  (sort sorted-parameters
		(lambda (entry1 entry2)
		  (string< (car entry1) (car entry2))))))
    (concat
     method
     "&"
     (twittering-oauth-url-encode base-url)
     "&"
     (mapconcat
      (lambda (entry)
	(let ((key (car entry))
	      (value (cdr entry)))
	  (concat (twittering-oauth-url-encode key)
		  "%3D"
		  (twittering-oauth-url-encode value))))
      sorted-parameters
      "%26"))))

(defun twittering-oauth-make-random-string (len)
  (let* ((table
	  (concat
	   "0123456789"
	   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	   "abcdefghijklmnopqrstuvwxyz"))
	 (n (length table))
	 (l 0)
	 (result (make-string len ?0)))
    (while (< l len)
      (aset result l (aref table (random n)))
      (setq l (1+ l)))
    result))

;;;
;;; The below function is derived from `hmac-sha1' retrieved
;;; from http://www.emacswiki.org/emacs/HmacShaOne.
;;;
(defun twittering-hmac-sha1 (key message)
  "Return an HMAC-SHA1 authentication code for KEY and MESSAGE.

KEY and MESSAGE must be unibyte strings.  The result is a unibyte
string.  Use the function `encode-hex-string' or the function
`base64-encode-string' to produce human-readable output.

See URL:<http://en.wikipedia.org/wiki/HMAC> for more information
on the HMAC-SHA1 algorithm.

The Emacs multibyte representation actually uses a series of
8-bit values under the hood, so we could have allowed multibyte
strings as arguments.  However, internal 8-bit values don't
correspond to any external representation \(at least for major
version 22).  This makes multibyte strings useless for generating
hashes.

Instead, callers must explicitly pick and use an encoding for
their multibyte data.  Most callers will want to use UTF-8
encoding, which we can generate as follows:

  (let ((unibyte-key   (encode-coding-string key   'utf-8 t))
        (unibyte-value (encode-coding-string value 'utf-8 t)))
    (twittering-hmac-sha1 unibyte-key unibyte-value))

For keys and values that are already unibyte, the
`encode-coding-string' calls just return the same string."
;;; Return an HMAC-SHA1 authentication code for KEY and MESSAGE.
;;; 
;;; KEY and MESSAGE must be unibyte strings.  The result is a unibyte
;;; string.  Use the function `encode-hex-string' or the function
;;; `base64-encode-string' to produce human-readable output.
;;; 
;;; See URL:<http://en.wikipedia.org/wiki/HMAC> for more information
;;; on the HMAC-SHA1 algorithm.
;;; 
;;; The Emacs multibyte representation actually uses a series of
;;; 8-bit values under the hood, so we could have allowed multibyte
;;; strings as arguments.  However, internal 8-bit values don't
;;; correspond to any external representation \(at least for major
;;; version 22).  This makes multibyte strings useless for generating
;;; hashes.
;;; 
;;; Instead, callers must explicitly pick and use an encoding for
;;; their multibyte data.  Most callers will want to use UTF-8
;;; encoding, which we can generate as follows:
;;; 
;;; (let ((unibyte-key   (encode-coding-string key   'utf-8 t))
;;;       (unibyte-value (encode-coding-string value 'utf-8 t)))
;;; (hmac-sha1 unibyte-key unibyte-value))
;;; 
;;; For keys and values that are already unibyte, the
;;; `encode-coding-string' calls just return the same string.
;;;
;;; Author: Derek Upham - sand (at) blarg.net
;;;
;;; Copyright: This code is in the public domain.
  (require 'sha1)
  (when (multibyte-string-p key)
    (error "key must be unibyte"))
  (when (multibyte-string-p message)
    (error "message must be unibyte"))

  ;; The key block is always exactly the block size of the hash
  ;; algorithm.  If the key is too small, we pad it with zeroes (or
  ;; instead, we initialize the key block with zeroes and copy the
  ;; key onto the nulls).  If the key is too large, we run it
  ;; through the hash algorithm and use the hashed value (strange
  ;; but true).

  (let ((+hmac-sha1-block-size-bytes+ 64)) ; SHA-1 uses 512-bit blocks
    (when (< +hmac-sha1-block-size-bytes+ (length key))
      (setq key (sha1 key nil nil t)))

    (let ((key-block (make-vector +hmac-sha1-block-size-bytes+ 0)))
      (dotimes (i (length key))
	(aset key-block i (aref key i)))

      (let ((opad (make-vector +hmac-sha1-block-size-bytes+ #x5c))
	    (ipad (make-vector +hmac-sha1-block-size-bytes+ #x36)))

	(dotimes (i +hmac-sha1-block-size-bytes+)
	  (aset ipad i (logxor (aref ipad i) (aref key-block i)))
	  (aset opad i (logxor (aref opad i) (aref key-block i))))

	(when (fboundp 'unibyte-string)
	  ;; `concat' of Emacs23 (and later?) generates a multi-byte
	  ;; string from a vector of characters with eight bit.
	  ;; Since `opad' and `ipad' must be unibyte, we have to
	  ;; convert them by using `unibyte-string'.
	  ;; We cannot use `string-as-unibyte' here because it encodes
	  ;; bytes with the manner of UTF-8.
	  (setq opad (apply 'unibyte-string (mapcar 'identity opad)))
	  (setq ipad (apply 'unibyte-string (mapcar 'identity ipad))))

	(sha1 (concat opad
		      (sha1 (concat ipad message)
			    nil nil t))
	      nil nil t)))))

(defun twittering-oauth-auth-str (method base-url query-parameters oauth-parameters key)
  "Generate the value for HTTP Authorization header on OAuth.
QUERY-PARAMETERS is an alist for query parameters, where name and value
must be encoded into the same as they will be sent."
  (let* ((parameters (append query-parameters oauth-parameters))
	 (base-string
	  (twittering-oauth-make-signature-base-string method base-url parameters))
	 (key (if (multibyte-string-p key)
		  (string-make-unibyte key)
		key))
	 (base-string (if (multibyte-string-p base-string)
			  (string-make-unibyte base-string)
			base-string))
	 (signature
	  (base64-encode-string (twittering-hmac-sha1 key base-string))))
    (concat
     "OAuth "
     (mapconcat
      (lambda (entry)
	(concat (car entry) "=\"" (cdr entry) "\""))
      oauth-parameters
      ",")
     ",oauth_signature=\"" (twittering-oauth-url-encode signature) "\"")))

(defun twittering-oauth-auth-str-request-token (url query-parameters consumer-key consumer-secret &optional oauth-parameters)
  (let ((key (concat consumer-secret "&"))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
	       ("oauth_callback" . "oob")
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_consumer_key" . ,consumer-key)
	       ("oauth_version" . "1.0")))))
    (twittering-oauth-auth-str "POST" url query-parameters oauth-params key)))

(defun twittering-oauth-auth-str-exchange-token (url query-parameters consumer-key consumer-secret request-token request-token-secret verifier &optional oauth-parameters)
  (let ((key (concat consumer-secret "&" request-token-secret))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_consumer_key" . ,consumer-key)
	       ("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_version" . "1.0")
	       ("oauth_token" . ,request-token)
	       ("oauth_verifier" . ,verifier)))))
    (twittering-oauth-auth-str "POST" url query-parameters oauth-params key)))

(defun twittering-oauth-auth-str-access (method url query-parameters consumer-key consumer-secret access-token access-token-secret &optional oauth-parameters)
  "Generate a string for Authorization in HTTP header on OAuth.
METHOD means HTTP method such as \"GET\", \"POST\", etc. URL means a simple
URL without port number and query parameters.
QUERY-PARAMETERS means an alist of query parameters such as
'((\"status\" . \"test%20tweet\")
  (\"in_reply_to_status_id\" . \"12345678\")),
where name and value must be encoded into the same as they will be sent.
CONSUMER-KEY and CONSUMER-SECRET specifies the consumer.
ACCESS-TOKEN and ACCESS-TOKEN-SECRET must be authorized before calling this
function."
  (let ((key (concat consumer-secret "&" access-token-secret))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_consumer_key" . ,consumer-key)
	       ("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_version" . "1.0")
	       ("oauth_token" . ,access-token)))))
    (twittering-oauth-auth-str method url query-parameters oauth-params key)))

;; "OAuth Core 1.0a"
;; http://oauth.net/core/1.0a/#response_parameters
(defun twittering-oauth-make-response-alist (str)
  (mapcar
   (lambda (entry)
     (let* ((pair (split-string entry "="))
	    (name-entry (car pair))
	    (value-entry (cadr pair))
	    (name (and name-entry (twittering-oauth-url-decode name-entry)))
	    (value (and value-entry
			(twittering-oauth-url-decode value-entry))))
       `(,name . ,value)))
   (split-string str "&")))

(defun twittering-oauth-get-response-alist (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (and twittering-proxy-use twittering-oauth-use-ssl)
      ;; When using SSL via a proxy with CONNECT method,
      ;; omit a successful HTTP response and headers if they seem to be
      ;; sent from the proxy.
      (save-excursion
	(goto-char (point-min))
	(let ((first-regexp
	       ;; successful HTTP response
	       "\\`HTTP/1\.[01] 2[0-9][0-9] .*?\r?\n")
	      (next-regexp
	       ;; following HTTP response
	       "^\\(\r?\n\\)HTTP/1\.[01] [0-9][0-9][0-9] .*?\r?\n"))
	  (when (and (search-forward-regexp first-regexp nil t)
		     (search-forward-regexp next-regexp nil t))
	    (let ((beg (point-min))
		  (end (match-end 1)))
	      (delete-region beg end))))))
    (when (search-forward-regexp
	   "\\`\\(\\(HTTP/1\.[01]\\) \\([0-9][0-9][0-9]\\) \\(.*?\\)\\)\r?\n"
	   nil t)
      (let ((status-line (match-string 1))
	    (http-version (match-string 2))
	    (status-code (match-string 3))
	    (reason-phrase (match-string 4)))
	(cond
	 ((not (string-match "2[0-9][0-9]" status-code))
	  (message "Response: %s" status-line)
	  nil)
	 ((search-forward-regexp "\r?\n\r?\n" nil t)
	  (let ((beg (match-end 0))
		(end (if (search-forward-regexp "closed\r?\n\\'" nil t)
			 (match-beginning 0)
		       (point-max))))
	    (twittering-oauth-make-response-alist (buffer-substring beg end))))
	 (t
	  (message "Response: %s" status-line)
	  nil))))))

(defun twittering-oauth-get-token-alist-url (url auth-str)
  (let* ((url-request-method "POST")
	 (url-request-extra-headers
	  `(("Authorization" . ,auth-str)
	    ("Accept-Charset" . "us-ascii")
	    ("Content-Type" . "application/x-www-form-urlencoded")
	    ("Content-Length" . "0")
	    ))
	 (coding-system-for-read 'utf-8-unix))
    (lexical-let ((result 'queried))
      (let ((buffer
	     (url-retrieve
	      url
	      (lambda (&rest args)
		(let* ((status (if (< 21 emacs-major-version)
				   (car args)
				 nil))
		       (callback-args (if (< 21 emacs-major-version)
					  (cdr args)
					args))
		       (response-buffer (current-buffer)))
		  (setq result
			(twittering-oauth-get-response-alist response-buffer))
		  )))))
	(while (eq result 'queried)
	  (sit-for 0.1))
	(unless twittering-debug-mode
	  (kill-buffer buffer))
	result))))

(defun twittering-oauth-get-token-alist-native (url auth-str)
  (let* ((method "POST")
	 (url-parts (url-generic-parse-url url))
	 (scheme (and url-parts (aref url-parts 0)))
	 (host (and url-parts (aref url-parts 3)))
	 (port (and url-parts (aref url-parts 4)))
	 (path (and url-parts (aref url-parts 5)))
	 (proxy-info
	  (when twittering-proxy-use
	    (twittering-proxy-info scheme)))
	 (connect-host (if proxy-info
			   (cdr (assq 'server proxy-info))
			 host))
	 (connect-port (if proxy-info
			   (cdr (assq 'port proxy-info))
			 port))
	 (headers
	  `(("Authorization" . ,auth-str)
	    ("Accept-Charset" . "us-ascii")
	    ("Content-Type" . "application/x-www-form-urlencoded")
	    ("Content-Length" . "0")
	    ("Host" . ,host)))
	 (request-str
	  (format "%s %s HTTP/1.1\r\n%s\r\n\r\n"
		  method path
		  (mapconcat (lambda (pair)
			       (format "%s: %s" (car pair) (cdr pair)))
			     headers "\r\n"))))
    (with-temp-buffer
      (let* ((coding-system-for-read 'utf-8-unix)
	     (proc
	      (funcall (if twittering-oauth-use-ssl
			   'open-tls-stream
			 'open-network-stream)
		       "network-connection-process"
		       nil connect-host connect-port)))
	(when proc
	  (set-process-buffer proc (current-buffer))
	  (lexical-let ((result 'queried))
	    (set-process-sentinel
	     proc
	     (lambda (&rest args)
	       (let* ((proc (car args))
		      (buffer (process-buffer proc)))
		 (when buffer
		   (when twittering-debug-mode
		     (with-current-buffer (twittering-debug-buffer)
		       (insert-buffer-substring buffer)))
		   (setq result
			 (twittering-oauth-get-response-alist buffer))))))
	    (process-send-string proc request-str)
	    (while (eq result 'queried)
	      (sit-for 0.1))
	    result))))))

(defun twittering-oauth-get-token-alist-curl (url auth-str)
  (let* ((url-parts (url-generic-parse-url url))
	 (scheme (and url-parts (aref url-parts 0)))
	 (host (and url-parts (aref url-parts 3)))
	 (port (and url-parts (aref url-parts 4)))
	 (path (and url-parts (aref url-parts 5)))
	 (headers
	  `(("Authorization" . ,auth-str)
	    ("Accept-Charset" . "us-ascii")
	    ("Content-Type" . "application/x-www-form-urlencoded")
	    ("Content-Length" . "0")))
	 (cacert-fullpath (when twittering-oauth-use-ssl
			    (twittering-ensure-ca-cert)))
	 (cacert-dir (when cacert-fullpath
		       (file-name-directory cacert-fullpath)))
	 (cacert-filename (when cacert-fullpath
			    (file-name-nondirectory cacert-fullpath)))
	 (curl-args
	  `("--include" "--silent"
	    ,@(mapcan (lambda (pair)
			`("-H" ,(format "%s: %s" (car pair) (cdr pair))))
		      headers)
	    ,@(when twittering-oauth-use-ssl
		`("--cacert" ,cacert-filename))
	    ,@(when twittering-proxy-use
		(let* ((host (twittering-proxy-info scheme 'server))
		       (port (twittering-proxy-info scheme 'port)))
		  (when (and host port)
		    `("-x" ,(format "%s:%s" host port)))))
	    ,@(when twittering-proxy-use
		(let ((pair
		       (cond
			((string= scheme "https")
			 `(,twittering-https-proxy-user
			   . ,twittering-https-proxy-password))
			((string= scheme "http")
			 `(,twittering-http-proxy-user
			   . ,twittering-http-proxy-password))
			(t
			 nil))))
		  (when (and pair (car pair) (cdr pair))
		    `("-U" ,(format "%s:%s" (car pair) (cdr pair))))))
	    ;; HTTP method must be POST for getting a request token.
	    "-d" ""
	    ,url)))
    (with-temp-buffer
      (let* ((coding-system-for-read 'utf-8-unix)
	     (default-directory
	       ;; If `twittering-oauth-use-ssl' is non-nil, the `curl' process
	       ;; is executed at the same directory as the temporary cert file.
	       ;; Without changing directory, `curl' misses the cert file if
	       ;; you use Emacs on Cygwin because the path on Emacs differs
	       ;; from Windows.
	       ;; With changing directory, `curl' on Windows can find the cert
	       ;; file if you use Emacs on Cygwin.
	       (if twittering-oauth-use-ssl
		   cacert-dir
		 default-directory))
	     (proc (apply 'start-process "*twmode-curl*" (current-buffer)
			  twittering-curl-program curl-args)))
	(when proc
	  (lexical-let ((result 'queried))
	    (set-process-sentinel
	     proc
	     (lambda (&rest args)
	       (let* ((proc (car args))
		      (buffer (process-buffer proc)))
		 (when buffer
		   (when twittering-debug-mode
		     (with-current-buffer (twittering-debug-buffer)
		       (insert-buffer-substring buffer)))
		   (setq result
			 (twittering-oauth-get-response-alist buffer))))))
	    (while (eq result 'queried)
	      (sit-for 0.1))
	    result))))))

(defun twittering-oauth-get-token-alist (url auth-str)
  (let ((func (cdr (assq twittering-oauth-get-token-function-type
			 twittering-oauth-get-token-function-table))))
    (when (and func (functionp func))
      (funcall func url auth-str))))

(defun twittering-oauth-get-request-token (url consumer-key consumer-secret)
  (let ((auth-str
	 (twittering-oauth-auth-str-request-token
	  url nil consumer-key consumer-secret)))
    (twittering-oauth-get-token-alist url auth-str)))

(defun twittering-oauth-exchange-request-token (url consumer-key consumer-secret request-token request-token-secret verifier)
  (let ((auth-str
	 (twittering-oauth-auth-str-exchange-token
	  url nil
	  consumer-key consumer-secret
	  request-token request-token-secret verifier)))
    (twittering-oauth-get-token-alist url auth-str)))

(defun twittering-oauth-get-access-token (request-token-url authorize-url-func access-token-url consumer-key consumer-secret consumer-name)
  "Return an alist of authorized access token.
The function retrieves a request token from the site specified by
REQUEST-TOKEN-URL. Then, The function asks a WWW browser to authorize the
token by calling `browse-url'. The URL for authorization is calculated by
calling AUTHORIZE-URL-FUNC with the request token as an argument.
AUTHORIZE-URL-FUNC is called as `(funcal AUTHORIZE-URL-FUNC request-token)',
where the request-token is a string.
After calling `browse-url', the function waits for user to input the PIN code
that is displayed in the browser. The request token is authorized by the
PIN code, and then it is exchanged for the access token on the site
specified by ACCESS-TOKEN-URL.
CONSUMER-KEY and CONSUMER-SECRET specify the consumer.
CONSUMER-NAME is displayed at the guide of authorization.

The access token is returned as a list of a cons pair of name and value
like following:
 ((\"oauth_token\"
  . \"819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw\")
  (\"oauth_token_secret\"
   . \"J6zix3FfA9LofH0awS24M3HcBYXO5nI1iYe8EfBA\")
  (\"user_id\" . \"819797\")
  (\"screen_name\" . \"episod\"))
."
  (let* ((request-token-alist
	  (twittering-oauth-get-request-token
	   request-token-url consumer-key consumer-secret))
	 (request-token (cdr (assoc "oauth_token" request-token-alist)))
	 (request-token-secret
	  (cdr (assoc "oauth_token_secret" request-token-alist)))
	 (authorize-url (funcall authorize-url-func request-token))
	 (str
	  (concat
	   (propertize "Authorization via OAuth\n" 'face 'bold)
	   "\n"
	   "1.Allow access by " consumer-name " on the below site.\n"
	   "\n  "
	   (propertize authorize-url 'url authorize-url 'face 'bold)
	   "\n"
	   "\n"
	   (when twittering-oauth-invoke-browser
	     (concat
	      "  Emacs invokes your browser by the function `browse-url'.\n"
	      "  If the site is not opened automatically, you have to open\n"
	      "  the site manually.\n"
	      "\n"))
	   "2.After allowing access, the site will display the PIN code."
	   "\n"
	   "  Input the PIN code "
	   (propertize "at the below minibuffer." 'face 'bold))))
    (when request-token-alist
      (with-temp-buffer
	(switch-to-buffer (current-buffer))
	(let* ((str-height (length (split-string str "\n")))
	       (height (max 0 (- (/ (- (window-text-height) 1) 2)
				 (/ str-height 2)))))
	  (insert (make-string height ?\n) str)
	  (if twittering-oauth-invoke-browser
	      (browse-url authorize-url)
	    (when (y-or-n-p "Open authorization URL with browser? (using `browse-url')")
	      (browse-url authorize-url)))
	  (let* ((pin
		  (block pin-input-block
		    (while t
		      (let ((pin-input (read-string "Input PIN code: ")))
			(when (string-match "^\\s-*\\([0-9]+\\)\\s-*$" pin-input)
			  (return-from pin-input-block
			    (match-string 1 pin-input)))))))
		 (verifier pin))
	    (twittering-oauth-exchange-request-token
	     access-token-url
	     consumer-key consumer-secret
	     request-token request-token-secret verifier)))))))

;;;
;;; Private storage
;;;

(defun twittering-load-private-info ()
  (let* ((file twittering-private-info-file)
	 (decrypted-str (twittering-read-from-encrypted-file file)))
    (when decrypted-str
      (mapcar (lambda (pair)
		(let ((sym (car pair))
		      (value (cdr pair)))
		  (set sym value)
		  sym))
	      (condition-case nil
		  (read decrypted-str)
		(error
		 nil))))))

(defun twittering-load-private-info-with-guide ()
  (let ((str (concat
	      "Loading authorized access token for OAuth from\n"
	      (format "%s.\n" twittering-private-info-file)
	      "\n"
	      (propertize "Please input the master password.\n" 'face 'bold)
	      "\n"
	      "To cancel it, you may need to press C-g multiple times.\n"
	      )))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((str-height (length (split-string str "\n")))
	     (height (max 0 (- (/ (- (window-text-height) 1) 2)
			       (/ str-height 2)))))
	(insert (make-string height ?\n) str)
	(set-buffer-modified-p nil)
	(twittering-load-private-info)))))

(defun twittering-save-private-info ()
  (let* ((obj (mapcar (lambda (sym)
			`(,sym . ,(symbol-value sym)))
		      twittering-variables-stored-with-encryption))
	 (str (with-output-to-string (pp obj))))
    (twittering-write-and-encrypt twittering-private-info-file str)))



(defun twittering-save-private-info-with-guide ()
  (let ((str (concat
	      "Saving authorized access token for OAuth to "
	      (format "%s.\n" twittering-private-info-file)
	      "\n"
	      (propertize "Please input a master password twice."
			  'face 'bold))))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((str-height (length (split-string str "\n")))
	     (height (max 0 (- (/ (- (window-text-height) 1) 2)
			       (/ str-height 2)))))
	(insert (make-string height ?\n) str)
	(set-buffer-modified-p nil)
	(twittering-save-private-info)))))

(defun twittering-capable-of-encryption-p ()
  (and (or (require 'epa nil t) (require 'alpaca nil t))
       (executable-find "gpg")))

(defun twittering-read-from-encrypted-file (file)
  (cond
   ((require 'epa nil t)
    (let ((context (epg-make-context epa-protocol)))
      (epg-context-set-passphrase-callback
       context #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback
       context
       (cons #'epa-progress-callback-function
	     (format "Decrypting %s..." (file-name-nondirectory file))))
      (message "Decrypting %s..." (file-name-nondirectory file))
      (condition-case err
	  (epg-decrypt-file context file nil)
	(error
	 (message "%s" (cdr err))
	 nil))))
   ((require 'alpaca nil t)
    (with-temp-buffer
      (let ((buffer-file-name file)
	    (alpaca-regex-suffix ".*")
	    (temp-buffer (current-buffer)))
	(insert-file-contents-literally file)
	(set-buffer-modified-p nil)
	(condition-case nil
	    (progn
	      (alpaca-after-find-file)
	      (if (eq temp-buffer (current-buffer))
		  (buffer-string)
		;; `alpaca-after-find-file' kills the current buffer
		;; if the decryption is failed.
		nil))
	  (error
	   (when (eq temp-buffer (current-buffer))
	     (delete-region (point-min) (point-max)))
	   nil)))))
   (t
    nil)))

(defun twittering-write-and-encrypt (file str)
  (cond
   ((require 'epg nil t)
    (let ((context (epg-make-context epa-protocol)))
      (epg-context-set-passphrase-callback
       context #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback
       context (cons #'epa-progress-callback-function "Encrypting..."))
      (message "Encrypting...")
      (condition-case err
	  (with-temp-file file
	    (set-buffer-multibyte nil)
	    (delete-region (point-min) (point-max))
	    (insert (epg-encrypt-string context str nil))
	    (message "Encrypting...wrote %s" file))
	(error
	 (message "%s" (cdr err))
	 nil))))
   ((require 'alpaca nil t)
    ;; Create the file.
    ;; This is required because `alpaca-save-buffer' checks its timestamp.
    (with-temp-file file)
    (with-temp-buffer
      (let ((buffer-file-name file))
	(insert str)
	(condition-case nil
	    (if (alpaca-save-buffer)
		t
	      (delete-file file)
	      nil)
	  (error
	   (when (file-exists-p file)
	     (delete-file file))
	   nil)))))
   (t
    nil)))

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
      (twittering-render-timeline (current-buffer) nil nil t))))

(defvar twittering-icon-prop-hash (make-hash-table :test 'equal)
  "Hash table for storing display properties of icon. The key is the size of
icon and the value is a hash. The key of the child hash is URL and its value
is the display property for the icon.")

(defvar twittering-convert-program (executable-find "convert"))
(defvar twittering-convert-fix-size 48)
(defvar twittering-use-convert (not (null twittering-convert-program))
  "*This variable makes a sense only if `twittering-convert-fix-size'
is non-nil. If this variable is non-nil, icon images are converted by
invoking \"convert\". Otherwise, cropped images are displayed.")

(defconst twittering-error-icon-data-pair
  '(xpm . "/* XPM */
static char * yellow3_xpm[] = {
\"16 16 2 1\",
\" 	c None\",
\".	c #FF0000\",
\"................\",
\".              .\",
\". .          . .\",
\".  .        .  .\",
\".   .      .   .\",
\".    .    .    .\",
\".     .  .     .\",
\".      ..      .\",
\".      ..      .\",
\".     .  .     .\",
\".    .    .    .\",
\".   .      .   .\",
\".  .        .  .\",
\". .          . .\",
\".              .\",
\"................\"};
")
  "Image used when the valid icon cannot be retrieved.")

(defun twittering-get-display-spec-for-icon (image-url)
  (let ((hash
	 (gethash twittering-convert-fix-size twittering-icon-prop-hash)))
    (when hash
      (gethash image-url hash))))

(defun twittering-convert-image-data (image-data dest-type &optional src-type)
  "Convert IMAGE-DATA into XPM format and return it. If it fails to convert,
return nil."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (buffer-disable-undo)
    (let ((coding-system-for-read 'binary)
	  (coding-system-for-write 'binary)
	  (require-final-newline nil))
      (insert image-data)
      (let* ((args
	      `(,(if src-type (format "%s:-" src-type) "-")
		,@(when (integerp twittering-convert-fix-size)
		    `("-resize"
		      ,(format "%dx%d" twittering-convert-fix-size
			       twittering-convert-fix-size)))
		,(format "%s:-" dest-type)))
	     (exit-status
	      (apply 'call-process-region (point-min) (point-max)
		     twittering-convert-program t t nil args)))
	(if (equal 0 exit-status)
	    (buffer-string)
	  ;; failed to convert the image.
	  nil)))))

(defun twittering-create-image-pair (image-data)
  "Return a pair of image type and image data.
IMAGE-DATA is converted by `convert' if the image type of IMAGE-DATA is not
available and `twittering-use-convert' is non-nil."
  (let* ((image-type (image-type-from-data image-data))
	 (image-pair `(,image-type . ,image-data))
	 (converted-size
	  `(,twittering-convert-fix-size . ,twittering-convert-fix-size)))
    (cond
     ((and (image-type-available-p image-type)
	   (or (not (integerp twittering-convert-fix-size))
	       (equal (image-size (create-image image-data image-type t) t)
		      converted-size)))
      image-pair)
     (twittering-use-convert
      (let ((converted-data
	     (twittering-convert-image-data image-data 'xpm image-type)))
	(if converted-data
	    `(xpm . ,converted-data)
	  twittering-error-icon-data-pair)))
     (t
      twittering-error-icon-data-pair))))

(defun twittering-register-image-data (image-url image-data)
  (let ((image-pair (twittering-create-image-pair image-data)))
    (when image-pair
      (let ((hash (gethash twittering-convert-fix-size
			   twittering-icon-prop-hash)))
	(unless hash
	  (setq hash (make-hash-table :test 'equal))
	  (puthash twittering-convert-fix-size hash
		   twittering-icon-prop-hash))
	(let ((spec (twittering-make-display-spec-for-icon image-pair)))
	  (puthash image-url spec hash)
	  spec)))))

(defun twittering-make-slice-spec (image-spec)
  "Return slice property for reducing the image size by cropping it."
  (let* ((size (image-size image-spec t))
	 (width (car size))
	 (height (cdr size))
	 (fixed-length twittering-convert-fix-size)
	 (half-fixed-length (/ fixed-length 2)))
    (if (or (< fixed-length width) (< fixed-length height))
	`(slice ,(max 0 (- (/ width 2) half-fixed-length))
		,(max 0 (- (/ height 2) half-fixed-length))
		,fixed-length ,fixed-length)
      `(slice 0 0 ,fixed-length ,fixed-length))))

(defun twittering-make-display-spec-for-icon (image-pair)
  "Return the specification for `display' text property, which
limits the size of an icon image IMAGE-PAIR up to FIXED-LENGTH. If
the type of the image is not supported, nil is returned.

If the size of the image exceeds FIXED-LENGTH, the center of the
image are displayed."
  (let* ((type (car-safe image-pair))
	 (data (cdr-safe image-pair))
	 (raw-image-spec ;; without margins
	  (create-image data type t))
	 (slice-spec
	  (when (and twittering-convert-fix-size (not twittering-use-convert))
	    (twittering-make-slice-spec raw-image-spec)))
	 (image-spec
	  (if (fboundp 'create-animated-image) ;; Emacs24 or later
	      (create-animated-image data type t :margin 2 :ascent 'center)
	    (create-image data type t :margin 2 :ascent 'center))))
    (if slice-spec
	`(display (,image-spec ,slice-spec))
      `(display ,image-spec))))

(defun twittering-make-icon-string (beg end image-url)
  (let ((display-spec (twittering-get-display-spec-for-icon image-url))
	(image-data (gethash image-url twittering-url-data-hash))
	(properties (and beg (text-properties-at beg)))
	(icon-string (copy-sequence " ")))
    (when properties
      (add-text-properties 0 (length icon-string) properties icon-string))
    (cond
     (display-spec
      (let ((icon-string (apply 'propertize "_"
				(append properties display-spec))))
	;; Remove the property required no longer.
	(remove-text-properties 0 (length icon-string)
				'(need-to-be-updated nil)
				icon-string)
	icon-string))
     (image-data
      (twittering-register-image-data image-url image-data)
      (twittering-make-icon-string beg end image-url))
     (t
      (put-text-property 0 (length icon-string)
			 'need-to-be-updated
			 `(twittering-make-icon-string ,image-url)
			 icon-string)
      (twittering-url-retrieve-async image-url 'twittering-register-image-data)
      icon-string))))

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

(defun twittering-mode-line-buffer-identification ()
  (let ((active-mode-indicator
	 (if twittering-active-mode
	     twittering-modeline-active
	   twittering-modeline-inactive))
	(enabled-options
	 `(,@(when twittering-use-ssl `(,twittering-modeline-ssl))
	   ,@(when twittering-jojo-mode '("jojo"))
	   ,@(when twittering-icon-mode '("icon"))
	   ,@(when twittering-reverse-mode '("reverse"))
	   ,@(when twittering-scroll-mode '("scroll"))
	   ,@(when twittering-proxy-use '("proxy")))))
    (concat active-mode-indicator
	    (when twittering-display-remaining
	      (format " %d/%d"
		      (twittering-get-ratelimit-remaining)
		      (twittering-get-ratelimit-limit)))
	    (when enabled-options
	      (concat "[" (mapconcat 'identity enabled-options " ") "]")))))

(defun twittering-update-mode-line ()
  "Update mode line."
  (force-mode-line-update))

(defun twittering-status-id< (id1 id2)
  (let ((len1 (length id1))
	(len2 (length id2)))
    (cond
     ((= len1 len2) (string< id1 id2))
     ((< len1 len2) t)
     (t nil))))

(defun twittering-status-id= (id1 id2)
  (equal id1 id2))

(defun twittering-fill-string (str &optional adjustment prefix keep-newline)
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
      (let ((fill-column temporary-fill-column)
	    (fill-prefix (or prefix fill-prefix))
	    (adaptive-fill-regexp ""))
	(if keep-newline
	    (let* ((hard-newline (propertize "\n" 'hard t))
		   (str (mapconcat 'identity (split-string str "\n")
				   (concat hard-newline fill-prefix))))
	      (use-hard-newlines)
	      (insert (concat prefix str))
	      (fill-region (point-min) (point-max))
	      (remove-text-properties (point-min) (point-max) '(hard nil)))
	  (insert (concat prefix str))
	  (fill-region-as-paragraph (point-min) (point-max)))
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
  (beg end encoded-created-at time-format &optional additional-properties)
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
	   (t (format-time-string time-format encoded-created-at))))
	 (properties (append additional-properties
			     (and beg (text-properties-at beg)))))
    ;; Restore properties.
    (when properties
      (add-text-properties 0 (length time-string) properties time-string))
    (if (< secs 84600)
	(put-text-property 0 (length time-string)
			   'need-to-be-updated
			   `(twittering-make-passed-time-string
			     ,encoded-created-at ,time-format)
			   time-string)
      ;; Remove the property required no longer.
      (remove-text-properties 0 (length time-string) '(need-to-be-updated nil)
			      time-string))
    time-string))

(defun twittering-update-filled-string (beg end formater status prefix local-prefix &optional keep-newline)
  (let* ((str (twittering-fill-string (funcall formater status prefix)
				      (length prefix) local-prefix
				      keep-newline))
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
			     ,formater ,status ,prefix ,local-prefix
			     ,keep-newline)
			   str)
      ;; Remove the property required no longer.
      (remove-text-properties 0 (length str) '(need-to-be-updated nil) str))
    str))

(defun twittering-current-window-config (window-list)
  "Return window parameters of WINDOW-LIST."
  (mapcar (lambda (win)
	    (let ((start (window-start win))
		  (point (window-point win)))
	      `(,win ,start ,point)))
	  window-list))

(defun twittering-restore-window-config-after-modification (config beg end)
  "Restore window parameters changed by modification on given region.
CONFIG is window parameters made by `twittering-current-window-config'.
BEG and END mean a region that had been modified."
  (mapc (lambda (entry)
	  (let ((win (elt entry 0))
		(start (elt entry 1))
		(point (elt entry 2)))
	    (when (and (< beg start) (< start end))
	      (set-window-start win start))
	    (when (and (< beg point) (< point end))
	      (set-window-point win point))))
	config))

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

(defun twittering-delete-status-from-data-table (id)
  (let ((modified-spec nil))
    (maphash
     (lambda (spec data)
       (let* ((id-table (elt data 0))
	      (referring-id-table (elt data 1))
	      (timeline-data (elt data 2))
	      (status (gethash id id-table)))
	 (when status
	   (remhash id id-table)
	   ;; Here, `referring-id-table' is not modified.
	   ;; Therefore, the retweet observed secondly will not appear even
	   ;; if the retweet observed first for the same tweet is deleted.
	   (setq modified-spec
		 (cons `(,spec
			 ,id-table
			 ,referring-id-table
			 ,(remove status timeline-data))
		       modified-spec)))))
     twittering-timeline-data-table)
    (mapc
     (lambda (data)
       (let* ((spec (car data))
	      (buffer (twittering-get-buffer-from-spec spec)))
	 (puthash spec (cdr data) twittering-timeline-data-table)
	 (when (buffer-live-p buffer)
	   (with-current-buffer buffer
	     (save-excursion
	       (twittering-for-each-property-region
		'id
		(lambda (beg end value)
		  (when (twittering-status-id= id value)
		    (let ((buffer-read-only nil)
			  (separator-pos (min (point-max) (1+ end))))
		      (delete-region beg separator-pos)
		      (goto-char beg))))
		buffer))))))
     modified-spec)))

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
	(when (twittering-jojo-mode-p spec)
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
  "List of buffers managed by `twittering-mode'.")

(defun twittering-get-buffer-list ()
  "Return buffers managed by `twittering-mode'."
  (twittering-unregister-killed-buffer)
  twittering-buffer-info-list)

(defun twittering-get-active-buffer-list ()
  "Return active buffers managed by `twittering-mode', where statuses are
retrieved periodically."
  (twittering-unregister-killed-buffer)
  (remove nil
	  (mapcar (lambda (buffer)
		    (if (twittering-buffer-active-p buffer)
			buffer
		      nil))
		  twittering-buffer-info-list)))

(defun twittering-buffer-p (&optional buffer)
  "Return t if BUFFER is managed by `twittering-mode'.
BUFFER defaults to the the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
	 (memq buffer twittering-buffer-info-list))))

(defun twittering-buffer-active-p (&optional buffer)
  "Return t if BUFFER is an active buffer managed by `twittering-mode'.
BUFFER defaults to the the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (twittering-buffer-p buffer)
	 (with-current-buffer buffer
	   twittering-active-mode))))

(defun twittering-get-buffer-from-spec (spec)
  "Return the buffer bound to SPEC. If no buffers are bound to SPEC,
return nil."
  (let* ((spec-string (twittering-timeline-spec-to-string spec))
	 (buffers
	  (remove
	   nil
	   (mapcar
	    (lambda (buffer)
	      (if (twittering-equal-string-as-timeline
		   spec-string
		   (twittering-get-timeline-spec-string-for-buffer buffer))
		  buffer
		nil))
	    (twittering-get-buffer-list)))))
    (if buffers
	;; We assume that the buffer with the same spec is unique.
	(car buffers)
      nil)))

(defun twittering-get-buffer-from-spec-string (spec-string)
  "Return the buffer bound to SPEC-STRING. If no buffers are bound to it,
return nil."
  (let ((spec (twittering-string-to-timeline-spec spec-string)))
    (and spec (twittering-get-buffer-from-spec spec))))

(defun twittering-get-timeline-spec-for-buffer (buffer)
  "Return the timeline spec bound to BUFFER. If BUFFER is not managed by
`twittering-mode', return nil."
  (when (twittering-buffer-p buffer)
    (with-current-buffer buffer
      twittering-timeline-spec)))

(defun twittering-get-timeline-spec-string-for-buffer (buffer)
  "Return the timeline spec string bound to BUFFER. If BUFFER is not managed
by `twittering-mode', return nil."
  (when (twittering-buffer-p buffer)
    (with-current-buffer buffer
      twittering-timeline-spec-string)))

(defun twittering-current-timeline-spec ()
  "Return the timeline spec bound to the current buffer. If it is not managed
by `twittering-mode', return nil."
  (twittering-get-timeline-spec-for-buffer (current-buffer)))

(defun twittering-current-timeline-spec-string ()
  "Return the timeline spec string bound to the current buffer. If it is not
managed by `twittering-mode', return nil."
  (twittering-get-timeline-spec-string-for-buffer (current-buffer)))

(defun twittering-unregister-buffer (buffer &optional keep-timer)
  "Unregister BUFFER from `twittering-buffer-info-list'.
If BUFFER is the last managed buffer and KEEP-TIMER is nil, call
`twittering-stop' to stop timers."
  (when (memq buffer twittering-buffer-info-list)
    (setq twittering-buffer-info-list
	  (delq buffer twittering-buffer-info-list))
    (when (and (null twittering-buffer-info-list)
	       (not keep-timer))
      (twittering-stop))))

(defun twittering-unregister-killed-buffer ()
  "Unregister buffers which has been killed."
  (mapc (lambda (buffer)
	  (unless (buffer-live-p buffer)
	    (twittering-unregister-buffer buffer)))
	twittering-buffer-info-list))

(defun twittering-replace-spec-string-for-buffer (buffer spec-string)
  "Replace the timeline spec string for BUFFER with SPEC-STRING when
BUFFER is managed by `twittering-mode' and SPEC-STRING is equivalent
to the current one."
  (when (twittering-buffer-p buffer)
    (let ((current (twittering-get-timeline-spec-string-for-buffer buffer)))
      (when (and (not (string= current spec-string))
		 (twittering-equal-string-as-timeline current spec-string))
	(with-current-buffer buffer
	  (rename-buffer spec-string t)
	  (setq twittering-timeline-spec-string spec-string))))))

(defun twittering-set-active-flag-for-buffer (buffer active)
  "Set ACTIVE to active-flag for BUFFER."
  (when (twittering-buffer-p buffer)
    (let ((current (twittering-buffer-active-p buffer)))
      (when (or (and active (not current))
		(and (not active) current))
	(twittering-toggle-activate-buffer buffer)))))

(defun twittering-toggle-activate-buffer (&optional buffer)
  "Toggle whether to retrieve timeline for the current buffer periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (twittering-buffer-p buffer)
      (with-current-buffer buffer
	(let* ((new-mode (not twittering-active-mode))
	       (active-buffer-list (twittering-get-active-buffer-list))
	       (start-timer (and new-mode (null active-buffer-list))))
	  (setq twittering-active-mode new-mode)
	  (when start-timer
	    (twittering-start))
	  (twittering-update-mode-line))))))

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
	 ;; `spec-string' without text properties is required because
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
	(let ((buffer (generate-new-buffer spec-string))
	      (start-timer (null twittering-buffer-info-list)))
	  (add-to-list 'twittering-buffer-info-list buffer t)
	  (with-current-buffer buffer
	    (twittering-mode-setup spec-string)
	    (twittering-render-timeline buffer)
	    (when (twittering-account-authorized-p)
	      (when start-timer
		;; If `buffer' is the first managed buffer,
		;; call `twittering-start' to start timers.
		(twittering-start))
	      (unless (and start-timer twittering-active-mode)
		;; If `buffer' is active and the first managed buffer,
		;; `twittering-start' invokes
		;; `twittering-get-and-render-timeline' indirectly.
		;; Otherwise, `twittering-get-and-render-timeline' should be
		;; invoked here.
		(twittering-get-and-render-timeline))))
	  buffer)))))

(defun twittering-switch-to-next-timeline ()
  (interactive)
  (when (twittering-buffer-p)
    (let* ((buffer-list (twittering-get-buffer-list))
	   (following-buffers (cdr (memq (current-buffer) buffer-list)))
	   (next (if following-buffers
		     (car following-buffers)
		   (car buffer-list))))
      (unless (eq (current-buffer) next)
	(switch-to-buffer next)))))

(defun twittering-switch-to-previous-timeline ()
  (interactive)
  (when (twittering-buffer-p)
    (let* ((buffer-list (reverse (twittering-get-buffer-list)))
	   (preceding-buffers (cdr (memq (current-buffer) buffer-list)))
	   (previous (if preceding-buffers
			 (car preceding-buffers)
		       (car buffer-list))))
      (unless (eq (current-buffer) previous)
	(switch-to-buffer previous)))))

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
    (when (and buffer (not (eq buffer (current-buffer))))
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
;;; mode-line icon
;;;

;;; SSL
(defconst twittering-ssl-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
/*
 * Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>
 * Copyright (C) 2003 Kazu Yamamoto <kazu@Mew.org>
 * Copyright (C) 2004 Yoshifumi Nishida <nishida@csl.sony.co.jp>
 * Copyright notice is the same as Mew's one.
 */
static char * yellow3_xpm[] = {
\"14 14 7 1\",
\" 	c None\",
\".	c #B07403\",
\"+	c #EFEE38\",
\"@	c #603300\",
\"#	c #D0A607\",
\"$	c #FAFC90\",
\"%	c #241100\",
\"    .++++@    \",
\"   .+@...+@   \",
\"  .+@    .+@  \",
\"  .+@    .+@  \",
\"  .+@    .+@  \",
\"++########@@@@\",
\"+$$++++++++#@@\",
\"+$++++%@+++#@@\",
\"+$+++%%%@++#@@\",
\"+$+++%%%@++#@@\",
\"+$++++%@+++#@@\",
\"+$++++%@+++#@@\",
\"+$+++++++++#@@\",
\"++@@@@@@@@@@@@\"};
"
;;; The above image is copied from `mew-lock.xpm' distributed with Mew.
;;; The copyright of the image is below, which is copied from `mew.el'.

;;; Copyright Notice:

;; Copyright (C) 1994-2009 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
	    ))
  "Image for indicator of SSL state.")

(defconst twittering-modeline-ssl
  (if twittering-ssl-indicator-image
      (propertize "SSL"
		  'display twittering-ssl-indicator-image
		  'help-echo "SSL is enabled.")
    "SSL"))

;;; ACTIVE/INACTIVE
(defconst twittering-active-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char *plugged[] = {
\"32 12 8 1\",
\"  c None\",
\". c #a6caf0\",
\"# c #8fa5cf\",
\"a c #717171\",
\"b c #5d5d97\",
\"c c #8488ca\",
\"d c #9f9f9f\",
\"e c #7f8080\",
\"            ...                 \",
\"           .ccb....             \",
\"           accb####.            \",
\"          .accb#####..          \",
\"   eeeeeeeeaccb#####.eeeeeeee   \",
\"   dddddddcaccb#####.dedddddd   \",
\"   dddddddcaccb#####.dedddddd   \",
\"   eeeeeeeeaccb#####.eeeeeeee   \",
\"          aaccb####aaa          \",
\"           accbaaaaa            \",
\"           aaaaaaaa             \",
\"            aaa                 \"
};
"))
  "Image for indicator of active state."
;;; The above image is copied from `plugged.xpm' distributed with Wanderlust
;;; by Yuuichi Teranishi <teranisi@gohome.org>.
;;; The copyright of the image is below, which is copied from `COPYING' of
;;; Wanderlust 2.14.
;; Copyright (C) 1998-2001 Yuuichi Teranishi <teranisi@gohome.org>
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2, or (at your option)
;;    any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with GNU Emacs; see the file COPYING.  If not, write to the
;;    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;    Boston, MA 02111-1307, USA.
)

(defconst twittering-inactive-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
	    :ascent center
	    :data
	    "/* XPM */
static char * unplugged_xpm[] = {
\"32 12 9 1\",
\" 	s None	c None\",
\".	c tomato\",
\"X	c #a6caf0\",
\"o	c #8488ca\",
\"O	c #5d5d97\",
\"+	c #8fa5cf\",
\"@	c #717171\",
\"#	c #7f8080\",
\"$	c #9f9f9f\",
\"          XXX......             \",
\"           ...    ...           \",
\"          ..O     ....X         \",
\"         ..oO    ...+..XX       \",
\"   ######.ooO   ...+++.X#####   \",
\"   $$$$$o.ooO  ...@+++.X$#$$$   \",
\"   $$$$$o.ooO ... @+++.X$#$$$   \",
\"   ######.ooO...  @+++.X#####   \",
\"         ..o...   @++..@@       \",
\"          ....    @@..@         \",
\"           ...    ...@          \",
\"             ......             \"
};
"))
  "Image for indicator of inactive state."
;;; The above image is copied from `unplugged.xpm' distributed with Wanderlust
;;; by Yuuichi Teranishi <teranisi@gohome.org>.
;;; The copyright of the image is below, which is copied from `COPYING' of
;;; Wanderlust 2.14.
;; Copyright (C) 1998-2001 Yuuichi Teranishi <teranisi@gohome.org>
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2, or (at your option)
;;    any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with GNU Emacs; see the file COPYING.  If not, write to the
;;    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;    Boston, MA 02111-1307, USA.
)

(let ((props
       (when (display-mouse-p)
	 `(local-map
	   ,(purecopy (make-mode-line-mouse-map
		       'mouse-2 #'twittering-toggle-activate-buffer))
	   help-echo "mouse-2 toggles activate buffer"))))
  (defconst twittering-modeline-active
    (if twittering-active-indicator-image
	(apply 'propertize " "
	       `(display ,twittering-active-indicator-image ,@props))
      " "))
  (defconst twittering-modeline-inactive
    (if twittering-inactive-indicator-image
	(apply 'propertize "INACTIVE"
	       `(display ,twittering-inactive-indicator-image ,@props))
      "INACTIVE")))

;;;
;;; Account authorization
;;;

(defvar twittering-account-authorization nil
  "State of account authorization for `twittering-username' and
`twittering-password'.  The value is one of the following symbols:
nil -- The account have not been authorized yet.
queried -- The authorization has been queried, but not finished yet.
authorized -- The account has been authorized.")

(defun twittering-account-authorized-p ()
  (eq twittering-account-authorization 'authorized))
(defun twittering-account-authorization-queried-p ()
  (eq twittering-account-authorization 'queried))

(defun twittering-prepare-account-info ()
  (when (eq twittering-auth-method 'basic)
    (unless (twittering-get-username)
      (setq twittering-username (read-string "your twitter username: ")))
    (unless (twittering-get-password)
      (setq twittering-password
	    (read-passwd (format "%s's twitter password: "
				 twittering-username))))))

(defun twittering-verify-credentials ()
  (when (and (not (twittering-account-authorized-p))
	     (not (twittering-account-authorization-queried-p))
	     (eq twittering-auth-method 'oauth))
    (let* ((entry (twittering-lookup-connection-type twittering-oauth-use-ssl))
	   (oauth-get-token-type (cdr (assq 'oauth-get-token entry))))
      (setq twittering-oauth-get-token-function-type oauth-get-token-type)))
  (cond
   ((or (twittering-account-authorized-p)
	(twittering-account-authorization-queried-p))
    nil)
   ((and (eq twittering-auth-method 'oauth)
	 (or (null twittering-oauth-consumer-key)
	     (null twittering-oauth-consumer-secret)))
    (message "Consumer for OAuth is not specified.")
    nil)
   ((and twittering-use-master-password
	 (not (twittering-capable-of-encryption-p)))
    (message "You need GnuPG and (EasyPG or alpaca.el) for master password!")
    nil)
   ((and (eq twittering-auth-method 'oauth)
	 twittering-use-master-password
	 (twittering-capable-of-encryption-p)
	 (file-exists-p twittering-private-info-file))
    (cond
     ((twittering-load-private-info-with-guide)
      (message "The authorized token is loaded.")
      (setq twittering-account-authorization 'queried)
      (setq twittering-username
	    (cdr (assoc "screen_name" twittering-oauth-access-token-alist)))
      (let ((proc
	     (twittering-call-api
	      'verify-credentials
	      `((sentinel
		 . twittering-http-get-verify-credentials-sentinel)))))
	(unless proc
	  (setq twittering-account-authorization nil)
	  (message "Authorization failed. Type M-x twit to retry.")
	  (setq twittering-oauth-access-token-alist nil))))
     (t
      (message "Failed to load an authorized token from \"%s\"."
	       twittering-private-info-file)
      nil)))
   ((eq twittering-auth-method 'oauth)
    (let ((token-alist
	   (twittering-oauth-get-access-token
	    twittering-oauth-request-token-url
	    (lambda (token)
	      (concat twittering-oauth-authorization-url-base token))
	    twittering-oauth-access-token-url
	    twittering-oauth-consumer-key twittering-oauth-consumer-secret
	    "twittering-mode")))
      (cond
       ((and (assoc "oauth_token" token-alist)
	     (assoc "oauth_token_secret" token-alist)
	     (assoc "screen_name" token-alist))
	(let ((username (cdr (assoc "screen_name" token-alist))))
	  (setq twittering-oauth-access-token-alist token-alist)
	  (setq twittering-username username)
	  (setq twittering-account-authorization 'authorized)
	  (twittering-start)
	  (message "Authorization for the account \"%s\" succeeded."
		   username)
	  (when (and twittering-use-master-password
		     (twittering-capable-of-encryption-p)
		     (not (file-exists-p twittering-private-info-file)))
	    (twittering-save-private-info-with-guide))))
       (t
	(message "Authorization via OAuth failed. Type M-x twit to retry.")))))
   ((eq twittering-auth-method 'basic)
    (setq twittering-account-authorization 'queried)
    (let ((proc
	   (twittering-call-api
	    'verify-credentials
	    `((sentinel . twittering-http-get-verify-credentials-sentinel)))))
      (unless proc
	(setq twittering-account-authorization nil)
	(message "Authorization for the account \"%s\" failed. Type M-x twit to retry."
		 (twittering-get-username))
	(setq twittering-username nil)
	(setq twittering-password nil))))))

(defun twittering-http-get-verify-credentials-sentinel (header-info proc noninteractive &optional suc-msg)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info))))
    (case-string
     status-code
     (("200")
      (setq twittering-account-authorization 'authorized)
      (twittering-start)
      (format "Authorization for the account \"%s\" succeeded."
	      (twittering-get-username)))
     (t
      (setq twittering-account-authorization nil)
      (let ((error-mes
	     (format "Authorization for the account \"%s\" failed. Type M-x twit to retry."
		     (twittering-get-username))))
	(cond
	 ((eq twittering-auth-method 'oauth)
	  (setq twittering-oauth-access-token-alist nil))
	 ((eq twittering-auth-method 'basic)
	  (setq twittering-username nil)
	  (setq twittering-password nil)))
	error-mes)))))

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
      (define-key km (kbd "C-c C-u") 'twittering-user-timeline)
      (define-key km (kbd "C-c C-d") 'twittering-direct-messages-timeline)
      (define-key km (kbd "C-c C-s") 'twittering-update-status-interactive)
      (define-key km (kbd "C-c C-e") 'twittering-erase-old-statuses)
      (define-key km (kbd "C-c C-m") 'twittering-retweet)
      (define-key km (kbd "C-c C-t") 'twittering-set-current-hashtag)
      (define-key km (kbd "C-m") 'twittering-enter)
      (define-key km (kbd "C-c C-l") 'twittering-update-lambda)
      (define-key km (kbd "<mouse-1>") 'twittering-click)
      (define-key km (kbd "C-c C-v") 'twittering-view-user-page)
      (define-key km (kbd "C-c D") 'twittering-delete-status)
      (define-key km (kbd "a") 'twittering-toggle-activate-buffer)
      (define-key km (kbd "g") 'twittering-current-timeline)
      (define-key km (kbd "u") 'twittering-update-status-interactive)
      (define-key km (kbd "d") 'twittering-direct-message)
      (define-key km (kbd "v") 'twittering-other-user-timeline)
      (define-key km (kbd "V") 'twittering-visit-timeline)
      (define-key km (kbd "L") 'twittering-other-user-list-interactive)
      (define-key km (kbd "f") 'twittering-switch-to-next-timeline)
      (define-key km (kbd "b") 'twittering-switch-to-previous-timeline)
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

(defun twittering-initialize-global-variables-if-necessary ()
  "Initialize global variables for `twittering-mode' if they have not
been initialized yet."
  (unless twittering-initialized
    (defface twittering-username-face
      `((t ,(append '(:underline t)
		    (face-attr-construct
		     (if (facep 'font-lock-string-face)
			 'font-lock-string-face
		       'bold)))))
      "" :group 'faces)
    (defface twittering-uri-face `((t (:underline t))) "" :group 'faces)
    (twittering-update-status-format)
    (when twittering-use-convert
      (if (null twittering-convert-program)
	  (setq twittering-use-convert nil)
	(with-temp-buffer
	  (call-process twittering-convert-program nil (current-buffer) nil
			"-version")
	  (goto-char (point-min))
	  (if (null (search-forward-regexp "\\(Image\\|Graphics\\)Magick"
					   nil t))
	      (setq twittering-use-convert nil)))))
    (twittering-setup-proxy)
    (setq twittering-initialized t)))

(defvar twittering-mode-hook nil
  "Twittering-mode hook.")

(defvar twittering-initialized nil)

(defun twittering-mode-setup (spec-string)
  "Set up the current buffer for `twittering-mode'."
  (kill-all-local-variables)
  (setq major-mode 'twittering-mode)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq mode-name "twittering-mode")
  (setq mode-line-buffer-identification
	`(,(default-value 'mode-line-buffer-identification)
	  (:eval (twittering-mode-line-buffer-identification))))

  ;; Prevent `global-font-lock-mode' enabling `font-lock-mode'.
  ;; This technique is derived from `lisp/bs.el' distributed with Emacs 22.2.
  (make-local-variable 'font-lock-global-modes)
  (setq font-lock-global-modes '(not twittering-mode))

  (make-local-variable 'twittering-timeline-spec)
  (make-local-variable 'twittering-timeline-spec-string)
  (make-local-variable 'twittering-active-mode)
  (make-local-variable 'twittering-icon-mode)
  (make-local-variable 'twittering-jojo-mode)
  (make-local-variable 'twittering-reverse-mode)
  (make-local-variable 'twittering-scroll-mode)

  (setq twittering-timeline-spec-string spec-string)
  (setq twittering-timeline-spec
	(twittering-string-to-timeline-spec spec-string))
  (setq twittering-active-mode t)

  (use-local-map twittering-mode-map)
  (twittering-update-mode-line)
  (set-syntax-table twittering-mode-syntax-table)
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (font-lock-mode -1))
  (add-to-list 'twittering-buffer-info-list (current-buffer) t)
  (run-hooks 'twittering-mode-hook))

(defun twittering-mode ()
  "Major mode for Twitter
\\{twittering-mode-map}"
  (interactive)
  (if (listp twittering-initial-timeline-spec-string)
      (mapc 'twittering-visit-timeline
	    twittering-initial-timeline-spec-string)
    (twittering-visit-timeline twittering-initial-timeline-spec-string)))

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
	 (help-str (format (substitute-command-keys "Keymap:
  \\[twittering-edit-post-status]: send %s
  \\[twittering-edit-cancel-status]: cancel %s
  \\[twittering-edit-next-history]: next history element
  \\[twittering-edit-previous-history]: previous history element
  \\[twittering-edit-replace-at-point]: shorten URL at point

---- text above this line is ignored ----
") item item))
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
     ((or (not twittering-request-confirmation-on-posting)
	  (y-or-n-p "Send this tweet? "))
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
		      (concat "\\`@" username "\\(?:[\n\r \t]+\\)*")
		      status))
	    (add-to-list 'parameters
			 `("in_reply_to_status_id" .
			   ,(format "%s" reply-to-id))))
	  (twittering-http-post twittering-api-host "1/statuses/update"
				parameters))))
      (twittering-edit-close))
     (t
      nil))))

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

(defvar twittering-url-show-status t
  "*Whether to show a running total of bytes transferred.")
(defun twittering-url-wrapper (func &rest args)
  (let ((url-proxy-services
	 (when twittering-proxy-use
	   (twittering-url-proxy-services)))
	(url-show-status twittering-url-show-status))
    (if (eq func 'url-retrieve)
	(let ((buffer (apply func args)))
	  (when (buffer-live-p buffer)
	    (with-current-buffer buffer
	      (set (make-local-variable 'url-show-status)
		   twittering-url-show-status)))
	  buffer)
      (apply func args))))

(defun twittering-url-insert-file-contents (url)
  (twittering-url-wrapper 'url-insert-file-contents url))

(defun twittering-url-retrieve-synchronously (url)
  (twittering-url-wrapper 'url-retrieve-synchronously url))

;;;
;;; Asynchronous retrieval
;;;

(defvar twittering-url-data-hash (make-hash-table :test 'equal))
(defvar twittering-url-request-list nil)
(defvar twittering-url-request-sentinel-hash (make-hash-table :test 'equal))
(defvar twittering-internal-url-queue nil)
(defvar twittering-url-request-resolving-p nil)
(defvar twittering-url-request-retry-limit 3)
(defconst twittering-url-request-dummy-buffer-name
  " *twittering-dummy-for-url-retrieve-async*")

(defun twittering-remove-redundant-queries (queue)
  (remove nil
	  (mapcar
	   (lambda (url)
	     (let ((current (gethash url twittering-url-data-hash)))
	       (when (or (null current)
			 (and (integerp current)
			      (< current twittering-url-request-retry-limit)))
		 url)))
	   (twittering-remove-duplicates queue))))

(defun twittering-resolve-url-request ()
  "Resolve requests of asynchronous URL retrieval."
  (when (null twittering-url-request-resolving-p)
    (setq twittering-url-request-resolving-p t)
    ;; It is assumed that the following part is not processed
    ;; in parallel.
    (setq twittering-internal-url-queue
	  (append twittering-internal-url-queue twittering-url-request-list))
    (setq twittering-url-request-list nil)
    (setq twittering-internal-url-queue
	  (twittering-remove-redundant-queries twittering-internal-url-queue))
    (if (null twittering-internal-url-queue)
	(setq twittering-url-request-resolving-p nil)
      (let ((url (car twittering-internal-url-queue))
	    (coding-system-for-read 'binary)
	    (require-final-newline nil))
	(twittering-url-wrapper
	 'url-retrieve
	 url
	 (lambda (&rest args)
	   (let* ((status (if (< 21 emacs-major-version)
			      (car args)
			    nil))
		  (callback-args (if (< 21 emacs-major-version)
				     (cdr args)
				   args))
		  (url (elt callback-args 0)))
	     (goto-char (point-min))
	     (if (and (or (null status) (not (member :error status)))
		      (search-forward-regexp "\r?\n\r?\n" nil t))
		 (let ((body (buffer-substring (match-end 0) (point-max))))
		   (puthash url body twittering-url-data-hash)
		   (setq twittering-internal-url-queue
			 (remove url twittering-internal-url-queue))
		   (let ((sentinels
			  (gethash url twittering-url-request-sentinel-hash)))
		     (when sentinels
		       (remhash url twittering-url-request-sentinel-hash)
		       (mapc (lambda (func)
			       (funcall func url body))
			     sentinels))))
	       (let ((current (gethash url twittering-url-data-hash)))
		 (cond
		  ((null current)
		   (puthash url 1 twittering-url-data-hash))
		  ((integerp current)
		   (puthash url (1+ current) twittering-url-data-hash))
		  (t
		   nil))))
	     (let ((current (current-buffer)))
	       (set-buffer (get-buffer-create
			    twittering-url-request-dummy-buffer-name))
	       (kill-buffer current))
	     (setq twittering-url-request-resolving-p nil)
	     (twittering-resolve-url-request)))
	 `(,url))))))

(defun twittering-url-retrieve-async (url &optional sentinel)
  "Retrieve URL asynchronously and call SENTINEL with the retrieved data.
The request is placed at the last of queries queue. When the data has been
retrieved, SENTINEL will be called as (funcall SENTINEL URL url-data).
The retrieved data can be referred as (gethash url twittering-url-data-hash)."
  (add-to-list 'twittering-url-request-list url t)
  (when sentinel
    (let ((current (gethash url twittering-url-request-sentinel-hash)))
      (unless (member url current)
	(puthash url (cons sentinel current)
		 twittering-url-request-sentinel-hash))))
  (twittering-resolve-url-request))

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
  (unless twittering-curl-program
    (setq twittering-curl-program (twittering-find-curl-program)))
  (not (null twittering-curl-program)))

(defun twittering-start-http-session-curl-https-p ()
  "Return t if curl was installed and the curl support HTTPS, otherwise nil."
  (when (twittering-start-http-session-curl-p)
    (unless twittering-curl-program-https-capability
      (with-temp-buffer
	(call-process twittering-curl-program
		      nil (current-buffer) nil
		      "--version")
	(goto-char (point-min))
	(setq twittering-curl-program-https-capability
	      (if (search-forward-regexp "^Protocols: .*https" nil t)
		  'capable
		'incapable))))
    (eq twittering-curl-program-https-capability 'capable)))

(defun twittering-lookup-connection-type (use-ssl &optional order table)
  "Return available entry extracted fron connection type table.
TABLE is connection type table, which is an alist of type symbol and its
item alist, such as
 '((native (check . t)
           (https . twittering-start-http-session-native-tls-p)
           (start . twittering-start-http-session-native))
   (curl (check . twittering-start-http-session-curl-p)
         (https . twittering-start-http-session-curl-https-p)
         (start . twittering-start-http-session-curl))) .
ORDER means the priority order of type symbols.
If USE-SSL is nil, the item `https' is ignored.
When the type `curl' has priority and is available for the above table,
the function returns
 '((check . twittering-start-http-session-curl-p)
   (https . twittering-start-http-session-curl-https-p)
   (start . twittering-start-http-session-curl)) ."
  (let ((rest (or order twittering-connection-type-order))
	(table (or table twittering-connection-type-table))
	(result nil))
    (while (and rest (null result))
      (let* ((candidate (car rest))
	     (entry (cdr (assq candidate table)))
	     (validate (lambda (item)
			 (let ((v (cdr (assq item entry))))
			   (or (null v) (eq t v) (functionp v)))))
	     (confirm (lambda (item)
			(let ((v (cdr (assq item entry))))
			  (cond
			   ((null v) nil)
			   ((eq t v) t)
			   ((functionp v) (funcall v)))))))
	(if (and (funcall validate 'check)
		 (or (not use-ssl) (funcall validate 'https)))
	    (cond
	     ((and (funcall confirm 'check)
		   (or (not use-ssl) (funcall confirm 'https)))
	      (setq rest nil)
	      (setq result entry))
	     (t
	      (setq rest (cdr rest))))
	  (message "The configuration for conncetion type `%s' is invalid."
		   candidate)
	  (setq rest nil))))
    result))

(defun twittering-lookup-http-start-function (&optional order table)
  "Decide a connection method from currently available methods."
  (let* ((order (or order twittering-connection-type-order))
	 (table (or table twittering-connection-type-table))
	 (entry (twittering-lookup-connection-type
		 twittering-use-ssl order table)))
    (if entry
	(cdr (assq 'start entry))
      (cond
       ((and twittering-use-ssl
	     (yes-or-no-p "HTTPS(SSL) is unavailable. Use HTTP instead? "))
	;; Fall back on connection without SSL.
	(setq twittering-use-ssl nil)
	(twittering-update-mode-line)
	(twittering-lookup-http-start-function order table))
       (t
	(message "No connection methods are available.")
	nil)))))

(defun twittering-start-http-session (method headers host port path parameters &optional noninteractive sentinel)
  "METHOD    : http method
HEADERS   : http request headers in assoc list
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
	 (cacert-fullpath (when twittering-use-ssl
			    (twittering-ensure-ca-cert)))
	 (cacert-dir (when cacert-fullpath
		       (file-name-directory cacert-fullpath)))
	 (cacert-filename (when cacert-fullpath
			    (file-name-nondirectory cacert-fullpath)))
	 (default-directory
	   ;; If `twittering-use-ssl' is non-nil, the `curl' process
	   ;; is executed at the same directory as the temporary cert file.
	   ;; Without changing directory, `curl' misses the cert file if
	   ;; you use Emacs on Cygwin because the path on Emacs differs
	   ;; from Windows.
	   ;; With changing directory, `curl' on Windows can find the cert
	   ;; file if you use Emacs on Cygwin.
	   (if twittering-use-ssl
	       cacert-dir
	     default-directory))
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
		`("--cacert" ,cacert-filename))
	    ,@(when twittering-proxy-use
		(let* ((scheme (funcall request :schema))
		       (host (twittering-proxy-info scheme 'server))
		       (port (twittering-proxy-info scheme 'port)))
		  (when (and host port)
		    `("-x" ,(format "%s:%s" host port)))))
	    ,@(when twittering-proxy-use
		(let ((pair
		       (cdr (assoc
			     (funcall request :schema)
			     `(("http" .
				(,twittering-http-proxy-user
				 . ,twittering-http-proxy-password))
			       ("https" .
				(,twittering-https-proxy-user
				 . ,twittering-https-proxy-password)))))))
		  (when (and pair (car pair) (cdr pair))
		    `("-U" ,(format "%s:%s" (car pair) (cdr pair))))))
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
		       (concat "?" (funcall request :query-string))))))
	 (coding-system-for-read 'utf-8-unix))
    (debug-print curl-args)
    (lexical-let ((noninteractive noninteractive)
		  (sentinel sentinel))
      (let ((curl-process
	     (apply 'start-process
		    "*twmode-curl*"
		    temp-buffer
		    twittering-curl-program
		    curl-args)))
	(when curl-process
	  (set-process-sentinel
	   curl-process
	   (lambda (&rest args)
	     (apply #'twittering-http-default-sentinel
		    sentinel noninteractive args))))
	curl-process)))
  )

(defun twittering-start-http-session-native-tls-p ()
  (when (require 'tls nil t)
    (unless twittering-tls-program
      (let ((programs
	     (remove nil
		     (mapcar (lambda (cmd)
			       (when (string-match "\\`\\([^ ]+\\) " cmd)
				 (when (executable-find (match-string 1 cmd))
				   cmd)))
			     tls-program))))
	(setq twittering-tls-program programs)))
    (not (null twittering-tls-program))))

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
	     (proxy-info
	      (when twittering-proxy-use
		(twittering-proxy-info (request :schema))))
	     (server (if proxy-info
			 (cdr (assq 'server proxy-info))
		       (request :host)))
	     (port (if proxy-info
		       (cdr (assq 'port proxy-info))
		     (request :port)))
	     (coding-system-for-read 'utf-8-unix)
	     (proc
	      (cond
	       (twittering-use-ssl
		(let* ((tls-program twittering-tls-program)
		       (proc
			(open-tls-stream
			 "network-connection-process" nil server port)))
		  (when proc
		    (set-process-buffer proc temp-buffer))
		  proc))
	       (t
		(open-network-stream
		 "network-connection-process" temp-buffer server port))))
	     )
	(when proc
	  (lexical-let ((sentinel sentinel)
			(noninteractive noninteractive))
	    (set-process-sentinel
	     proc
	     (lambda (&rest args)
	       (apply #'twittering-http-default-sentinel
		      sentinel noninteractive args))))
	  (debug-print request-str)
	  (process-send-string proc request-str))
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
  "Return an assoc list of HTTP headers for twittering-mode."
  (unless method
    (setq method "GET"))

  (let ((headers headers))
    (push (cons "User-Agent" (twittering-user-agent)) headers)
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
      (let* ((scheme (if twittering-use-ssl "https" "http"))
	     (keep-alive (twittering-proxy-info scheme 'keep-alive))
	     (user (twittering-proxy-info scheme 'user))
	     (password (twittering-proxy-info scheme 'password)))
	(when (twittering-proxy-info scheme 'keep-alive)
	  (push (cons "Proxy-Connection" "Keep-Alive")
		headers))
	(when (and user password)
	  (push (cons
		 "Proxy-Authorization"
		 (concat "Basic "
			 (base64-encode-string (concat user ":" password))))
		headers))))
    headers
    ))

(defun twittering-get-error-message (buffer)
  (if buffer
      (let ((xmltree (twittering-get-response-body buffer
						   'xml-parse-region)))
	(car (cddr (assq 'error (or (assq 'errors xmltree)
				    (assq 'hash xmltree))))))
    nil))

(defun twittering-http-application-headers-with-auth (method url query-parameters)
  "Return an alist of HTTP headers with Authorization for `twittering-mode'.
METHOD is HTTP method (\"GET\", \"POST\", etc.).
QUERY-PARAMETERS is a list of cons pair of name and value such as
'((\"status\" . \"test tweet\")
  (\"in_reply_to_status_id\" . \"12345678\"))."
  (let ((auth-str
	 (cond
	  ((eq twittering-auth-method 'basic)
	   (concat "Basic "
		   (base64-encode-string
		    (concat (twittering-get-username)
			    ":" (twittering-get-password)))))
	  ((eq twittering-auth-method 'oauth)
	   (let ((access-token
		  (cdr (assoc "oauth_token"
			      twittering-oauth-access-token-alist)))
		 (access-token-secret
		  (cdr (assoc "oauth_token_secret"
			      twittering-oauth-access-token-alist)))
		 (encoded-query-parameters
		  (mapcar
		   (lambda (entry)
		     (let ((name (twittering-percent-encode (car entry)))
			   (value (twittering-percent-encode (cdr entry))))
		       `(,name . ,value)))
		   query-parameters)))
	     (twittering-oauth-auth-str-access
	      method url encoded-query-parameters
	      twittering-oauth-consumer-key twittering-oauth-consumer-secret
	      access-token access-token-secret)))
	  (t
	   nil)))
	(base-headers (twittering-http-application-headers method)))
    (if auth-str
	`(,@base-headers
	  ("Authorization" . ,auth-str))
      base-headers)))

(defun twittering-http-get (host method &optional noninteractive parameters format sentinel)
  (let* ((format (or format "xml"))
	 (sentinel (or sentinel 'twittering-http-get-default-sentinel))
	 (scheme (if twittering-use-ssl
		     "https"
		   "http"))
	 (path (concat "/" method "." format))
	 (url (format "%s://%s%s" scheme host path))
	 (headers
	  (twittering-http-application-headers-with-auth
	   "GET" url parameters)))
    (twittering-start-http-session
     "GET" headers host nil path parameters noninteractive sentinel)))

(defun twittering-created-at-to-seconds (created-at)
  (let ((encoded-time (apply 'encode-time (parse-time-string created-at))))
    (+ (* (car encoded-time) 65536)
       (cadr encoded-time))))

(defun twittering-http-default-sentinel (func noninteractive proc stat &optional suc-msg)
  (debug-printf "http-default-sentinel: proc=%s stat=%s" proc stat)
  (let ((temp-buffer (process-buffer proc))
	(status (process-status proc))
	(mes nil))
    (when (and twittering-proxy-use twittering-use-ssl
	       (buffer-live-p temp-buffer))
      ;; When using SSL via a proxy with CONNECT method,
      ;; omit a successful HTTP response and headers if they seem to be
      ;; sent from the proxy.
      (with-current-buffer temp-buffer
	(save-excursion
	  (goto-char (point-min))
	  (let ((first-regexp
		 ;; successful HTTP response
		 "\\`HTTP/1\.[01] 2[0-9][0-9] .*?\r?\n")
		(next-regexp
		 ;; following HTTP response
		 "^\\(\r?\n\\)HTTP/1\.[01] [0-9][0-9][0-9] .*?\r?\n"))
	    (when (and (search-forward-regexp first-regexp nil t)
		       (search-forward-regexp next-regexp nil t))
	      (let ((beg (point-min))
		    (end (match-end 1)))
		(delete-region beg end)))))))
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
      (message "%s" mes))))

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
	    (when (and new-statuses buffer)
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
  (let* ((format (or format "xml"))
	 (sentinel (or sentinel 'twittering-http-post-default-sentinel))
	 (parameters (cons '("source" . "twmode") parameters))
	 (scheme (if twittering-use-ssl
		     "https"
		   "http"))
	 (path (concat "/" method "." format))
	 (url (format "%s://%s%s" scheme host path))
	 (headers
	  (twittering-http-application-headers-with-auth
	   "POST" url parameters)))
    (twittering-start-http-session
     "POST" headers host nil path parameters noninteractive sentinel)))

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
  "Extract HTTP response header from HTTP response.
BUFFER may be a buffer or the name of an existing buffer which contains the HTTP response."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp "\r?\n\r?\n" nil t)
	  (buffer-substring (point-min) (match-end 0))
	nil))))

(defun twittering-get-response-body (buffer &optional func)
  "Extract HTTP response body from HTTP response.
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
  "Extract statuses from HTTP response, and return a list.
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
	   (recipient-screen-name
	    (assq-get 'recipient_screen_name status-data))
	   recipient_screen_name
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
                    original-user-screen-name
		    recipient-screen-name))))))

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
			  (recipient_screen_name
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
  "Encode STR according to Percent-Encoding defined in RFC 3986."
  (twittering-oauth-url-encode str coding-system))

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
  (mapc (lambda (buffer)
	  (unless (with-current-buffer buffer mark-active)
	    (twittering-redisplay-status-on-each-buffer buffer)))
	(twittering-get-buffer-list)))

(defun twittering-redisplay-status-on-each-buffer (buffer)
  (let ((deactivate-mark deactivate-mark)
	(window-list (get-buffer-window-list buffer nil t))
	(marker (with-current-buffer buffer (point-marker)))
	(result nil))
    (with-current-buffer buffer
      (save-excursion
	(twittering-for-each-property-region
	 'need-to-be-updated
	 (lambda (beg end value)
	   (let* ((func (car value))
		  (args (cdr value))
		  (current-str (buffer-substring beg end))
		  (updated-str (apply func beg end args))
		  (config (twittering-current-window-config window-list))
		  (buffer-read-only nil))
	     ;; Replace `current-str' if it differs to `updated-str' with
	     ;; ignoring properties. This is an ad-hoc solution.
	     ;; `current-str' is a part of the displayed status, but it has
	     ;; properties which are determined by the whole status.
	     ;; (For example, the `id' property.)
	     ;; Therefore, we cannot compare the strings with their
	     ;; properties.
	     (unless (string= current-str updated-str)
	       ;; If the region to be modified includes the current position,
	       ;; the point moves to the beginning of the region.
	       (when (and (< beg marker) (< marker end))
		 ;; This is required because the point moves to the center if
		 ;; the point becomes outside of the window by the effect of
		 ;; `set-window-start'.
		 (setq result beg))
	       (delete-region beg end)
	       (goto-char beg)
	       (insert updated-str)
	       (twittering-restore-window-config-after-modification
		config beg end))))
	 buffer))
      (set-marker marker nil)
      (when (and result (eq (window-buffer) buffer))
	(let ((win (selected-window)))
	  (when (< result (window-start win))
	    (set-window-start win result))
	  (set-window-point win result))))))

;;;
;;; display functions
;;;

(defun twittering-render-timeline (buffer &optional additional timeline-data keep-point)
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
      (debug-print (current-buffer))
      (cond
       (keep-point
	;; Restore points.
	(mapc (lambda (pair)
		(let* ((point (car pair))
		       (window (cdr pair))
		       (dest (max (point-max) point)))
		  (set-window-point window dest)))
	      point-window-list)
	(goto-char original-pos))
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
		(separator "\n")
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
		       (insert-before-markers formatted-status separator)
		     (insert formatted-status separator))))
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

(defun twittering-generate-format-table (status-sym prefix-sym)
  `(("%" . "%")
    ("}" . "}")
    ("#" . (cdr (assq 'id ,status-sym)))
    ("'" . (when (string= "true" (cdr (assq 'truncated ,status-sym)))
	     "..."))
    ("c" . (cdr (assq 'created-at ,status-sym)))
    ("d" . (cdr (assq 'user-description ,status-sym)))
    ("f" . (cdr (assq 'source ,status-sym)))
    ("i" .
     (when (and twittering-icon-mode window-system)
       (let* ((url (cdr (assq 'user-profile-image-url ,status-sym))))
	 (twittering-make-icon-string nil nil url))))
    ("j" . (cdr (assq 'user-id ,status-sym)))
    ("L" .
     (let ((location (or (cdr (assq 'user-location ,status-sym)) "")))
       (unless (string= "" location)
	 (concat "[" location "]"))))
    ("l" . (cdr (assq 'user-location ,status-sym)))
    ("p" . (when (string= "true" (cdr (assq 'user-protected ,status-sym)))
	     "[x]"))
    ("r" .
     (let ((reply-id (or (cdr (assq 'in-reply-to-status-id ,status-sym)) ""))
	   (reply-name (or (cdr (assq 'in-reply-to-screen-name ,status-sym))
			   ""))
	   (recipient-screen-name
	    (cdr (assq 'recipient-screen-name ,status-sym))))
       (let* ((pair
	       (cond
		(recipient-screen-name
		 (cons (format "sent to %s" recipient-screen-name)
		       (twittering-get-status-url recipient-screen-name)))
		((and (not (string= "" reply-id))
		      (not (string= "" reply-name)))
		 (cons (format "in reply to %s" reply-name)
		       (twittering-get-status-url reply-name reply-id)))
		(t nil)))
	      (str (car pair))
	      (url (cdr pair))
	      (properties
	       (list 'mouse-face 'highlight 'face 'twittering-uri-face
		     'uri url)))
	 (when (and str url)
	   (concat " " (apply 'propertize str properties))))))
    ("R" .
     (let ((retweeted-by
	    (or (cdr (assq 'original-user-screen-name ,status-sym)) "")))
       (unless (string= "" retweeted-by)
	 (concat " (retweeted by " retweeted-by ")"))))
    ("S" . (cdr (assq 'user-name ,status-sym)))
    ("s" . (cdr (assq 'user-screen-name ,status-sym)))
    ("T" . (cdr (assq 'text ,status-sym)))
    ("t" . (cdr (assq 'text ,status-sym)))
    ("u" . (cdr (assq 'user-url ,status-sym)))))

(defun twittering-generate-formater-for-first-spec (format-str status-sym prefix-sym)
  (cond
   ((string-match "\\`}" format-str)
    ;; "}" at the first means the end of the current level.
    `(nil . ,(substring format-str (match-end 0))))
   ((string-match "\\`%" format-str)
    (let* ((following (substring format-str 1))
	   (table (twittering-generate-format-table status-sym prefix-sym))
	   (regexp (concat "\\`\\(" (mapconcat 'car table "\\|") "\\)"))
	   (case-fold-search nil))
      (cond
       ((string-match "\\`@\\({\\([^}]*\\)}\\)?" following)
	(let ((time-format (or (match-string 2 following)
			       "%I:%M %p %B %d, %Y"))
	      (rest (substring following (match-end 0))))
	  `((let* ((created-at-str (cdr (assq 'created-at ,status-sym)))
		   (created-at
		    (apply 'encode-time
			   (parse-time-string created-at-str)))
		   (url
		    (twittering-get-status-url
		     (cdr (assq 'user-screen-name ,status-sym))
		     (cdr (assq 'id ,status-sym))))
		   (properties
		    (list 'mouse-face 'highlight 'face 'twittering-uri-face
			  'uri url)))
	      (twittering-make-passed-time-string
	       nil nil created-at ,time-format properties))
	    . ,rest)))
       ((string-match "\\`C\\({\\([^}]*\\)}\\)?" following)
	(let ((time-format (or (match-string 2 following) "%H:%M:%S"))
	      (rest (substring following (match-end 0))))
	  `((let* ((created-at-str (cdr (assq 'created-at ,status-sym)))
		   (created-at (apply 'encode-time
				      (parse-time-string created-at-str))))
	      (format-time-string ,time-format created-at))
	    . ,rest)))
       ((string-match "\\`FACE\\[\\([a-zA-Z0-9:-]+\\)\\]{" following)
	(let* ((face-name-str (match-string 1 following))
	       (str-after-brace (substring following (match-end 0)))
	       (face-sym (intern face-name-str))
	       (pair (twittering-generate-formater-for-current-level
		      str-after-brace status-sym prefix-sym))
	       (braced-body (car pair))
	       (rest (cdr pair)))
	  `((propertize (concat ,@braced-body) 'face ',face-sym)
	    . ,rest)))
       ((string-match "\\`\\(FILL\\|FOLD\\)\\(\\[\\([^]]*\\)\\]\\)?{"
		      following)
	(let* ((str-after-brace (substring following (match-end 0)))
	       (specifier (match-string 1 following))
	       (prefix-str (match-string 3 following))
	       (pair (twittering-generate-formater-for-current-level
		      str-after-brace status-sym prefix-sym))
	       (filled-body (car pair))
	       (formater
		`(lambda (,status-sym ,prefix-sym)
		   (let ((,prefix-sym (concat ,prefix-sym ,prefix-str)))
		     (concat ,@filled-body))))
	       (keep-newline (string= "FOLD" specifier))
	       (rest (cdr pair)))
	  `((twittering-update-filled-string
	     nil nil ,formater ,status-sym ,prefix-sym ,prefix-str
	     ,keep-newline)
	    . ,rest)))
       ((string-match regexp following)
	(let ((specifier (match-string 1 following))
	      (rest (substring following (match-end 0))))
	  `(,(cdr (assoc specifier table)) . ,rest)))
       (t
	`("%" . ,following)))))
   ((string-match "\\(%\\|}\\)" format-str)
    (let* ((sep (match-beginning 0))
	   (first (substring format-str 0 sep))
	   (last (substring format-str sep)))
      ;; Split before "%" or "}".
      `(,first . ,last)))
   (t
    `(,format-str . nil))))

(defun twittering-generate-formater-for-current-level (format-str status-sym prefix-sym)
  (let ((result nil)
	(rest format-str)
	(continue t))
    (while (and continue rest)
      (let* ((pair
	      (twittering-generate-formater-for-first-spec
	       rest status-sym prefix-sym))
	     (current-result (car pair)))
	(if current-result
	    (setq result (append result `(,current-result)))
	  ;; If `result' is nil, it means the end of the current level.
	  (setq continue nil))
	(setq rest (cdr pair))))
    `(,result . ,rest)))

(defun twittering-generate-format-status-function (format-str)
  (let* ((status-sym 'status)
	 (prefix-sym 'prefix)
	 (pair
	  (twittering-generate-formater-for-current-level
	   format-str status-sym prefix-sym))
	 (body (car pair))
	 (rest (cdr pair)))
    (cond
     ((null rest)
      `(lambda (status prefix)
	 (let* ((username (cdr (assq 'user-screen-name status)))
		(id (cdr (assq 'id status)))
		(text (cdr (assq 'text status)))
		(common-properties (list 'username username 'id id 'text text))
		(str (concat ,@body))
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
	     (remove-text-properties 0 (length str)
				     '(need-to-be-updated nil) str)
	     (put-text-property 0 (length str) 'need-to-be-updated
				`(twittering-format-status-for-redisplay
				  ,status ,prefix)
				str))
	   str)))
     (t
      (message "Failed to generate a status formater for `twittering-mode'.")
      nil))))

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
  "Show the number of characters in minibuffer."
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
    (re-search-forward "\\`[[:space:]]*@[a-zA-Z0-9_-]+\\([[:space:]]+@[a-zA-Z0-9_-]+\\)*" nil t)
    (re-search-forward "[^[:space:]]" nil t)))

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
				(concat "\\`@" username "\\(?:[\n\r \t]+\\)*")
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
  (twittering-call-api
   'get-list-index
   `((username . ,username)
     (sentinel . twittering-http-get-list-index-sentinel))))

(defun twittering-get-list-index-sync (username)
  (setq twittering-list-index-retrieved nil)
  (twittering-get-list-index username)
  (while (not twittering-list-index-retrieved)
    (sit-for 0.1))
  (cond
   ((stringp twittering-list-index-retrieved)
    (if (string= "" twittering-list-index-retrieved)
	(message "%s does not have a list." username)
      (message "%s" twittering-list-index-retrieved))
    nil)
   ((listp twittering-list-index-retrieved)
    twittering-list-index-retrieved)))

(defun twittering-get-and-render-timeline (&optional noninteractive id)
  (let ((spec (twittering-current-timeline-spec))
	(spec-string (twittering-current-timeline-spec-string)))
    (cond
     ((not (twittering-account-authorized-p))
      ;; ignore any requests if the account has not been authorized.
      (message "No account for Twitter has been authorized.")
      t)
     ((and noninteractive (twittering-process-active-p spec))
      ;; ignore non-interactive request if a process is waiting for responses.
      t)
     ((twittering-timeline-spec-primary-p spec)
      (let* ((is-search-spec (eq 'search (car spec)))
	     (default-number 20)
	     (max-number (if is-search-spec
			    100 ;; FIXME: refer to defconst.
			  twittering-max-number-of-tweets-on-retrieval))
	     (number twittering-number-of-tweets-on-retrieval)
	     (number (cond
		     ((integerp number) number)
		     ((string-match "^[0-9]+$" number)
		      (string-to-number number 10))
		     (t default-number)))
	     (number (min (max 1 number) max-number))
	     (latest-status
	      ;; Assume that a list which was returned by
	      ;; `twittering-current-timeline-data' is sorted.
	      (car (twittering-current-timeline-data spec)))
	     (since_id (cdr-safe (assq 'id latest-status)))
	     (word (when is-search-spec (cadr spec)))
	     (args
	      `((timeline-spec . ,spec)
		(timeline-spec-string . ,spec-string)
		(number . ,number)
		,@(when id `((max_id . ,id)))
		,@(cond
		   (is-search-spec `((word . ,word)))
		   ((and since_id (null id)) `((since_id . ,since_id)))
		   (t nil))))
	     (proc
	      (twittering-call-api 'retrieve-timeline args noninteractive)))
	(when proc
	  (twittering-register-process proc spec spec-string))))
     (t
      (let ((type (car spec)))
	(error "%s has not been supported yet" type))))))

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

(defun twittering-jojo-mode-p (spec)
  (let ((buffer (twittering-get-buffer-from-spec spec)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	twittering-jojo-mode))))

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

(defun twittering-direct-messages-timeline ()
  (interactive)
  (twittering-visit-timeline '(direct_messages)))

(defun twittering-sent-direct-messages-timeline ()
  (interactive)
  (twittering-visit-timeline '(direct_messages_sent)))

(defun twittering-update-active-buffers (&optional noninteractive)
  "Invoke `twittering-get-and-render-timeline' for each active buffer
managed by `twittering-mode'."
  (when (twittering-account-authorized-p)
    (let ((buffer-list (twittering-get-active-buffer-list)))
      (mapc (lambda (buffer)
	      (with-current-buffer buffer
		(twittering-get-and-render-timeline noninteractive)))
	    buffer-list))))

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

(defun twittering-delete-status (&optional id)
  (interactive)
  (let* ((id (get-text-property (point) 'id))
	 (username (get-text-property (point) 'username))
	 (text (copy-sequence (get-text-property (point) 'text)))
	 (text (progn
		 (set-text-properties 0 (length text) nil text)
		 text))
	 (width (max 40 ;; XXX
		     (- (frame-width)
			1 ;; margin for wide characters
			11 ;; == (length (concat "Delete \"" "\"? "))
			9) ;; == (length "(y or n) ")
		     ))
	 (mes (format "Delete \"%s\"? "
		      (if (< width (string-width text))
			  (concat
			   (truncate-string-to-width text (- width 3))
			   "...")
			text))))
    (cond
     ((not (string= username (twittering-get-username)))
      (message "The status is not yours!"))
     ((not id)
      (message "No status selected"))
     ((y-or-n-p mes)
      (twittering-call-api 'destroy-status `((id . ,id)))
      (twittering-delete-status-from-data-table id))
     (t
      (message "Request canceled")))))

(defun twittering-update-lambda ()
  (interactive)
  (when (and (string= "Japanese" current-language-environment)
	     (or (< 21 emacs-major-version)
		 (eq 'utf-8 (terminal-coding-system))))
    (let ((text (mapconcat
		 'char-to-string
		 (mapcar 'twittering-ucs-to-char
			 '(955 12363 12431 12356 12356 12424 955)) "")))
      (twittering-call-api 'update-status `((status . ,text))))))

(defun twittering-update-jojo (usr msg)
  (when (and (not (string= usr (twittering-get-username)))
	     (string= "Japanese" current-language-environment)
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
	(let ((text (concat "@" usr " "
			    (match-string-no-properties 2 msg)
			    (mapconcat
			     'char-to-string
			     (mapcar 'twittering-ucs-to-char
				     '(12288 12399 12387 33 63)) ""))))
	  (twittering-call-api 'update-status `((status . ,text)))))))

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
	(method (if remove 'destroy-friendships 'create-friendships))
	(mes (if remove "Unfollowing" "Following")))
    (unless username
      (setq username (or (twittering-read-username-with-completion
			  "who: " "" 'twittering-user-history)
			 "")))
    (if (string= "" username)
	(message "No user selected")
      (set-text-properties 0 (length username) nil username)
      (if (y-or-n-p (format "%s %s? " mes username))
	  (twittering-call-api method `((username . ,username)))
	(message "Request canceled")))))

(defun twittering-unfollow ()
  (interactive)
  (twittering-follow t))

(defun twittering-native-retweet ()
  (interactive)
  (let ((id (get-text-property (point) 'id))
	(text (copy-sequence (get-text-property (point) 'text)))
	(user (get-text-property (point) 'username))
	(width (max 40 ;; XXX
		    (- (frame-width)
		       1 ;; margin for wide characters
		       12 ;; == (length (concat "Retweet \"" "\"? "))
		       9) ;; == (length "(y or n) ")
		    )))
    (set-text-properties 0 (length text) nil text)
    (if id
	(if (not (string= user twittering-username))
	    (let ((mes (format "Retweet \"%s\"? "
			       (if (< width (string-width text))
				   (concat
				    (truncate-string-to-width text (- width 3))
				    "...")
				 text))))
	      (if (y-or-n-p mes)
		  (twittering-call-api 'retweet `((id . ,id)))
		(message "Request canceled")))
	  (message "Cannot retweet your own tweet"))
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
	(method (if remove 'destroy-favorites 'create-favorites)))
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
	      (twittering-call-api method `((id . ,id)))
	    (message "Request canceled")))
      (message "No status selected"))))

(defun twittering-unfavorite ()
  (interactive)
  (twittering-favorite t))

(defun twittering-visit-timeline (&optional timeline-spec initial)
  (interactive)
  (when (twittering-lookup-http-start-function)
    (twittering-initialize-global-variables-if-necessary)
    (twittering-prepare-account-info)
    (let ((timeline-spec
	   (or timeline-spec
	       (twittering-read-timeline-spec-with-completion
		"timeline: " initial t))))
      (when timeline-spec
	(twittering-verify-credentials)
	(switch-to-buffer (twittering-get-managed-buffer timeline-spec))))))

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
  (let* ((dummy-hist
	  (append twittering-timeline-history
		  (twittering-get-usernames-from-timeline)
		  '(":direct_messages" ":direct_messages_sent" ":friends"
		    ":home" ":mentions" ":public" ":replies"
		    ":retweeted_by_me" ":retweeted_to_me" ":retweets_of_me")))
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
  twittering-username)

(defun twittering-get-password ()
  twittering-password)

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

(defun twittering-goto-next-thing (&optional backward)
  "Go to next interesting thing. ex) username, URI, ... "
  (interactive)
  (let* ((property-change-f (if backward
			       'previous-single-property-change
			     'next-single-property-change))
	 (pos (funcall property-change-f (point) 'face)))
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
      (setq pos (funcall property-change-f pos 'face)))
    (when pos
      (goto-char pos))))

(defun twittering-goto-previous-thing (&optional backward)
  "Go to previous interesting thing. ex) username, URI, ... "
  (interactive)
  (twittering-goto-next-thing (not backward)))

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

                  (progn  (when  (
                   boundp  (  intern (
                    mapconcat 'identity '
                    ("twittering" "oauth"
                      "consumer" "key" ) "-"
                       )  )  )   (eval  ` (
                        setq ,(intern (mapconcat
                         (quote identity) (quote
                          ("twittering"    "oauth"
                           "consumer" "key")  )"-"
                           ))  (base64-decode-string
                         (map (quote string) (quote 1-)
                        (quote (83 88 75 114 88 73 79 117
                      101 109 109 105 82 123 75 120 78 73 
                     105 122 83 69 67 78   98 49 75 109 101 
                   120 62 62))))))) (       when ( boundp  (
                  intern (mapconcat '      identity'("twittering"
                 "oauth" "consumer"         "secret") "-")))(eval `
                (setq  ,(intern   (         mapconcat 'identity '(
               "twittering" "oauth"          "consumer" "secret") "-"))
              (base64-decode-string          (map (quote string) (quote
             1-) (quote (91 70                    113 87 83 123 75 112
            87 123 75 117 87 50                109 50  102  85 83 91 101
           49 87 116 100 73 101                  106 82 107 67 113  90 49
          75 68  99  52  79 120                   80 89  91  51  79 85 71
         110 101  110 91  49                      100 49   58  71))) )) )))
