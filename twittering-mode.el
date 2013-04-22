;;; twittering-mode.el --- Major mode for Twitter

;; Copyright (C) 2007, 2009, 2010 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO

;; Author: Y. Hayamizu <y.hayamizu@gmail.com>
;;	Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;	Alberto Garcia <agarcia@igalia.com>
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
(require 'url)

(defconst twittering-mode-version "HEAD")
(defconst twittering-mode-identity "$Id$")
(defvar twittering-api-host "api.twitter.com")
(defvar twittering-api-search-host "search.twitter.com")
(defvar twittering-web-host "twitter.com")
(defvar twittering-oauth-request-token-url-without-scheme
  "://api.twitter.com/oauth/request_token")
(defvar twittering-oauth-authorization-url-base-without-scheme
  "://api.twitter.com/oauth/authorize?oauth_token=")
(defvar twittering-oauth-access-token-url-without-scheme
  "://api.twitter.com/oauth/access_token")

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
OAuth Authentication. The symbol `xauth' means xAuth Authentication.
OAuth Authentication requires `twittering-oauth-consumer-key' and
`twittering-oauth-consumer-secret'. Additionally, it requires an external
command `curl' or another command included in `tls-program', which may be
`openssl' or `gnutls-cli', for SSL.")

(defvar twittering-account-authorization nil
  "State of account authorization for `twittering-username' and
`twittering-password'.  The value is one of the following symbols:
nil -- The account have not been authorized yet.
queried -- The authorization has been queried, but not finished yet.
authorized -- The account has been authorized.")

(defvar twittering-oauth-use-ssl t
  "*Whether to use SSL on authentication via OAuth. Twitter requires SSL
on authorization via OAuth.")
(defvar twittering-oauth-invoke-browser nil
  "*Whether to invoke a browser on authorization of access key automatically.")

(defvar twittering-oauth-consumer-key nil)
(defvar twittering-oauth-consumer-secret nil)
(defvar twittering-oauth-access-token-alist nil)

(defconst twittering-max-number-of-tweets-on-retrieval 200
  "The maximum number of `twittering-number-of-tweets-on-retrieval'.")

(defvar twittering-number-of-tweets-on-retrieval 20
  "*The number of tweets which will be retrieved in one request.
The upper limit is `twittering-max-number-of-tweets-on-retrieval'.")

(defvar twittering-tinyurl-service 'tinyurl
  "*The service to shorten URI.
This must be one of key symbols of `twittering-tinyurl-services-map'.
To use 'bit.ly or 'j.mp, you have to configure `twittering-bitly-login' and
`twittering-bitly-api-key'.")

(defvar twittering-tinyurl-services-map
  '((bit.ly twittering-make-http-request-for-bitly
	    (lambda (service reply)
	      (if (string-match "\n\\'" reply)
		  (substring reply 0 (match-beginning 0))
		reply)))
    (goo.gl
     (lambda (service longurl)
       (twittering-make-http-request-from-uri
	"POST" '(("Content-Type" . "application/json"))
	"https://www.googleapis.com/urlshortener/v1/url"
	(concat "{\"longUrl\": \"" longurl "\"}")))
     (lambda (service reply)
       (when (string-match "\"id\"[[:space:]]*:[[:space:]]*\"\\([^\"]*\\)\""
			   reply)
	 (match-string 1 reply))))
    (is.gd . "http://is.gd/create.php?format=simple&url=")
    (j.mp twittering-make-http-request-for-bitly
	  (lambda (service reply)
	    (if (string-match "\n\\'" reply)
		(substring reply 0 (match-beginning 0))
	      reply)))
    (migre.me . "http://migre.me/api.txt?url=")
    (tinyurl . "http://tinyurl.com/api-create.php?url=")
    (toly
     (lambda (service longurl)
       (twittering-make-http-request-from-uri
	"POST" nil
	"http://to.ly/api.php"
	(concat "longurl=" (twittering-percent-encode longurl))))))
  "Alist of URL shortening services.
The key is a symbol specifying the service.
The value is a string or a list consisting of two elements at most.

If the value is a string, `(concat THE-FIRST-ELEMENT longurl)' is used as the
URL invoking the service.
If the value is a list, it is interpreted as follows.
The first element specifies how to make a HTTP request for shortening a URL.
If the first element is a string, `(concat THE-FIRST-ELEMENT longurl)' is
used as the URL invoking the service.
If the first element is a function, it is called as `(funcall THE-FIRST-ELEMENT
service-symbol longurl)' to obtain a HTTP request alist for invoking the
service, which must be generated by `twittering-make-http-request'.

The second element specifies how to post-process a HTTP reply by the HTTP
request.
If the second element is nil, the reply is directly used as a shortened URL.
If the second element is a function, it is called as `(funcall
THE-SECOND-ELEMENT service-symbol HTTP-reply-string)' and its result is used
as a shortened URL.")

(defvar twittering-bitly-login nil
  "*The login name for URL shortening service bit.ly and j.mp.")
(defvar twittering-bitly-api-key nil
  "*The API key for URL shortening service bit.ly and j.mp.")

(defvar twittering-mode-map (make-sparse-keymap))
(defvar twittering-mode-menu-on-uri-map (make-sparse-keymap "Twittering Mode"))
(defvar twittering-mode-on-uri-map (make-sparse-keymap))

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

(defvar twittering-timer-interval-for-redisplaying 5.0
  "The interval of auto redisplaying statuses.
Each time Emacs remains idle for the interval, twittering-mode updates parts
requiring to be redrawn.")

(defvar twittering-username nil
  "*An username of your Twitter account.")

(defvar twittering-password nil
  "*A password of your Twitter account. Leave it blank is the
recommended way because writing a password in .emacs file is so
dangerous.")

(defvar twittering-initial-timeline-spec-string ":home"
  "*An initial timeline spec string or a list of timeline spec strings.
This specifies one or more initial timeline spec strings, which are
automatically visited when invoking `twittering-mode' or `twit'.

If it is a string, it specifies a timeline spec string.
If it is a list of strings, it specifies multiple timeline spec strings.")

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
 `((\"FRIENDS\" . \"my-account/friends-list\")
   (\"related-to\" .
            ,(lambda (username)
               (if username
                   (format \":search/to:%s OR from:%s OR @%s/\"
                           username username username)
                 \":home\")))),
then you can use \"$FRIENDS\" and \"$related-to(USER)\" as
\"my-account/friends-list\" and \":search/to:USER OR from:USER OR @USER/\",
respectively.")

(defvar twittering-current-timeline-spec-string nil
  "The current timeline spec string. This variable should not be referred
directly. Use `twittering-current-timeline-spec-string' or
`twittering-current-timeline-spec'.")
(defvar twittering-list-index-retrieved nil)

(defvar twittering-process-info-alist nil
  "Alist of active process and timeline spec retrieved by the process.")

(defvar twittering-server-info-alist nil
  "Alist of server information.")

(defvar twittering-api-limit-info-alist '()
  "Alist of an API identifier and an alist representing rate limit for the API.")

(defvar twittering-timeline-spec-to-api-table '()
  "Alist of a timeline spec and an API identifier for retrieving the timeline.")


(defvar twittering-mode-init-hook nil
  "*Hook run after initializing global variables for `twittering-mode'.")
(defvar twittering-mode-hook nil
  "*Hook run every time a buffer is initialized as a twittering-mode buffer.")

(defvar twittering-cookie-alist nil
  "Alist for stroing cookies for each account.
This variable stores an alist.
A key of the alist is a string that is a screen name of an account.
A value of the alist is a cookie alist which corresponds to a list of
a pair of a cookie name and value.")

(defvar twittering-new-tweets-count 0
  "Number of new tweets when `twittering-new-tweets-hook' is run.")
(defvar twittering-new-tweets-spec nil
  "Timeline spec, which new tweets belong to, when
`twittering-new-tweets-hook' is run.")
(defvar twittering-new-tweets-statuses nil
  "New tweet status messages, when
`twittering-new-tweets-hook' is run.")

(defvar twittering-new-tweets-hook nil
  "*Hook run when new tweets are received.

You can read `twittering-new-tweets-count' or `twittering-new-tweets-spec'
to get the number of new tweets received when this hook is run.")

(defvar twittering-rendered-new-tweets-spec nil
  "A timeline spec of newly rendered tweets.
This variable is bound when invoking hooks registered with
`twittering-new-tweets-rendered-hook'.")

(defvar twittering-rendered-new-tweets-spec-string nil
  "A timeline spec string of newly rendered tweets.
This variable is bound when invoking hooks registered with
`twittering-new-tweets-rendered-hook'.")

(defvar twittering-rendered-new-tweets nil
  "A list of newly rendered tweets.
Hooks registered with `twittering-new-tweets-rendered-hook' can use this
variable as a list of rendered tweets. Each tweet is represented as an alist.
You can refer to a property of a tweet alist as
 (cdr (assq PROPERTY-SYMBOL TWEET-ALIST)).
Valid symbols are following; id, text, user-name, user-screen-name, user-id,
 source, source-uri.
In the list, tweets are placed in order of time. The car of the list is the
latest one, and the last is the oldest one.")

(defvar twittering-new-tweets-rendered-hook nil
  "*Hook run when new tweets are rendered.
When the registered functions are called, the current buffer is the buffer
that the new tweets are just rendered on.
The functions can refer to the timeline spec and timeline spec string as
`twittering-rendered-new-tweets-spec' and
`twittering-rendered-new-tweets-spec-string', repectively.
Hooks can also use the local variable `twittering-rendered-new-tweets' as a
list of rendered tweets.
For the detail of the representation of tweets, see the variable
`twittering-rendered-new-tweets'.")

(defvar twittering-active-mode nil
  "Non-nil if new statuses should be retrieved periodically.
Do not modify this variable directly. Use `twittering-activate-buffer',
`twittering-deactivate-buffer', `twittering-toggle-activate-buffer' or
`twittering-set-active-flag-for-buffer'.")

(defvar twittering-jojo-mode nil)
(defvar twittering-reverse-mode nil
  "*Non-nil means tweets are aligned in reverse order of `http://twitter.com/'.")
(defvar twittering-display-remaining nil
  "*If non-nil, display remaining of rate limit on the mode-line.")
(defvar twittering-display-connection-method t
  "*If non-nil, display the current connection method on the mode-line.")
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
 %RT{...} - strings rendered only when the tweet is a retweet.
            The braced strings are rendered with the information of the
            retweet itself instead of that of the retweeted original tweet.
            For example, %s for a retweet means who posted the original
            tweet, but %RT{%s} means who retweeted it.
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
                      The prefix is optional. This keeps newlines and does not
                      squeeze a series of white spaces.
                      You can use any other specifiers in braces.
 %f - source
 %# - id
")

(defvar twittering-retweet-format '(nil _ " RT: %t (via @%s)")
  "*A format string or a skeleton for retweet.
If the value is a string, it means a format string for generating an initial
string of a retweet. The format string is converted with the below replacement
table. And then, the cursor is placed on the next of the initial string.
It is equivalent to the skeleton '(nil STRING _).
Note that this string is inserted before the edit skeleton specified by
`twittering-edit-skeleton' is performed.

If the value is a list, it is treated as a skeleton used with
`skeleton-insert'. The strings included in the list are converted with the
following replacement table. And then, the list with converted strings is
inserted by `skeleton-insert'.
Note that this skeleton is performed before the edit skeleton specified by
`twittering-edit-skeleton' is performed.

Replacement table:
 %s - The screen-name of the cited tweet.
 %t - The text of the cited tweet.
 %u - The URL of the cited tweet.
 %# - The ID of the cited tweet.
 %% - % itself.")

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

(defvar twittering-disable-overlay-on-too-long-string nil
  "*If non-nil, disable overlay on too long string on edit buffer.

If nil, `twittering-edit-mode' puts an overlay `twittering-warning-overlay' on
characters following the 140th character.

On some environments, some input methods seem to interfere the update of the
overlay. In such case, you may avoid the problems by setting this variable to
non-nil.")

(defvar twittering-use-show-minibuffer-length t
  "*Show current length of minibuffer if this variable is non-nil.

We suggest that you should set to nil to disable the showing function
when it conflict with your input method (such as AquaSKK, etc.)")

(defvar twittering-notify-successful-http-get t)

(defvar twittering-use-ssl t
  "Use SSL connection if this variable is non-nil.

SSL connections use 'curl' command as a backend.")
(defvar twittering-allow-insecure-server-cert nil
  "*If non-nil, twittering-mode allows insecure server certificates.")

(defvar twittering-curl-program nil
  "Cache a result of `twittering-find-curl-program'.
DO NOT SET VALUE MANUALLY.")
(defvar twittering-curl-program-https-capability nil
  "Cache a result of `twittering-start-http-session-curl-https-p'.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-wget-program nil
  "Cache a result of `twittering-find-wget-program'.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-tls-program nil
  "*List of strings containing commands to start TLS stream to a host.
Each entry in the list is tried until a connection is successful.
%h is replaced with server hostname, %p with port to connect to.
Also see `tls-program'.
If nil, this is initialized with a list of valied entries extracted from
`tls-program'.")

(defvar twittering-connection-type-order
  '(curl wget urllib-http native urllib-https))
  "*A list of connection methods in the preferred order."

(defvar twittering-connection-type-table
  '((native (check . t)
	    (https . twittering-start-http-session-native-tls-p)
	    (send-http-request . twittering-send-http-request-native)
	    (pre-process-buffer . twittering-pre-process-buffer-native))
    (curl (check . twittering-start-http-session-curl-p)
	  (https . twittering-start-http-session-curl-https-p)
	  (send-http-request . twittering-send-http-request-curl)
	  (pre-process-buffer . twittering-pre-process-buffer-curl))
    (wget (check . twittering-start-http-session-wget-p)
	  (https . t)
	  (send-http-request . twittering-send-http-request-wget)
	  (pre-process-buffer . twittering-pre-process-buffer-wget))
    (urllib-http
     (display-name . "urllib")
     (check . twittering-start-http-session-urllib-p)
     (https . nil)
     (send-http-request . twittering-send-http-request-urllib)
     (pre-process-buffer . twittering-pre-process-buffer-urllib))
    (urllib-https
     (display-name . "urllib")
     (check . twittering-start-http-session-urllib-p)
     (https . twittering-start-http-session-urllib-https-p)
     (send-http-request . twittering-send-http-request-urllib)
     (pre-process-buffer . twittering-pre-process-buffer-urllib)))
  "A list of alist of connection methods.")

(defvar twittering-format-status-function-source ""
  "The status format string that has generated the current
`twittering-format-status-function'.")
(defvar twittering-format-status-function nil
  "The formating function generated from `twittering-format-status-function-source'.")
(defvar twittering-format-status-function-without-compile nil
  "The formating function generated from `twittering-format-status-function-source',
which is a lambda expression without being compiled.")

(defvar twittering-timeline-data-table (make-hash-table :test 'equal))

(defvar twittering-username-face 'twittering-username-face)
(defvar twittering-uri-face 'twittering-uri-face)

(defvar twittering-use-native-retweet nil
  "Post retweets using native retweets if this variable is non-nil.")

(defvar twittering-update-status-function
  'twittering-update-status-from-pop-up-buffer
  "The function used to posting a tweet. It takes 5 arguments,
INIT-STR, REPLY-TO-ID, USERNAME, TWEET-TYPE, CURRENT-SPEC.
The first argument INIT-STR is nil or an initial text to be edited.
REPLY-TO-ID and USERNAME are an ID and a user-screen-name of a tweet to
which you are going to reply. If the tweet is not a reply, they are nil.
TWEET-TYPE is a symbol specifying a type of a tweet being edited. It must
be one of 'direct-message, 'normal, 'organic-retweet and 'reply.
CURRENT-SPEC means on which timeline the function is called.

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
(defvar twittering-private-info-file-loaded nil
  "Whether the private info file has been loaded or not.")
(defvar twittering-variables-stored-with-encryption
  '(twittering-oauth-access-token-alist))

(defvar twittering-api-prefix "1/")
(defvar twittering-search-api-method "search")
(defvar twittering-web-path-prefix "")

(defvar twittering-service-method 'twitter-api-v1.1
  "*Service method for `twittering-mode'.
The symbol `twitter' means Twitter Service. The symbol `statusnet' means
StatusNet Service.")

(defvar twittering-service-method-table
  '((twitter (status-url . twittering-get-status-url-twitter)
	     (search-url . twittering-get-search-url-twitter))
    (twitter-api-v1.1
     (status-url . twittering-get-status-url-twitter)
     (search-url . twittering-get-search-url-twitter))
    (statusnet (status-url . twittering-get-status-url-statusnet)
	       (search-url . twittering-get-search-url-statusnet)))
  "A list of alist of service methods.")

(defvar twittering-timeline-header-face 'twittering-timeline-header-face
  "*Face for the header on `twittering-mode'.
The face is used for rendering `twittering-timeline-header'.")
(defvar twittering-timeline-footer-face 'twittering-timeline-footer-face
  "*Face for the footer on `twittering-mode'.
The face is used for rendering `twittering-timeline-footer'.")
(defvar twittering-timeline-header "-- Press Enter here to update --\n"
  "*Timeline header string on `twittering-mode'.
The string is rendered on the beginning of a `twittering-mode' buffer.
Its face is specified by `twittering-timeline-header-face'.")
(defvar twittering-timeline-footer "-- Press Enter here to update --"
  "*Timeline footer string on `twittering-mode'.
The string is rendered on the end of a `twittering-mode' buffer.
Its face is specified by `twittering-timeline-footer-face'.")

(defvar twittering-pop-to-buffer-function
  'twittering-pop-to-buffer-in-bottom-largest-window
  "*Function being invoked by `twittering-pop-to-buffer'.
It will receive an argument, the buffer being selected.
For example, the following functions can be used; `pop-to-buffer',
`twittering-pop-to-buffer-simple',
`twittering-pop-to-buffer-in-current-window',
`twittering-pop-to-buffer-in-largest-window', and
`twittering-pop-to-buffer-in-bottom-largest-window'.")

(defvar twittering-relative-retrieval-interval-alist
  '(("\\`:direct.*\\'" 4)
    (":home" ":mentions" 1)
    (t 1))
  "*An alist of relative intervals of retrieving timelines.
Each element looks like (TIMELINE-SPEC-REGEXP RELATIVE-INTERVAL).

TIMELINE-SPEC-REGEXP must be t or a regexp string specifying primary
timeline specs.
If TIMELINE-SPEC-REGEXP is t, it matches all timelines.
RELATIVE-INTERVAL must be zero or a positive integer specifying relative
interval of retrieving timelines that match TIMELINE-SPEC-REGEXP.

An interval for a timeline is determined as follows;
1. Find the first element where TIMELINE-SPEC-REGEXP matches the
   timeline or TIMELINE-SPEC-REGEXP is t.
   If no elements are found, the interval is `twittering-timer-interval'.
2. Check the RELATIVE-INTERVAL of the element.
   If RELATIVE-INTERVAL is a positive integer, the interval is
   RELATIVE-INTERVAL times as long as `twittering-timer-interval'.

   If RELATIVE-INTERVAL is zero, the interval is infinity.
   The timeline is not retrieved automatically.")

(defvar twittering-relative-retrieval-count-alist '()
  "An alist for counting retrieval of primary timelines.")

;;;;
;;;; Macro and small utility function
;;;;

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

(defmacro twittering-wait-while (timeout interval condition &optional form &rest timeout-forms)
  "Wait while CONDITION returns non-nil until TIMEOUT seconds passes.

The form CONDITION is repeatedly evaluated for every INTERVAL seconds
until CONDITION returns nil or TIMEOUT seconds passes unless TIMEOUT is nil.
If TIMEOUT is nil, there is no time limit.

If CONDITION returns nil, evaluate the form FORM and return its value.
If TIMEOUT seconds passes, evaluate the forms TIMEOUT-FORMS and return
the value of the last form in TIMEOUT-FORMS."
  `(lexical-let (,@(when timeout `((timeout ,timeout)))
		 (interval ,interval)
		 (current 0.0))
     (while (and ,@(when timeout '((< current timeout)))
		 ,condition)
       (sit-for interval)
       (setq current (+ current interval)))
     ,(when (or form timeout-forms)
	(if (null timeout)
	    form
	  `(if (< current timeout)
	       ,form
	     ,@timeout-forms)))))


(defun twittering-extract-matched-substring-all (regexp str)
  (let ((pos 0)
	(result nil))
    (while (string-match regexp str pos)
      (setq result (cons (match-string 1 str) result))
      (setq pos (match-end 0)))
    (reverse result)))

(defun twittering-process-alive-p (proc)
  "Return non-nil if PROC is alive."
  (not (memq (process-status proc) '(nil closed exit failed signal))))

(defun twittering-start-process-with-sentinel (name buffer program args sentinel)
  "Start a program in a subprocess with a sentinel.

This function is the same as `start-process' except that SENTINEL must
be invoked when the process is successfully started."
  (let ((proc (apply 'start-process name buffer program args)))
    (when (and proc (functionp sentinel))
      (if (twittering-process-alive-p proc)
	  (set-process-sentinel proc sentinel)
	;; Ensure that the sentinel is invoked if a subprocess is
	;; successfully started.
	(funcall sentinel proc "finished")))
    proc))

(defun twittering-parse-time-string (str &optional round-up)
  "Parse the time-string STR into (SEC MIN HOUR DAY MON YEAR DOW DST TZ).
This function is the same as `parse-time-string' except to complement the
lacked parameters with the current time.
If ROUND-UP is nil, complement the lacked parameters with the oldest ones.
If ROUND-UP is non-nil, complement the lacked parameters with the latest ones.
For example, (twittering-parse-time-string \"2012-04-20\")
returns (0 0 0 20 4 2012 nil nil 32400).
And (twittering-parse-time-string \"2012-04-20\" t)
returns (59 59 23 20 4 2012 nil nil 32400).
The values are identical to those of `decode-time', but any values that are
unknown are returned as nil."
  (let* ((parsed (parse-time-string str))
	 (current (decode-time (current-time)))
	 (replacement-alist
	  `((SEC . ,(if round-up
			59
		      0))
	    (MIN . ,(if round-up
			59
		      0))
	    (HOUR . ,(if round-up
			 23
		       0))
	    (DAY . nil)
	    (MON . nil)
	    (YEAR . nil)
	    (DOW . nil)
	    (DST . nil)
	    (TZ . nil)))
	 (sym-list (mapcar 'car replacement-alist))
	 (result nil))
    (while (and parsed current sym-list)
      (let* ((sym (car sym-list))
	     (v (or (car parsed)
		    (cdr (assq sym replacement-alist))
		    ;; If `sym' is not 'DOW and it is bound to nil
		    ;; in `replacement-alist', use `current'.
		    (unless (eq sym 'DOW)
		      (car current)))))
	(setq result (cons v result)))
      (setq parsed (cdr parsed))
      (setq current (cdr current))
      (setq sym-list (cdr sym-list)))
    (reverse result)))

;;;;
;;;; Utility for portability
;;;;

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

;;;;
;;;; Debug mode
;;;;

(defvar twittering-debug-mode nil)
(defvar twittering-debug-buffer "*debug*")

(defun twittering-get-or-generate-buffer (buffer)
  (if (bufferp buffer)
      (if (buffer-live-p buffer)
	  buffer
	(generate-new-buffer (buffer-name buffer)))
    (if (stringp buffer)
	(or (get-buffer buffer)
	    (generate-new-buffer buffer)))))

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

;;;;
;;;; Proxy setting / functions
;;;;

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

;;;;
;;;; Functions for URL library
;;;;

(defvar twittering-url-show-status nil
  "*Whether to show a running total of bytes transferred.")

;;;;
;;;; CA certificate
;;;;

(defvar twittering-cert-file nil
  "The full-path of the file including the certificates authorizing
servers on SSL.")

(defconst twittering-ca-cert-list
  '(;; Equifax Secure Certificate Authority
    ;; subject= /C=US/O=Equifax/OU=Equifax Secure Certificate Authority
    ;; SHA1 Fingerprint=D2:32:09:AD:23:D3:14:23:21:74:E4:0D:7F:9D:62:13:97:86:63:3A
    ;; Retrieved from: https://www.geotrust.com/resources/root-certificates/index.html
    ;; URL: https://www.geotrust.com/resources/root_certificates/certificates/Equifax_Secure_Certificate_Authority.cer
    ;; for www.googleapis.com
    "-----BEGIN CERTIFICATE-----
MIIDIDCCAomgAwIBAgIENd70zzANBgkqhkiG9w0BAQUFADBOMQswCQYDVQQGEwJV
UzEQMA4GA1UEChMHRXF1aWZheDEtMCsGA1UECxMkRXF1aWZheCBTZWN1cmUgQ2Vy
dGlmaWNhdGUgQXV0aG9yaXR5MB4XDTk4MDgyMjE2NDE1MVoXDTE4MDgyMjE2NDE1
MVowTjELMAkGA1UEBhMCVVMxEDAOBgNVBAoTB0VxdWlmYXgxLTArBgNVBAsTJEVx
dWlmYXggU2VjdXJlIENlcnRpZmljYXRlIEF1dGhvcml0eTCBnzANBgkqhkiG9w0B
AQEFAAOBjQAwgYkCgYEAwV2xWGcIYu6gmi0fCG2RFGiYCh7+2gRvE4RiIcPRfM6f
BeC4AfBONOziipUEZKzxa1NfBbPLZ4C/QgKO/t0BCezhABRP/PvwDN1Dulsr4R+A
cJkVV5MW8Q+XarfCaCMczE1ZMKxRHjuvK9buY0V7xdlfUNLjUA86iOe/FP3gx7kC
AwEAAaOCAQkwggEFMHAGA1UdHwRpMGcwZaBjoGGkXzBdMQswCQYDVQQGEwJVUzEQ
MA4GA1UEChMHRXF1aWZheDEtMCsGA1UECxMkRXF1aWZheCBTZWN1cmUgQ2VydGlm
aWNhdGUgQXV0aG9yaXR5MQ0wCwYDVQQDEwRDUkwxMBoGA1UdEAQTMBGBDzIwMTgw
ODIyMTY0MTUxWjALBgNVHQ8EBAMCAQYwHwYDVR0jBBgwFoAUSOZo+SvSspXXR9gj
IBBPM5iQn9QwHQYDVR0OBBYEFEjmaPkr0rKV10fYIyAQTzOYkJ/UMAwGA1UdEwQF
MAMBAf8wGgYJKoZIhvZ9B0EABA0wCxsFVjMuMGMDAgbAMA0GCSqGSIb3DQEBBQUA
A4GBAFjOKer89961zgK5F7WF0bnj4JXMJTENAKaSbn+2kmOeUJXRmm/kEd5jhW6Y
7qj/WsjTVbJmcVfewCHrPSqnI0kBBIZCe/zuf6IWUrVnZ9NA2zsmWLIodz2uFHdh
1voqZiegDfqnc1zqcPGUIWVEX/r87yloqaKHee9570+sB3c4
-----END CERTIFICATE-----
"
    ;; VeriSign Class 3 Public Primary CA - G2
    ;; subject= /C=US/O=VeriSign, Inc./OU=Class 3 Public Primary Certification Authority - G2/OU=(c) 1998 VeriSign, Inc. - For authorized use only/OU=VeriSign Trust Network
    ;; SHA1 Fingerprint=85:37:1C:A6:E5:50:14:3D:CE:28:03:47:1B:DE:3A:09:E8:F8:77:0F
    ;; Retrieved from: https://www.verisign.com/support/roots.html
    ;; URL: https://www.verisign.com/repository/roots/root-certificates/PCA-3G2.pem
    ;; for api.twitter.com
    "-----BEGIN CERTIFICATE-----
MIIDAjCCAmsCEH3Z/gfPqB63EHln+6eJNMYwDQYJKoZIhvcNAQEFBQAwgcExCzAJ
BgNVBAYTAlVTMRcwFQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE8MDoGA1UECxMzQ2xh
c3MgMyBQdWJsaWMgUHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSAtIEcy
MTowOAYDVQQLEzEoYykgMTk5OCBWZXJpU2lnbiwgSW5jLiAtIEZvciBhdXRob3Jp
emVkIHVzZSBvbmx5MR8wHQYDVQQLExZWZXJpU2lnbiBUcnVzdCBOZXR3b3JrMB4X
DTk4MDUxODAwMDAwMFoXDTI4MDgwMTIzNTk1OVowgcExCzAJBgNVBAYTAlVTMRcw
FQYDVQQKEw5WZXJpU2lnbiwgSW5jLjE8MDoGA1UECxMzQ2xhc3MgMyBQdWJsaWMg
UHJpbWFyeSBDZXJ0aWZpY2F0aW9uIEF1dGhvcml0eSAtIEcyMTowOAYDVQQLEzEo
YykgMTk5OCBWZXJpU2lnbiwgSW5jLiAtIEZvciBhdXRob3JpemVkIHVzZSBvbmx5
MR8wHQYDVQQLExZWZXJpU2lnbiBUcnVzdCBOZXR3b3JrMIGfMA0GCSqGSIb3DQEB
AQUAA4GNADCBiQKBgQDMXtERXVxp0KvTuWpMmR9ZmDCOFoUgRm1HP9SFIIThbbP4
pO0M8RcPO/mn+SXXwc+EY/J8Y8+iR/LGWzOOZEAEaMGAuWQcRXfH2G71lSk8UOg0
13gfqLptQ5GVj0VXXn7F+8qkBOvqlzdUMG+7AUcyM83cV5tkaWH4mx0ciU9cZwID
AQABMA0GCSqGSIb3DQEBBQUAA4GBAFFNzb5cy5gZnBWyATl4Lk0PZ3BwmcYQWpSk
U01UbSuvDV1Ai2TT1+7eVmGSX6bEHRBhNtMsJzzoKQm5EWR0zLVznxxIqbxhAe7i
F6YM40AIOw7n60RzKprxaZLvcRTDOaxxp5EJb+RxBrO6WVcmeQD2+A2iMzAo1KpY
oJ2daZH9
-----END CERTIFICATE-----
"
    ;; GeoTrust Global CA
    ;; subject= /C=US/O=GeoTrust Inc./CN=GeoTrust Global CA
    ;; SHA1 Fingerprint=DE:28:F4:A4:FF:E5:B9:2F:A3:C5:03:D1:A3:49:A7:F9:96:2A:82:12
    ;; Retrieved from: https://www.geotrust.com/resources/root-certificates/index.html
    ;; URL: https://www.geotrust.com/resources/root_certificates/certificates/GeoTrust_Global_CA.cer
    ;; for search.twitter.com
    "-----BEGIN CERTIFICATE-----
MIIDVDCCAjygAwIBAgIDAjRWMA0GCSqGSIb3DQEBBQUAMEIxCzAJBgNVBAYTAlVT
MRYwFAYDVQQKEw1HZW9UcnVzdCBJbmMuMRswGQYDVQQDExJHZW9UcnVzdCBHbG9i
YWwgQ0EwHhcNMDIwNTIxMDQwMDAwWhcNMjIwNTIxMDQwMDAwWjBCMQswCQYDVQQG
EwJVUzEWMBQGA1UEChMNR2VvVHJ1c3QgSW5jLjEbMBkGA1UEAxMSR2VvVHJ1c3Qg
R2xvYmFsIENBMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA2swYYzD9
9BcjGlZ+W988bDjkcbd4kdS8odhM+KhDtgPpTSEHCIjaWC9mOSm9BXiLnTjoBbdq
fnGk5sRgprDvgOSJKA+eJdbtg/OtppHHmMlCGDUUna2YRpIuT8rxh0PBFpVXLVDv
iS2Aelet8u5fa9IAjbkU+BQVNdnARqN7csiRv8lVK83Qlz6cJmTM386DGXHKTubU
1XupGc1V3sjs0l44U+VcT4wt/lAjNvxm5suOpDkZALeVAjmRCw7+OC7RHQWa9k0+
bw8HHa8sHo9gOeL6NlMTOdReJivbPagUvTLrGAMoUgRx5aszPeE4uwc2hGKceeoW
MPRfwCvocWvk+QIDAQABo1MwUTAPBgNVHRMBAf8EBTADAQH/MB0GA1UdDgQWBBTA
ephojYn7qwVkDBF9qn1luMrMTjAfBgNVHSMEGDAWgBTAephojYn7qwVkDBF9qn1l
uMrMTjANBgkqhkiG9w0BAQUFAAOCAQEANeMpauUvXVSOKVCUn5kaFOSPeCpilKIn
Z57QzxpeR+nBsqTP3UEaBU6bS+5Kb1VSsyShNwrrZHYqLizz/Tt1kL/6cdjHPTfS
tQWVYrmm3ok9Nns4d0iXrKYgjy6myQzCsplFAMfOEVEiIuCl6rYVSAlk6l5PdPcF
PseKUgzbFbS9bZvlxrFUaKnjaZC2mqUPuLk/IH2uSrW4nOQdtqvmlKXBx4Ot2/Un
hw4EbNX/3aBd7YdStysVAq45pmp06drE57xNNB6pXE0zX5IJL4hmXXeXxx12E6nV
5fEWCRE11azbJHFwLJhWC9kXtNHjUStedejV0NxPNO3CBWaAocvmMw==
-----END CERTIFICATE-----
"))

(defun twittering-delete-ca-cert ()
  (when (and twittering-cert-file
	     (file-exists-p twittering-cert-file))
    (delete-file twittering-cert-file))
  (setq twittering-cert-file nil))

(defun twittering-ensure-ca-cert ()
  "Return a full-path of the file including CA certificates.

If it does not exist, create it. The directory includes root certificates
in \"hash format\". In detail, see verify(1SSL)."
  (unless twittering-cert-file
    (let ((coding-system-for-write 'iso-safe)
	  (file (make-temp-file "twmode-cacert")))
      (with-temp-file file
	(apply 'insert twittering-ca-cert-list))
      (setq twittering-cert-file file)
      (add-hook 'kill-emacs-hook 'twittering-delete-ca-cert)))
  twittering-cert-file)

;;;;
;;;; User agent
;;;;

(defvar twittering-user-agent-function 'twittering-user-agent-default-function)

(defun twittering-user-agent-default-function ()
  "Twittering mode default User-Agent function."
  (format "Emacs/%d.%d Twittering-mode/%s"
	  emacs-major-version emacs-minor-version
	  twittering-mode-version))

(defun twittering-user-agent ()
  "Return User-Agent header string."
  (funcall twittering-user-agent-function))

;;;;
;;;; Basic HTTP functions (general)
;;;;

(defun twittering-percent-encode (str &optional coding-system)
  "Encode STR according to Percent-Encoding defined in RFC 3986."
  (twittering-oauth-url-encode str coding-system))

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
	     (entry (cons `(symbol . ,candidate)
			  (cdr (assq candidate table))))
	     (entry (if (assq 'display-name entry)
			entry
		      (cons `(display-name . ,(symbol-name candidate))
			    entry)))
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

(defun twittering-get-connection-method-name (use-ssl)
  "Return a name of the preferred connection method.
If USE-SSL is non-nil, return a connection method for HTTPS.
If USE-SSL is nil, return a connection method for HTTP."
  (cdr (assq 'display-name (twittering-lookup-connection-type use-ssl))))

(defun twittering-lookup-http-start-function (&optional order table)
  "Decide a connection method from currently available methods."
  (let ((entry
	 (twittering-lookup-connection-type twittering-use-ssl order table)))
    (cdr (assq 'send-http-request entry))))

(defun twittering-ensure-connection-method (&optional order table)
  "Ensure a connection method with a compromise.
Return nil if no connection methods are available with a compromise."
  (let* ((use-ssl (or twittering-use-ssl twittering-oauth-use-ssl))
	 (entry (twittering-lookup-connection-type use-ssl order table)))
    (cond
     (entry
      t)
     ((and (null entry) use-ssl
	   (yes-or-no-p "HTTPS(SSL) is unavailable. Use HTTP instead? "))
      ;; Fall back on connection without SSL.
      (setq twittering-use-ssl nil)
      (setq twittering-oauth-use-ssl nil)
      (twittering-update-mode-line)
      (twittering-ensure-connection-method order table))
     (t
      (message "No connection methods are available.")
      nil))))

(defun twittering-make-http-request (method header-list host port path query-parameters post-body use-ssl)
  "Returns an alist specifying a HTTP request.
METHOD specifies HTTP method. It must be \"GET\" or \"POST\".
HEADER-LIST is a list of (field-name . field-value) specifying HTTP header
fields. The fields \"Host\", \"User-Agent\" and \"Content-Length\" are
automatically filled if necessary.
HOST specifies the host.
PORT specifies the port. This must be an integer.
PATH specifies the absolute path in URI (without query string).
QUERY-PARAMTERS is a string or an alist.
If QUERY-PARAMTERS is a string, it is treated as an encoded query string.
If QUERY-PARAMTERS is an alist, it represents a list of cons pairs of
string, (query-key . query-value).
POST-BODY specifies the post body sent when METHOD equals to \"POST\".
If POST-BODY is nil, no body is posted.
If USE-SSL is non-nil, the request is performed with SSL.

The result alist includes the following keys, where a key is a symbol.
  method: HTTP method such as \"GET\" or \"POST\".
  scheme: the scheme name. \"http\" or \"https\".
  host: the host to which the request is sent.
  port: the port to which the request is sent (integer).
  path: the absolute path string. Note that it does not include query string.
  query-string: the query string.
  encoded-query-alist: the alist consisting of pairs of encoded query-name and
    encoded query-value.
  uri: the URI. It includes the query string.
  uri-without-query: the URI without the query string.
  header-list: an alist specifying pairs of a parameter and its value in HTTP
    header field.
  post-body: the entity that will be posted."
  (let* ((scheme (if use-ssl "https" "http"))
	 (default-port (if use-ssl 443 80))
	 (port (if port port default-port))
	 (query-string
	  (cond
	   ((stringp query-parameters)
	    query-parameters)
	   ((consp query-parameters)
	    (mapconcat (lambda (pair)
			 (cond
			  ((stringp pair)
			   (twittering-percent-encode pair))
			  ((consp pair)
			   (format
			    "%s=%s"
			    (twittering-percent-encode (car pair))
			    (twittering-percent-encode (cdr pair))))
			  (t
			   nil)))
		       query-parameters
		       "&"))
	   (t
	    nil)))
	 (encoded-query-alist
	  (cond
	   ((stringp query-parameters)
	    ;; Query name and its value must be already encoded.
	    (mapcar (lambda (str)
		      (if (string-match "=" str)
			  (let ((key (substring str 0 (match-beginning 0)))
				(value (substring str (match-end 0))))
			    `(,key . ,value))
			`(,str . nil)))
		    (split-string query-parameters "&")))
	   ((consp query-parameters)
	    (mapcar (lambda (pair)
		      (cond
		       ((stringp pair)
			(cons (twittering-percent-encode pair) nil))
		       ((consp pair)
			(cons (twittering-percent-encode (car pair))
			      (twittering-percent-encode (cdr pair))))
		       (t
			nil)))
		    query-parameters))
	   (t
	    nil)))
	 (uri-without-query
	  (concat scheme "://"
		  host
		  (when (and port (not (= port default-port)))
		    (format ":%d" port))
		  path))
	 (uri
	  (if query-string
	      (concat uri-without-query "?" query-string)
	    uri-without-query))
	 (header-list
	  `(,@(when (and (string= method "POST")
			 (not (assoc "Content-Length" header-list)))
		`(("Content-Length" . ,(format "%d" (length post-body)))))
	    ,@(unless (assoc "Host" header-list)
		`(("Host" . ,host)))
	    ,@(unless (assoc "User-Agent" header-list)
		`(("User-Agent" . ,(twittering-user-agent))))
	    ,@header-list)))
    (cond
     ((not (member method '("POST" "GET")))
      (error "Unknown HTTP method: %s" method)
      nil)
     ((not (string-match "^/" path))
      (error "Invalid HTTP path: %s" path)
      nil)
     (t
      `((method . ,method)
	(scheme . ,scheme)
	(host . ,host)
	(port . ,port)
	(path . ,path)
	(query-string . ,query-string)
	(encoded-query-alist . ,encoded-query-alist)
	(uri . ,uri)
	(uri-without-query . ,uri-without-query)
	(header-list . ,header-list)
	(post-body . ,post-body))))))

(defun twittering-make-http-request-from-uri (method header-list uri &optional post-body)
  "Returns an alist specifying a HTTP request.
The result alist has the same form as an alist generated by
`twittering-make-http-request'.

METHOD specifies HTTP method. It must be \"GET\" or \"POST\".
HEADER-LIST is a list of (field-name . field-value) specifying HTTP header
fields. The fields \"Host\" and \"User-Agent\" are automatically filled
if necessary.
URI specifies the URI including query string.
POST-BODY specifies the post body sent when METHOD equals to \"POST\".
If POST-BODY is nil, no body is posted."
  (let* ((parts-alist
	  (let ((parsed-url (url-generic-parse-url uri)))
	    ;; This is required for the difference of url library
	    ;; distributed with Emacs 22 and 23.
	    (cond
	     ((and (fboundp 'url-p) (url-p parsed-url))
	      ;; Emacs 23 and later.
	      `((scheme . ,(url-type parsed-url))
		(host . ,(url-host parsed-url))
		(port . ,(url-portspec parsed-url))
		(path . ,(url-filename parsed-url))))
	     ((vectorp parsed-url)
	      ;; Emacs 22.
	      `((scheme . ,(aref parsed-url 0))
		(host . ,(aref parsed-url 3))
		(port . ,(aref parsed-url 4))
		(path . ,(aref parsed-url 5))))
	     (t
	      nil))))
	 (path (let ((path (cdr (assq 'path parts-alist))))
		 (if (string-match "\\`\\(.*\\)\\?" path)
		     (match-string 1 path)
		   path)))
	 (query-string (let ((path (cdr (assq 'path parts-alist))))
			 (if (string-match "\\?\\(.*\\)\\'" path)
			     (match-string 1 path)
			   nil))))
    (twittering-make-http-request method header-list
				  (cdr (assq 'host parts-alist))
				  (cdr (assq 'port parts-alist))
				  path
				  query-string
				  post-body
				  (string= "https"
					   (cdr (assq 'scheme parts-alist))))))

(defun twittering-make-connection-info (request &optional additional order table)
  "Make an alist specifying the information of connection for REQUEST.
REQUEST must be an alist that has the same keys as that generated by
`twittering-make-http-request'.

ADDITIONAL is appended to the tail of the result alist.
Following ADDITIONAL, an entry in TABLE is also appended to the result alist,
where `twittering-lookup-connection-type' determines the entry according to
the priority order ORDER.
If ORDER is nil, `twittering-connection-type-order' is used in place of ORDER.
If TABLE is nil, `twittering-connection-type-table' is used in place of TABLE.

The parameter symbols are following:
  use-ssl: whether SSL is enabled or not.
  allow-insecure-server-cert: non-nil if an insecure server certificate is
    allowed on SSL.
  cacert-file-fullpath: the full-path of a file including the certificates
    authorizing a server certificate on SSL. The file must be in PEM format.
  use-proxy: non-nil if using a proxy.
  proxy-server: a proxy server or nil.
  proxy-port: a port for connecting the proxy (integer) or nil.
  proxy-user: a username for connecting the proxy or nil.
  proxy-password: a password for connecting the proxy or nil.
  request: an alist specifying a HTTP request."
  (let* ((order (or order twittering-connection-type-order))
	 (table (or table twittering-connection-type-table))
	 (scheme (cdr (assq 'scheme request)))
	 (use-ssl (string= "https" scheme))
	 (entry (twittering-lookup-connection-type use-ssl order table)))
    `((use-ssl . ,use-ssl)
      (allow-insecure-server-cert
       . ,twittering-allow-insecure-server-cert)
      (cacert-file-fullpath
       . ,(when use-ssl (twittering-ensure-ca-cert)))
      (use-proxy . ,twittering-proxy-use)
      ,@(when twittering-proxy-use
	  `((proxy-server . ,(twittering-proxy-info scheme 'server))
	    (proxy-port . ,(twittering-proxy-info scheme 'port))
	    (proxy-user . ,(twittering-proxy-info scheme 'user))
	    (proxy-password . ,(twittering-proxy-info scheme 'password))))
      (request . ,request)
      ,@additional
      ,@entry)))

(defun twittering-get-response-header (buffer)
  "Extract HTTP response header from HTTP response.
BUFFER may be a buffer or the name of an existing buffer which contains the HTTP response."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp "\r?\n\r?\n" nil t)
	  (prog1
	      (buffer-substring (point-min) (match-end 0))
	    (when twittering-debug-mode
	      (debug-printf "connection-info=%s\n" connection-info)
	      (debug-print "HTTP response header:\n--BEGIN\n")
	      (debug-print (buffer-substring (point-min) (match-end 0)))
	      (debug-print "--END\n")))
	nil))))

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

(defun twittering-get-content-subtype-symbol-from-header-info (header-info)
  "Return a symbol corresponding to the subtype of content-type."
  (let* ((content-type
	  ;; According to RFC2616, field name of a HTTP header is
	  ;; case-insensitive.
	  (car
	   (remove
	    nil
	    (mapcar (lambda (entry)
		      (when (and (stringp (car entry))
				 (let ((case-fold-search t))
				   (string-match "\\`content-type\\'"
						 (car entry))))
			(cdr entry)))
		    header-info))))
	 (subtype (when (and (stringp content-type)
			     (string-match "\\` *[^/]*/\\([^ ;]*\\)"
					   content-type))
		    (downcase (match-string 1 content-type))))
	 (symbol-alist
	  '(("json" . json)
	    ("atom+xml" . atom)
	    ("xml" . xml))))
    (cdr (assoc subtype symbol-alist))))

(defun twittering-decode-response-body (header-info)
  "Decode the current buffer according to the content-type in HEADER-INFO."
  (let* ((content-type
	  ;; According to RFC2616, field name of a HTTP header is
	  ;; case-insensitive.
	  (car
	   (remove
	    nil
	    (mapcar (lambda (entry)
		      (when (and (stringp (car entry))
				 (let ((case-fold-search t))
				   (string-match "\\`content-type\\'"
						 (car entry))))
			(cdr entry)))
		    header-info))))
	 (parameters (when (stringp content-type)
		       (cdr (split-string content-type ";"))))
	 (regexp "^[[:space:]]*charset=utf-8[[:space:]]*$")
	 (encoded-with-utf-8
	  (let ((case-fold-search t))
	    (remove nil
		    (mapcar (lambda (entry)
			      (string-match regexp entry))
			    parameters)))))
    (when encoded-with-utf-8
      (decode-coding-region (point-min) (point-max) 'utf-8))))

(defun twittering-send-http-request-internal (request additional-info sentinel &optional order table)
  "Open a connection and return a subprocess object for the connection.
REQUEST must be an alist that has the same keys as that generated by
`twittering-make-http-request'.
SENTINEL is called as a function when the process changes state.
It gets three arguments: the process, a string describing the change, and
the connection-info, which is generated by `twittering-make-connection-info'
and also includes an alist ADDITIONAL-INFO.

How to perform the request is selected from TABLE according to the priority
order ORDER. ORDER and TABLE are directly sent to
`twittering-make-connection-info'.
If ORDER is nil, `twittering-connection-type-order' is used in place of ORDER.
If TABLE is nil, `twittering-connection-type-table' is used in place of TABLE.
"
  (let* ((order (or order twittering-connection-type-order))
	 (table (or table twittering-connection-type-table))
	 (connection-info
	  (twittering-make-connection-info request additional-info
					   order table))
	 (func (cdr (assq 'send-http-request connection-info)))
	 (temp-buffer (generate-new-buffer "*twmode-http-buffer*"))
	 ;; Bind `default-directory' to the temporary directory
	 ;; because it is possible that the directory pointed by
	 ;; `default-directory' has been already removed.
	 (default-directory temporary-file-directory))
    (cond
     ((and func (functionp func))
      (funcall func "*twmode-generic*" temp-buffer
	       connection-info
	       (when (and sentinel (functionp sentinel))
		 (lexical-let ((sentinel sentinel)
			       (connection-info connection-info))
		   (lambda (proc status)
		     (apply sentinel proc status connection-info nil))))))
     (t
      (error "No valid connection method is found")
      nil))))

(defun twittering-send-http-request (request additional-info func &optional clean-up-func)
  "Send a HTTP request and return a subprocess object for the connection.
REQUEST must be an alist that has the same keys as that generated by
`twittering-make-http-request'.

FUNC is called when a HTTP response has been received without errors.
It is called with the current buffer containing the HTTP response (without
HTTP headers). FUNC is called with four arguments: the process, a symbol
describing the status of the process, a connection-info generated by
`twittering-make-connection-info', and a header-info generated by
`twittering-get-response-header' and `twittering-make-header-info-alist'.
The connection-info also includes an alist ADDITIONAL-INFO.
If FUNC returns non-nil and `twittering-buffer-related-p' is non-nil, the
returned value is displayed as a message.
And also, if FUNC returns a string and it matches the regular expression
\"^\\\\(Failuare\\\\|Response\\\\): \", the returned value is displayed
as a message.

CLEAN-UP-FUNC is called whenever the sentinel of the subprocess for the
connection is called (as `set-process-sentinel').
It is called with three arguments: the process, a symbol describing the status
of the proess, and a connection-info generated by
`twittering-make-connection-info'.
They are the same as arguments for FUNC.
When a HTTP response has been received, FUNC is called in advance of
CLEAN-UP-FUNC. CLEAN-UP-FUNC can overwrite the message displayed by FUNC.

If the subprocess has exited, the buffer bound to it is automatically killed
after calling CLEAN-UP-FUNC.

The method to perform the request is determined from
`twittering-connection-type-table' according to the priority order
`twittering-connection-type-order'."
  (lexical-let ((func func)
		(clean-up-func clean-up-func))
    (twittering-send-http-request-internal
     request additional-info
     (lambda (proc status-str connection-info)
       (let ((status (cond
		      ((string= status-str "urllib-finished") 'exit)
		      ((processp proc) (process-status proc))
		      (t nil)))
	     (buffer (process-buffer proc))
	     (exit-status (cond
			   ((string= status-str "urllib-finished") 0)
			   ((processp proc) (process-exit-status proc))
			   (t 1)))
	     (command (process-command proc))
	     (pre-process-func
	      (cdr (assq 'pre-process-buffer connection-info)))
	     (mes nil))
	 (unwind-protect
	     (setq mes
		   (cond
		    ((null status)
		     (format "Failure: process %s does not exist" proc))
		    ((or (memq status '(run stop open listen connect))
			 (not (memq status '(exit signal closed failed))))
		     ;; If the process is running, FUNC is not called.
		     nil)
		    ((and command
			  (not (= 0 exit-status)))
		     ;; If the process abnormally exited,
		     (format "Failure: %s exited abnormally (exit-status=%s)"
			     (car command) exit-status))
		    ((not (buffer-live-p buffer))
		     (format "Failure: the buffer for %s is already killed"
			     proc))
		    (t
		     (when (functionp pre-process-func)
		       ;; Pre-process buffer.
		       (funcall pre-process-func proc buffer connection-info))
		     (let* ((header (twittering-get-response-header buffer))
			    (header-info
			     (and header
				  (twittering-make-header-info-alist header))))
		       (with-current-buffer buffer
			 (goto-char (point-min))
			 (when (search-forward-regexp "\r?\n\r?\n" nil t)
			   ;; delete HTTP headers.
			   (delete-region (point-min) (match-end 0)))
			 ;; It may be necessary to decode the contents of
			 ;; the buffer by UTF-8 because
			 ;; `twittering-http-application-headers' specifies
			 ;; utf-8 as one of acceptable charset.
			 ;; For the present, only UTF-8 is taken into account.
			 (twittering-decode-response-body header-info)
			 (apply func proc status connection-info
				header-info nil))))))
	   ;; unwind-forms
	   (setq mes
		 (cond
		  ((null mes)
		   nil)
		  ((string-match "^\\(Failure\\|Response\\): " mes)
		   (let* ((request (cdr (assq 'request connection-info)))
			  (method (cdr (assq 'method request)))
			  (uri (cdr (assq 'uri request))))
		     (concat mes " for " method " " uri)))
		  ((twittering-buffer-related-p)
		   mes)))
	   (when mes
	     ;; CLEAN-UP-FUNC can overwrite a message from the return value
	     ;; of FUNC.
	     (message "%s" mes))
	   (when (functionp clean-up-func)
	     (funcall clean-up-func proc status connection-info))
	   (when (and (memq status '(exit signal closed failed))
		      (buffer-live-p buffer)
		      (not twittering-debug-mode))
	     (kill-buffer buffer))))))))

;;;;
;;;; Basic HTTP functions with tls and Emacs builtins.
;;;;

(eval-when-compile (require 'tls nil t))
(defun twittering-start-http-session-native-tls-p ()
  (when (and (not twittering-proxy-use)
	     (require 'tls nil t))
    (unless twittering-tls-program
      (let ((programs
	     (remove nil
		     (mapcar (lambda (cmd)
			       (when (string-match "\\`\\([^ ]+\\) " cmd)
				 (when (executable-find (match-string 1 cmd))
				   cmd)))
			     tls-program))))
	(setq twittering-tls-program
	      (if twittering-allow-insecure-server-cert
		  (mapcar
		   (lambda (str)
		     (cond
		      ((string-match "^\\([^ ]*/\\)?openssl s_client " str)
		       (concat (match-string 0 str) "-verify 0 "
			       (substring str (match-end 0))))
		      ((string-match "^\\([^ ]*/\\)?gnutls-cli " str)
		       (concat (match-string 0 str) "--insecure "
			       (substring str (match-end 0))))
		      (t
		       str)))
		   programs)
		programs))))
    (not (null twittering-tls-program))))

;; TODO: proxy
(defun twittering-send-http-request-native (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (uri (cdr (assq 'uri connection-info)))
	 (method (cdr (assq 'method request)))
	 (scheme (cdr (assq 'scheme request)))
	 (host (cdr (assq 'host request)))
	 (port (cdr (assq 'port request)))
	 (path (cdr (assq 'path request)))
	 (query-string (cdr (assq 'query-string request)))
	 (post-body (cdr (assq 'post-body request)))
	 (use-proxy (cdr (assq 'use-proxy connection-info)))
	 (proxy-server (cdr (assq 'proxy-server connection-info)))
	 (proxy-port (cdr (assq 'proxy-port connection-info)))
	 (proxy-user (cdr (assq 'proxy-user connection-info)))
	 (proxy-password (cdr (assq 'proxy-password connection-info)))
	 (proxy-credentials
	  (when (and proxy-user proxy-password)
	    (concat "Basic "
		    (base64-encode-string
		     (concat proxy-user ":" proxy-password)))))
	 (header-list
	  (let ((original-header-list (cdr (assq 'header-list request))))
	    (if proxy-credentials
		(cons
		 `("Proxy-Authorization" ,proxy-credentials)
		 original-header-list)
	      original-header-list)))
	 (use-ssl (cdr (assq 'use-ssl connection-info)))
	 (allow-insecure-server-cert
	  (cdr (assq 'allow-insecure-server-cert connection-info)))
	 (connect-host (or proxy-server host))
	 (connect-port (or proxy-port port))
	 (request-str
	  (format "%s %s HTTP/1.1\r\n%s\r\n\r\n%s\r\n"
		  method
		  (if use-proxy
		      ;; As described in 5.1.2 of RFC2616, the
		      ;; absolute URI is required here if the connection
		      ;; uses a proxy.
		      uri
		    (concat path
			    (when query-string
			      (concat "?" query-string))))
		  (mapconcat (lambda (pair)
			       (format "%s: %s" (car pair) (cdr pair)))
			     header-list "\r\n")
		  (or post-body "")))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (tls-program twittering-tls-program)
	 (proc
	  (funcall (if use-ssl
		       'open-tls-stream
		     'open-network-stream)
		   "network-connection-process"
		   nil connect-host connect-port)))
    (when proc
      (set-process-buffer proc buffer)
      (when (functionp sentinel)
	(if (twittering-process-alive-p proc)
	    (set-process-sentinel proc sentinel)
	  (funcall sentinel proc "finished")))
      (process-send-string proc request-str)
      proc)))

(defun twittering-pre-process-buffer-native (proc buffer connection-info)
  (let ((use-ssl (cdr (assq 'use-ssl connection-info)))
	(args (process-command proc)))
    (cond
     ((and use-ssl args
	   (car
	    (remove nil
		    (mapcar (lambda (cmd)
			      (string-match "^\\(.*/\\)?gnutls-cli\\b" cmd))
			    args))))
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-max))
	  (when (search-backward-regexp
		 "- Peer has closed the GNUTLS connection\r?\n\\'")
	    (let ((beg (match-beginning 0))
		  (end (match-end 0)))
	      (delete-region beg end))))))
     ((and use-ssl args
	   (car
	    (remove nil
		    (mapcar
		     (lambda (cmd)
		       (string-match "^\\(.*/\\)?openssl s_client\\b" cmd))
		     args))))
      (with-current-buffer buffer
	(save-excursion
	  (goto-char (point-max))
	  (when (search-backward-regexp "closed\r?\n\\'")
	    (let ((beg (match-beginning 0))
		  (end (match-end 0)))
	      (delete-region beg end))))))
     (t
      nil))))

;;;;
;;;; Basic HTTP functions with curl
;;;;

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
	(let ((coding-system-for-read 'iso-safe)
	      (coding-system-for-write 'iso-safe)
	      ;; Bind `default-directory' to the temporary directory
	      ;; because it is possible that the directory pointed by
	      ;; `default-directory' has been already removed.
	      (default-directory temporary-file-directory))
	  (call-process twittering-curl-program
			nil (current-buffer) nil
			"--version")
	  (goto-char (point-min))
	  (setq twittering-curl-program-https-capability
		(if (search-forward-regexp "^Protocols: .*https" nil t)
		    'capable
		  'incapable)))))
    (eq twittering-curl-program-https-capability 'capable)))

(defun twittering-send-http-request-curl (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (method (cdr (assq 'method request)))
	 (uri (cdr (assq 'uri request)))
	 (header-list (cdr (assq 'header-list request)))
	 (post-body (cdr (assq 'post-body request)))
	 (use-proxy (cdr (assq 'use-proxy connection-info)))
	 (proxy-server (cdr (assq 'proxy-server connection-info)))
	 (proxy-port (cdr (assq 'proxy-port connection-info)))
	 (proxy-user (cdr (assq 'proxy-user connection-info)))
	 (proxy-password (cdr (assq 'proxy-password connection-info)))
	 (use-ssl (cdr (assq 'use-ssl connection-info)))
	 (allow-insecure-server-cert
	  (cdr (assq 'allow-insecure-server-cert connection-info)))
	 (cacert-file-fullpath
	  (cdr (assq 'cacert-file-fullpath connection-info)))
	 (cacert-file-base-directory
	  (when cacert-file-fullpath
	    (file-name-directory cacert-file-fullpath)))
	 (cacert-file-body
	  (when cacert-file-fullpath
	    (file-name-nondirectory cacert-file-fullpath)))
	 (header-list
	  `(,@header-list
	    ;; Make `curl' remove the HTTP header field "Expect" for
	    ;; avoiding '417 Expectation Failed' HTTP response error.
	    ;; The header field is automatically added for a HTTP request
	    ;; exceeding 1024 byte. See
	    ;; http://d.hatena.ne.jp/imait/20091228/1262004813 and
	    ;; http://www.escafrace.co.jp/blog/09/10/16/1008
	    ("Expect" . "")))
	 (curl-args
	  `("--include" "--silent" "--compressed"
	    ,@(apply 'append
		     (mapcar
		      (lambda (pair)
			;; Do not overwrite internal headers `curl' would use.
			;; Thanks to William Xu.
			;; "cURL - How To Use"
			;; http://curl.haxx.se/docs/manpage.html
			(unless (string= (car pair) "Host")
			  `("-H" ,(format "%s: %s" (car pair) (cdr pair)))))
		      header-list))
	    ,@(when use-ssl `("--cacert" ,cacert-file-body))
	    ,@(when (and use-ssl allow-insecure-server-cert)
		`("--insecure"))
	    ,@(when (and use-proxy proxy-server proxy-port)
		(append
		 `("-x" ,(format "%s:%s" proxy-server proxy-port))
		 (when (and proxy-user proxy-password)
		   `("-U" ,(format "%s:%s" proxy-user proxy-password)))))
	    ,@(when (string= "POST" method)
		`("-d" ,(or post-body "")))
	    ,uri))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (default-directory
	   ;; If `use-ssl' is non-nil, the `curl' process
	   ;; is executed at the same directory as the temporary cert file.
	   ;; Without changing directory, `curl' misses the cert file if
	   ;; you use Emacs on Cygwin because the path on Emacs differs
	   ;; from Windows.
	   ;; With changing directory, `curl' on Windows can find the cert
	   ;; file if you use Emacs on Cygwin.
	   (if use-ssl
	       cacert-file-base-directory
	     default-directory)))
    (twittering-start-process-with-sentinel name buffer
					    twittering-curl-program
					    curl-args sentinel)))

(defun twittering-pre-process-buffer-curl (proc buffer connection-info)
  (let ((use-ssl (cdr (assq 'use-ssl connection-info)))
	(use-proxy (cdr (assq 'use-proxy connection-info))))
    (when (and use-ssl use-proxy)
      ;; When using SSL via a proxy with CONNECT method,
      ;; omit a successful HTTP response and headers if they seem to be
      ;; sent from the proxy.
      (with-current-buffer buffer
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
		(delete-region beg end)))))))))

;;;;
;;;; Basic HTTP functions with wget
;;;;

(defun twittering-find-wget-program ()
  "Returns an appropriate `wget' program pathname or nil if not found."
  (executable-find "wget"))

(defun twittering-start-http-session-wget-p ()
  "Return t if `wget' was installed, otherwise nil."
  (unless twittering-wget-program
    (setq twittering-wget-program (twittering-find-wget-program)))
  (not (null twittering-wget-program)))

(defun twittering-send-http-request-wget (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (method (cdr (assq 'method request)))
	 (scheme (cdr (assq 'scheme request)))
	 (uri (cdr (assq 'uri request)))
	 (header-list (cdr (assq 'header-list request)))
	 (post-body (cdr (assq 'post-body request)))
	 (use-proxy (cdr (assq 'use-proxy connection-info)))
	 (proxy-server (cdr (assq 'proxy-server connection-info)))
	 (proxy-port (cdr (assq 'proxy-port connection-info)))
	 (proxy-user (cdr (assq 'proxy-user connection-info)))
	 (proxy-password (cdr (assq 'proxy-password connection-info)))
	 (use-ssl (cdr (assq 'use-ssl connection-info)))
	 (allow-insecure-server-cert
	  (cdr (assq 'allow-insecure-server-cert connection-info)))
	 (cacert-file-fullpath
	  (cdr (assq 'cacert-file-fullpath connection-info)))
	 (cacert-file-base-directory
	  (when cacert-file-fullpath
	    (file-name-directory cacert-file-fullpath)))
	 (cacert-file-body
	  (when cacert-file-fullpath
	    (file-name-nondirectory cacert-file-fullpath)))
	 (args
	  `("--save-headers"
	    "--quiet"
	    "--output-document=-"
	    ,@(remove nil
		      (mapcar
		       (lambda (pair)
			 (unless (string= (car pair) "Host")
			   (format "--header=%s: %s" (car pair) (cdr pair))))
		       header-list))
	    ,@(when use-ssl
		`(,(format "--ca-certificate=%s" cacert-file-body)))
	    ,@(when (and use-ssl allow-insecure-server-cert)
		`("--no-check-certificate"))
	    ,@(cond
	       ((not use-proxy)
		'("--no-proxy"))
	       ((and use-proxy proxy-server proxy-port
		     proxy-user proxy-password)
		`(,(format "--proxy-user=%s" proxy-user)
		  ,(format "--proxy-password=%s" proxy-password)))
	       (t
		nil))
	    ,@(when (string= "POST" method)
		`(,(concat "--post-data=" (or post-body ""))))
	    ,uri))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (default-directory
	   ;; If `use-ssl' is non-nil, the `wget' process
	   ;; is executed at the same directory as the temporary cert file.
	   ;; Without changing directory, `wget' misses the cert file if
	   ;; you use Emacs on Cygwin because the path on Emacs differs
	   ;; from Windows.
	   ;; With changing directory, `wget' on Windows can find the cert
	   ;; file if you use Emacs on Cygwin.
	   (if use-ssl
	       cacert-file-base-directory
	     default-directory))
	 (process-environment
	  `(,@(when (and use-proxy proxy-server proxy-port)
		`(,(format "%s_proxy=%s://%s:%s/" scheme
			   scheme proxy-server proxy-port)))
	    ,@process-environment)))
    (twittering-start-process-with-sentinel name buffer
					    twittering-wget-program args
					    sentinel)))

(defun twittering-pre-process-buffer-wget (proc buffer connection-info)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "\\`[^\n]*?\r\r\n" (point-max) t)
	;; When `wget.exe' writes HTTP response in text mode,
	;; CRLF may be converted into CRCRLF.
	(goto-char (point-min))
	(while (search-forward "\r\n" nil t)
	  (replace-match "\n" nil t)))
      (goto-char (point-max))
      (when (search-backward-regexp "\nProcess [^\n]* finished\n\\'"
				    (point-min) t)
	(replace-match "" nil t))
      )))

;;;;
;;;; Basic HTTP functions with url library
;;;;

(defun twittering-start-http-session-urllib-p ()
  "Return t if url library is available, otherwise nil."
  (require 'url nil t))

(defun twittering-start-http-session-urllib-https-p ()
  "Return t if url library can be used for HTTPS, otherwise nil."
  (and (not twittering-proxy-use)
       (require 'url nil t)
       (cond
	((<= 22 emacs-major-version)
	 ;; On Emacs22 and later, `url' requires `tls'.
	 (twittering-start-http-session-native-tls-p))
	((require 'ssl nil t)
	 ;; On Emacs21, `url' requires `ssl'.
	 t)
	((or (and (fboundp 'open-ssl-stream)
		  ;; Since `url-gw' (required by `url') defines autoload of
		  ;; `open-ssl-stream' from "ssl",
		  ;; (fboundp 'open-ssl-stream) will be non-nil even if
		  ;; "ssl" cannot be loaded and `open-ssl-stream' is
		  ;; unavailable.
		  ;; Here, the availability is confirmed by `documentation'.
		  (documentation 'open-ssl-stream))
	     ;; On Emacs21, `url' requires `ssl' in order to use
	     ;; `open-ssl-stream', which is included in `ssl.el'.
	     ;; Even if `ssl' cannot be loaded, `open-tls-stream' can be
	     ;; used as an alternative of the function.
	     (and (twittering-start-http-session-native-tls-p)
		  (defalias 'open-ssl-stream 'open-tls-stream)))
	 (provide 'ssl)
	 t)
	(t
	 nil))))

(defun twittering-send-http-request-urllib (name buffer connection-info sentinel)
  (let* ((request (cdr (assq 'request connection-info)))
	 (method (cdr (assq 'method request)))
	 (scheme (cdr (assq 'scheme request)))
	 (uri (cdr (assq 'uri request)))
	 (header-list (cdr (assq 'header-list request)))
	 (post-body (cdr (assq 'post-body request)))
	 (use-proxy (cdr (assq 'use-proxy connection-info)))
	 (proxy-server (cdr (assq 'proxy-server connection-info)))
	 (proxy-port (cdr (assq 'proxy-port connection-info)))
	 (proxy-user (cdr (assq 'proxy-user connection-info)))
	 (proxy-password (cdr (assq 'proxy-password connection-info)))
	 (proxy-credentials
	  (when (and proxy-user proxy-password)
	    (concat "Basic "
		    (base64-encode-string
		     (concat proxy-user ":" proxy-password)))))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (url-proxy-services
	  (when use-proxy
	    `((,scheme . ,(format "%s:%s" proxy-server proxy-port)))))
	 (url-request-method method)
	 (url-request-extra-headers
	  ;; Remove some headers that should be configured by url library.
	  ;; They may break redirections by url library because
	  ;; `url-request-extra-headers' overwrites the new headers
	  ;; that are adapted to redirected connection.
	  (apply 'append
		 (mapcar (lambda (pair)
			   (if (member (car pair)
				       '("Host" "Content-Length"))
			       nil
			     `(,pair)))
			 (if proxy-credentials
			     (cons
			      `("Proxy-Authorization" ,proxy-credentials)
			      header-list)
			   header-list))))
	 (url-request-data post-body)
	 (url-show-status twittering-url-show-status)
	 (url-http-attempt-keepalives nil)
	 (tls-program twittering-tls-program)
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary))
    (lexical-let ((sentinel sentinel)
		  (buffer buffer))
      (let ((result-buffer
	     (url-retrieve
	      uri
	      (lambda (&rest args)
		(let ((proc url-http-process)
		      (url-buffer (current-buffer))
		      (status-str
		       (if (and (< emacs-major-version 22)
				(boundp 'url-http-end-of-headers)
				url-http-end-of-headers)
			   "urllib-finished"
			 "finished")))
		  ;; Callback may be called multiple times.
		  ;; (as filter and sentinel?)
		  (unless (local-variable-if-set-p 'twittering-retrieved)
		    (set (make-local-variable 'twittering-retrieved)
			 'not-completed)
		    (with-current-buffer buffer
		      (set-buffer-multibyte nil)
		      (insert-buffer-substring url-buffer))
		    (set-process-buffer proc buffer)
		    (unwind-protect
			(apply sentinel proc status-str nil)
		      (set-process-buffer proc url-buffer)
		      (if (eq twittering-retrieved 'exited)
			  (url-mark-buffer-as-dead url-buffer)
			(setq twittering-retrieved 'completed))))
		  (when (memq (process-status proc)
			      '(nil closed exit failed signal))
		    ;; Mark `url-buffer' as dead when the process exited
		    ;; and `sentinel' is completed.
		    ;; If this `lambda' is evaluated via a filter, the
		    ;; process may exit before it is finished to evaluate
		    ;; `(apply sentinel ...)'. In the case, `buffer' should
		    ;; not be killed. It should be killed after the
		    ;; evaluation of `sentinel'.
		    (if (eq twittering-retrieved 'completed)
			(url-mark-buffer-as-dead url-buffer)
		      (setq twittering-retrieved 'exited))))))))
	(when (buffer-live-p result-buffer)
	  (with-current-buffer result-buffer
	    (set (make-local-variable 'url-show-status)
		 twittering-url-show-status)
	    ;; Make `url-http-attempt-keepalives' buffer-local
	    ;; in order to send the current value of the variable
	    ;; to the sentinel invoked for HTTP redirection,
	    (make-local-variable 'url-http-attempt-keepalives))
	  (get-buffer-process result-buffer))))))

(defun twittering-pre-process-buffer-urllib (proc buffer connection-info)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (cond
       ((search-backward-regexp
	 "- Peer has closed the GNUTLS connection\r?\n\\'"
	 nil t)
	(let ((beg (match-beginning 0))
	      (end (match-end 0)))
	  (delete-region beg end)))
       ((search-backward-regexp "closed\r?\n\\'" nil t)
	(let ((beg (match-beginning 0))
	      (end (match-end 0)))
	  (delete-region beg end)))
       (t nil)))))

;;;;
;;;; HTTP functions for twitter-like serivce
;;;;

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
      (push (cons "Content-Type" "text/plain") headers))
    headers
    ))

(defun twittering-add-application-header-to-http-request (request account-info)
  "Make a new HTTP request based on REQUEST with the authorization header.
The authorization header is generated from ACCOUNT-INFO.
ACCOUNT-INFO must be an alist that includes the following keys;
  \"screen_name\" and \"password\" if `twittering-auth-method' is 'basic,
  \"screen_name\", \"oauth_token\" and \"oauth_token_secret\" if
  `twittering-auth-method' is 'oauth or 'xauth."
  (let* ((method (cdr (assq 'method request)))
	 (auth-str
	  (cond
	   ((eq twittering-auth-method 'basic)
	    (twittering-make-basic-authentication-string account-info))
	   ((memq twittering-auth-method '(oauth xauth))
	    (twittering-make-oauth-authentication-string account-info request))
	   (t
	    nil)))
	 (cookie-str (twittering-make-cookie-string request account-info))
	 (application-headers
	  `(,@(twittering-http-application-headers method)
	    ("Authorization" . ,auth-str)
	    ,@(when cookie-str
		`(("Cookie" . ,cookie-str))))))
    (mapcar (lambda (entry)
	      (if (eq (car entry) 'header-list)
		  `(header-list
		    . ,(append (cdr entry) application-headers))
		entry))
	    request)))

(defun twittering-get-error-message (header-info connection-info &optional buffer)
  "Return an error message generated from the arguments.
HEADER-INFO must be an alist generated by `twittering-get-response-header'.
CONNECTION-INFO must be an alist generated by
`twittering-make-connection-info'. It may include some additional information
which is added by `twittering-send-http-request'.
BUFFER must be nil or a HTTP response body, which includes error messages from
the server when the HTTP status code equals to 400 or 403.
If BUFFER is nil, the current buffer is used instead."
  (let ((buffer (or buffer (current-buffer)))
	(status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twittering-get-content-subtype-symbol-from-header-info header-info)))
    (cond
     ((and (buffer-live-p buffer)
	   (member status-code '("400" "401" "403" "404")))
      ;; Twitter returns an error message as a HTTP response body if
      ;; HTTP status is "400 Bad Request" or "403 Forbidden".
      ;; See "HTTP Response Codes and Errors | dev.twitter.com"
      ;; http://dev.twitter.com/pages/responses_errors .
      ;;
      ;; However, Twitter seems to return an error message even when
      ;; the HTTP status is "401 Unauthorized" or "404 Not Found".
      (let* ((error-mes
	      (cond
	       ((eq format 'xml)
		(let ((xmltree
		       (with-current-buffer buffer
			 (twittering-xml-parse-region (point-min)
						      (point-max)))))
		  (car (cddr (assq 'error (or (assq 'errors xmltree)
					      (assq 'hash xmltree)))))))
	       ((eq format 'json)
		(let ((json-object (with-current-buffer buffer
				     (twittering-json-read))))
		  (cdr (assq 'error json-object))))
	       (t
		;; ATOM is not supported.
		nil))))
	(if error-mes
	    (format "%s (%s)" status-line error-mes)
	  status-line)))
     (t
      status-line))))

(defun twittering-http-get (account-info-alist host method &optional parameters format additional-info sentinel clean-up-sentinel)
  "Send a HTTP GET request with application headers.
ACCOUNT-INFO-ALIST is an alist used by
`twittering-add-application-header-to-http-request'.
The alist made by `((account-info . ,ACCOUNT-INFO-ALIST) ,@ADDITIONAL-INFO)'
is used as the argument `additional-info' of `twittering-send-http-request'.
HOST is hostname of remote side, api.twitter.com (or search.twitter.com).
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6
FORMAT is a response data format (\"xml\", \"atom\", \"json\")"
  (let* ((format (or format "xml"))
	 (sentinel
	  (lexical-let ((sentinel (or sentinel
				      'twittering-http-get-default-sentinel)))
	    (lambda (proc status connection-info header-info)
	      (twittering-update-server-info connection-info header-info)
	      (apply sentinel proc status connection-info header-info nil))))
	 (path (concat "/" method "." format))
	 (headers nil)
	 (port nil)
	 (post-body "")
	 (request
	  (twittering-add-application-header-to-http-request
	   (twittering-make-http-request "GET" headers host port path
					 parameters post-body
					 twittering-use-ssl)
	   account-info-alist))
	 (additional-info
	  `((account-info . ,account-info-alist)
	    ,@additional-info)))
    (twittering-send-http-request request additional-info
				  sentinel clean-up-sentinel)))

(defun twittering-http-get-default-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twittering-get-content-subtype-symbol-from-header-info header-info)))
    (case-string
     status-code
     (("200")
      (debug-printf "connection-info=%s" connection-info)
      (let* ((spec (cdr (assq 'timeline-spec connection-info)))
	     (spec-string (cdr (assq 'timeline-spec-string connection-info)))
	     (service-method (cdr (assq 'service-method connection-info)))
	     (statuses
	      (cond
	       ((eq format 'json)
		(let ((json-array (twittering-json-read)))
		  (cond
		   ((null json-array)
		    nil)
		   ((eq (car spec) 'search)
		    (cond
		     ((memq service-method '(twitter statusnet))
		      (mapcar 'twittering-json-object-to-a-status-on-search
			      (cdr (assq 'results json-array))))
		     ((eq service-method 'twitter-api-v1.1)
		      (mapcar 'twittering-json-object-to-a-status
			      (cdr (assq 'statuses json-array))))))
		   ((twittering-timeline-spec-is-direct-messages-p spec)
		    (mapcar
		     'twittering-json-object-to-a-status-on-direct-messages
		     json-array))
		   (t
		    (mapcar 'twittering-json-object-to-a-status
			    json-array)))))
	       ((eq format 'xml)
		(let ((xmltree
		       (twittering-xml-parse-region (point-min) (point-max))))
		  (when xmltree
		    (twittering-xmltree-to-status xmltree))))
	       ((eq format 'atom)
		(let ((xmltree
		       (twittering-xml-parse-region (point-min) (point-max))))
		  (when xmltree
		    (twittering-atom-xmltree-to-status xmltree))))
	       (t
		nil)))
	     (rendered-tweets nil))
	(let ((updated-timeline-info
	       (twittering-add-statuses-to-timeline-data statuses spec))
	      (buffer (twittering-get-buffer-from-spec spec)))
	  ;; FIXME: We should retrieve un-retrieved statuses until
	  ;; statuses is nil. twitter server returns nil as
	  ;; xmltree with HTTP status-code is "200" when we
	  ;; retrieved all un-retrieved statuses.
	  (if twittering-notify-successful-http-get
	      (if updated-timeline-info
		  (concat
		   (format "Fetching %s. Success. " spec-string)
		   (mapconcat
		    (lambda (info)
		      (let ((spec-string (nth 0 info))
			    (num (nth 1 info)))
			(format "%s: +%d" spec-string num)))
		    updated-timeline-info
		    ", "))
		(format "Fetching %s. Success. (No new tweets)"
			spec-string))
	    nil))))
     (("404")
      ;; The requested resource does not exist.
      (let ((spec (cdr (assq 'timeline-spec connection-info)))
	    (spec-string (cdr (assq 'timeline-spec-string connection-info))))
	;; Remove specs related to the invalid spec from history.
	(mapc
	 (lambda (buffer)
	   (let ((other-spec (twittering-get-timeline-spec-for-buffer buffer))
		 (other-spec-string
		  (twittering-get-timeline-spec-string-for-buffer buffer)))
	     (when (twittering-timeline-spec-depending-on-p other-spec spec)
	       (twittering-remove-timeline-spec-string-from-history
		other-spec-string))))
	 (twittering-get-buffer-list)))
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info)))
     (t
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info))))))

(defun twittering-retrieve-single-tweet-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twittering-get-content-subtype-symbol-from-header-info header-info)))
    (case-string
     status-code
     (("200" "403" "404")
      (debug-printf "connection-info=%s" connection-info)
      (let* ((id (cdr (assq 'id connection-info)))
	     (user-screen-name (cdr (assq 'user-screen-name connection-info)))
	     (status
	      (cond
	       ((string= status-code "403")
		;; Forbidden. Maybe a protected tweet?
		(twittering-make-alist-of-forbidden-tweet id
							  user-screen-name))
	       ((string= status-code "404")
		;; The requested resource does not exist.
		(twittering-make-alist-of-non-existent-tweet id
							     user-screen-name))
	       ((eq format 'json)
		(let ((json-object (twittering-json-read)))
		  (twittering-json-object-to-a-status json-object)))
	       ((eq format 'xml)
		(let ((xmltree
		       (twittering-xml-parse-region (point-min) (point-max))))
		  (when xmltree
		    (car
		     (twittering-xmltree-to-status
		      `((statuses nil ,@xmltree)))))))
	       (t
		nil))))
	(when status
	  (twittering-add-statuses-to-timeline-data `(,status) '(:single))
	  (let ((buffer (cdr (assq 'buffer connection-info)))
		(spec (cdr (assq 'timeline-spec connection-info)))
		(prop
		 (cdr (assq 'property-to-be-redisplayed connection-info))))
	    (cond
	     ((null prop)
	      ;; The process has been invoked via `twittering-call-api' with
	      ;; the command `retrieve-timeline', not the command
	      ;; `retrieve-single-tweet' for rendering a replied tweet.
	      ;; No special property that specifies regions being re-rendered
	      ;; is given.
	      (let ((new-statuses `(,status))
		    (buffer (twittering-get-buffer-from-spec spec)))
		(when (and new-statuses buffer)
		  (twittering-render-timeline buffer new-statuses t))))
	     ((and buffer prop (buffer-live-p buffer))
	      (twittering-redisplay-status-on-each-buffer buffer prop)
	      (with-current-buffer buffer
		(save-excursion
		  (let ((buffer-read-only nil))
		    (lexical-let ((prop prop))
		      (twittering-for-each-property-region
		       prop
		       (lambda (beg end value)
			 ;; Remove the property required no longer.
			 (remove-text-properties beg end `(,prop nil))
			 (goto-char beg)
			 (twittering-render-replied-statuses)))))))))))
	(cond
	 ((string= status-code "403")
	  (format "You are not authorized to see this tweet (ID %s)." id))
	 ((string= status-code "404")
	  (format "The tweet with ID %s does not exist." id))
	 (twittering-notify-successful-http-get
	  (format "Fetching %s.  Success." id))
	 (t
	  nil))))
     (t
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info))))))

(defmacro twittering-http-get-list-sentinel-base (what)
  `(let ((status-line (cdr (assq 'status-line header-info)))
	 (status-code (cdr (assq 'status-code header-info)))
	 (format
	  (twittering-get-content-subtype-symbol-from-header-info header-info))
	 (indexes nil)
	 (mes nil))
     (case-string
      status-code
      (("200")
       (cond
	((eq format 'xml)
	 (let ((xmltree (twittering-xml-parse-region (point-min) (point-max))))
	   (when xmltree
	     (setq indexes
		   (mapcar
		    (lambda (c-node)
		      (caddr (assq ,what c-node)))
		    (remove nil
			    (mapcar
			     (lambda (node)
			       (and (consp node) (eq 'list (car node))
				    node))
			     (cdr-safe
			      (assq 'lists (assq 'lists_list xmltree))))
			    ))
		   ))))
	((eq format 'json)
	 (let* ((json-object (twittering-json-read))
		(json-list
		 (cond
		  ((arrayp json-object)
		   ;; GET lists/list in the Twitter REST API v1.1 returns
		   ;; an array.
		   json-object)
		  (t
		   ;; GET lists/subscriptions in the Twitter REST API v1.1
		   ;; returns an alist.
		   (cdr (assq 'lists json-object))))))
	   (when json-object
	     (setq indexes
		   (mapcar (lambda (entry)
			     (cdr (assq ,what entry)))
			   json-list)))))
	(t
	 (error "Format \"%s\" is not supported" format)
	 nil)))
      (t
       (setq mes (format "Response: %s"
			 (twittering-get-error-message header-info
						       connection-info)))))
     (setq twittering-list-index-retrieved
	   (or indexes
	       mes
	       "")) ;; set "" explicitly if user does not have a list.
     mes))

(defun twittering-http-get-list-index-sentinel (proc status connection-info header-info)
  (twittering-http-get-list-sentinel-base 'slug))

(defun twittering-http-get-list-subscriptions-sentinel (proc status connection-info header-info)
  (let ((result (twittering-http-get-list-sentinel-base 'full_name)))
    (when (listp twittering-list-index-retrieved)
      (setq twittering-list-index-retrieved
	    (mapcar (lambda (str)
		      (and (string-match "\\`@\\(.*\\)\\'" str)
			   (match-string 1 str)))
		    twittering-list-index-retrieved)))
    result))

(defun twittering-http-post (account-info-alist host method &optional parameters format additional-info sentinel clean-up-sentinel)
  "Send HTTP POST request to api.twitter.com (or search.twitter.com)
ACCOUNT-INFO-ALIST is an alist used by
`twittering-add-application-header-to-http-request'.
The alist made by `((account-info . ,ACCOUNT-INFO-ALIST) ,@ADDITIONAL-INFO)'
is used as the argument `additional-info' of `twittering-send-http-request'.
HOST is hostname of remote side, api.twitter.com (or search.twitter.com).
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6
FORMAT is a response data format (\"xml\", \"atom\", \"json\")"
  (let* ((format (or format "xml"))
	 (sentinel
	  (lexical-let ((sentinel (or sentinel
				      'twittering-http-post-default-sentinel)))
	    (lambda (proc status connection-info header-info)
	      (twittering-update-server-info connection-info header-info)
	      (apply sentinel proc status connection-info header-info nil))))
	 (path (concat "/" method "." format))
	 (headers nil)
	 (port nil)
	 (post-body "")
	 (request
	  (twittering-add-application-header-to-http-request
	   (twittering-make-http-request "POST" headers host port path
					 parameters post-body
					 twittering-use-ssl)
	   account-info-alist))
	 (additional-info `((account-info . ,account-info-alist)
			    ,@additional-info)))
    (twittering-send-http-request request additional-info
				  sentinel clean-up-sentinel)))

(defun twittering-http-post-default-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info))))
    (case-string
     status-code
     (("200")
      "Success: Post.")
     (t
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info))))))

(defun twittering-http-post-destroy-status-sentinel (proc status connection-info header-info)
  "A sentinel for deleting a status invoked via `twittering-call-api'."
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twittering-get-content-subtype-symbol-from-header-info header-info)))
    (case-string
     status-code
     (("200")
      (let* ((params
	      (cond
	       ((eq format 'xml)
		(let ((xml
		       (twittering-xml-parse-region (point-min) (point-max))))
		  `((id . ,(elt (assq 'id (assq 'status xml)) 2))
		    (text . ,(elt (assq 'text (assq 'status xml)) 2)))))
	       ((eq format 'json)
		(let ((json-object (twittering-json-read)))
		  `((id . ,(cdr (assq 'id_str json-object)))
		    (text . ,(cdr (assq 'text json-object))))))
	       (t
		(error "Format \"%s\" is not supported" format)
		nil)))
	     (id (cdr (assq 'id params)))
	     (text (cdr (assq 'text params))))
	(cond
	 (id
	  (twittering-delete-status-from-data-table id)
	  (format "Deleting \"%s\". Success." text))
	 (t
	  "Failure: the response for deletion could not be parsed."))))
     (t
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info))))))

;;;;
;;;; OAuth
;;;;

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

(defun twittering-sha1 (&rest args)
  "Return the SHA1 (Secure Hash Algorithm) of an object.

This is equivalent to the function `sha1' except that
`coding-system-for-read' and `coding-system-for-write' are bound to the
symbol `binary'.

The function `sha1' uses an external program for large object. However,
the coding system for transferring data from/to the program is not fixed,
at least in the implementation distributed with GNU Emacs 21.4.1, 22.2.1
and 23.2.1.
Therefore, the result from the function `sha1' may depend on the current
coding system.

This function avoid the dependency by binding `coding-system-for-read' and
`coding-system-for-write' to the symbol `binary'."
  (require 'sha1)
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	;; Bind `default-directory' to the temporary directory
	;; because it is possible that the directory pointed by
	;; `default-directory' has been already removed.
	(default-directory temporary-file-directory))
    (apply 'sha1 args)))

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
      (setq key (twittering-sha1 key nil nil t)))

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

	(twittering-sha1 (concat opad
				 (twittering-sha1 (concat ipad message)
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

;; "Using xAuth | dev.twitter.com"
;; http://dev.twitter.com/pages/xauth
(defun twittering-xauth-auth-str-access-token (url query-parameters consumer-key consumer-secret username password &optional oauth-parameters)
  (let ((key (concat consumer-secret "&"))
	(oauth-params
	 (or oauth-parameters
	     `(("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
	       ("oauth_signature_method" . "HMAC-SHA1")
	       ("oauth_timestamp" . ,(format-time-string "%s"))
	       ("oauth_consumer_key" . ,consumer-key)
	       ("oauth_version" . "1.0"))))
	(query-params
	 (append query-parameters
		 `(("x_auth_mode" . "client_auth")
		   ("x_auth_password"
		    . ,(twittering-oauth-url-encode password))
		   ("x_auth_username"
		    . ,(twittering-oauth-url-encode username))))))
    (twittering-oauth-auth-str "POST" url query-params oauth-params key)))

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
		(end (point-max)))
	    (twittering-oauth-make-response-alist (buffer-substring beg end))))
	 (t
	  (message "Response: %s" status-line)
	  nil))))))

(defun twittering-oauth-get-token-alist-url (url auth-str post-body)
  (let* ((url-request-method "POST")
	 (url-request-extra-headers
	  `(("Authorization" . ,auth-str)
	    ("Accept-Charset" . "us-ascii")
	    ("Content-Type" . "application/x-www-form-urlencoded")
	    ("Content-Length" . ,(format "%d" (length post-body)))))
	 (url-request-data post-body)
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

(defun twittering-oauth-get-token-alist (url auth-str &optional post-body)
  (let ((request
	 (twittering-make-http-request-from-uri
	  "POST"
	  `(("Authorization" . ,auth-str)
	    ("Accept-Charset" . "us-ascii")
	    ("Content-Type" . "application/x-www-form-urlencoded"))
	  url post-body)))
    (lexical-let ((result 'queried))
      (let ((proc
	     (twittering-send-http-request
	      request nil
	      (lambda (proc status connection-info header-info)
		(let ((status-line (cdr (assq 'status-line header-info)))
		      (status-code (cdr (assq 'status-code header-info))))
		  (case-string
		   status-code
		   (("200")
		    (when twittering-debug-mode
		      (let ((buffer (current-buffer)))
			(with-current-buffer (twittering-debug-buffer)
			  (insert-buffer-substring buffer))))
		    (setq result
			  (twittering-oauth-make-response-alist
			   (buffer-string)))
		    nil)
		   (t
		    (setq result nil)
		    (format "Response: %s" status-line)))))
	      (lambda (proc status connection-info)
		(when (and (not (twittering-process-alive-p proc))
			   (eq result 'queried))
		  (setq result nil))))))
	(twittering-wait-while nil 0.1
			       (and (eq result 'queried)
				    (twittering-process-alive-p proc)))
	(when (and (eq result 'queried)
		   (not (twittering-process-alive-p proc)))
	  ;; If the process has been dead, wait a moment because
	  ;; Emacs may be in the middle of evaluating the sentinel.
	  (twittering-wait-while 10 0.1
				 (eq result 'queried)
				 nil
				 (setq result nil)))
	result))))

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
    (cond
     (request-token-alist
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
	     request-token request-token-secret verifier)))))
     (t
      (error "Failed to retrieve a request token")
      nil))))

(defun twittering-xauth-get-access-token (access-token-url consumer-key consumer-secret username password)
  (let ((auth-str
	 (twittering-xauth-auth-str-access-token
	  access-token-url nil consumer-key consumer-secret
	  username password))
	(post-body
	 (mapconcat (lambda (pair)
		      (format "%s=%s" (car pair)
			      (twittering-oauth-url-encode (cdr pair))))
		    `(("x_auth_mode" . "client_auth")
		      ("x_auth_password" . ,password)
		      ("x_auth_username" . ,username))
		    "&")))
    (twittering-oauth-get-token-alist access-token-url auth-str post-body)))

;;;;
;;;; Private storage
;;;;

(defun twittering-private-info-loaded-p ()
  twittering-private-info-file-loaded)

(defun twittering-load-private-info ()
  (let* ((file twittering-private-info-file)
	 (decrypted-str (twittering-read-from-encrypted-file file))
	 (loaded-alist
	  (when decrypted-str
	    (condition-case nil
		(read decrypted-str)
	      (error
	       nil)))))
    (when loaded-alist
      (remove
       nil
       (mapcar
	(lambda (pair)
	  (when (consp pair)
	    (let ((sym (car pair))
		  (value (cdr pair)))
	      (cond
	       ((memq sym twittering-variables-stored-with-encryption)
		(set sym value)
		sym)
	       (t
		nil)))))
	loaded-alist)))))

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
	 (str (with-output-to-string (pp obj)))
	 (file twittering-private-info-file))
    (when (twittering-write-and-encrypt file str)
      (set-file-modes file #o600)
      (setq twittering-private-info-file-loaded t))))

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

(eval-when-compile
  (require 'epa nil t)
  (require 'alpaca nil t))
(defun twittering-read-from-encrypted-file (file)
  "Decrypt contents from FILE and return them.
Read encrypted contents from FILE and return the decrypted contents.
This function requires `epa' or `alpaca' library."
  (cond
   ((not (file-readable-p file))
    (error "Failed to read %s" file)
    nil)
   ((require 'epa nil t)
    (let ((context (epg-make-context epa-protocol))
	  ;; Bind `default-directory' to the temporary directory
	  ;; because it is possible that the directory pointed by
	  ;; `default-directory' has been already removed.
	  (default-directory temporary-file-directory))
      (epg-context-set-passphrase-callback
       context #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback
       context
       (cons #'epa-progress-callback-function
	     (format "Decrypting %s..." (file-name-nondirectory file))))
      (message "Decrypting %s..." (file-name-nondirectory file))
      (condition-case err
	  (let ((full-path (expand-file-name file)))
	    ;; `epg-decrypt-file' included in EasyPG 1.0.0, which is
	    ;; distributed with Emacs 23.2, requires the expanded full path
	    ;; as the argument CIPHER. This is because CIPHER is directly
	    ;; used as an argument of the command `gpg'.
	    (epg-decrypt-file context full-path nil))
	(error
	 (message "%s" (cdr err))
	 nil))))
   ((require 'alpaca nil t)
    (with-temp-buffer
      (let ((buffer-file-name (expand-file-name file))
	    (alpaca-regex-suffix ".*")
	    (coding-system-for-read 'binary)
	    (coding-system-for-write 'binary)
	    (temp-buffer (current-buffer))
	    ;; Bind `default-directory' to the temporary directory
	    ;; because it is possible that the directory pointed by
	    ;; `default-directory' has been already removed.
	    (default-directory temporary-file-directory))
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
    (let ((context (epg-make-context epa-protocol))
	  ;; Bind `default-directory' to the temporary directory
	  ;; because it is possible that the directory pointed by
	  ;; `default-directory' has been already removed.
	  (default-directory temporary-file-directory))
      (epg-context-set-passphrase-callback
       context #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback
       context (cons #'epa-progress-callback-function "Encrypting..."))
      (message "Encrypting...")
      (condition-case err
	  (unwind-protect
	      ;; In order to prevent `epa-file' to encrypt the file double,
	      ;; `epa-file-name-regexp' is temorarily changed into the null
	      ;; regexp that never matches any string.
	      (let ((epa-file-name-regexp "\\`\\'")
		    (coding-system-for-read 'binary)
		    (coding-system-for-write 'binary))
		(when (fboundp 'epa-file-name-regexp-update)
		  (epa-file-name-regexp-update))
		(with-temp-file file
		  (set-buffer-multibyte nil)
		  (delete-region (point-min) (point-max))
		  (insert (epg-encrypt-string context str nil))
		  (message "Encrypting...wrote %s" file)
		  t))
	    (when (fboundp 'epa-file-name-regexp-update)
	      (epa-file-name-regexp-update)))
	(error
	 (message "%s" (cdr err))
	 nil))))
   ((require 'alpaca nil t)
    ;; Create the file.
    ;; This is required because `alpaca-save-buffer' checks its timestamp.
    (with-temp-file file)
    (with-temp-buffer
      (let ((buffer-file-name file)
	    (coding-system-for-read 'binary)
	    (coding-system-for-write 'binary)
	    ;; Bind `default-directory' to the temporary directory
	    ;; because it is possible that the directory pointed by
	    ;; `default-directory' has been already removed.
	    (default-directory temporary-file-directory))
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

(defun twittering-ensure-private-info ()
  "Ensure that private information is loaded if necessary.
Return non-nil if `twittering-use-master-password' is nil or private
information has been already loaded. Also, return non-nil
if `twittering-use-master-password' is non-nil and this function succeeded
in loading private information.
Return nil if private information cannot be loaded."
  (if (or (not twittering-use-master-password)
	  (twittering-private-info-loaded-p))
      ;; The private information is unnecessary or already loaded.
      t
    (cond
     ((not (twittering-capable-of-encryption-p))
      (message "You need GnuPG and (EasyPG or alpaca.el) for master password!")
      nil)
     ((and (memq twittering-auth-method '(oauth xauth))
	   (file-exists-p twittering-private-info-file))
      (cond
       ((twittering-load-private-info-with-guide)
	(setq twittering-private-info-file-loaded t)
	(message "The authorized token is loaded.")
	t)
       (t
	(message "Failed to load an authorized token from \"%s\"."
		 twittering-private-info-file)
	nil)))
     (t
      ;; The file for private infomation does not exist now.
      t))))

;;;;
;;;; Asynchronous retrieval
;;;;

(defvar twittering-url-data-hash (make-hash-table :test 'equal))
(defvar twittering-url-request-list nil)
(defvar twittering-url-request-sentinel-hash (make-hash-table :test 'equal))
(defvar twittering-internal-url-queue nil)
(defvar twittering-url-request-resolving-p nil)
(defvar twittering-url-request-retry-limit 3)
(defvar twittering-url-request-sentinel-delay 1.0
  "*Delay from completing retrieval to invoking associated sentinels.
Sentinels registered by `twittering-url-retrieve-async' will be invoked
after retrieval is completed and Emacs remains idle a certain time, which
this variable specifies. The unit is second.")

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
      (let* ((url (car twittering-internal-url-queue))
	     (request (twittering-make-http-request-from-uri "GET" nil url))
	     (additional-info `((uri . ,url))))
	(twittering-send-http-request
	 request additional-info
	 'twittering-url-retrieve-async-sentinel
	 'twittering-url-retrieve-async-clean-up-sentinel)))))

(defun twittering-url-retrieve-async-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(uri (cdr (assq 'uri (assq 'request connection-info)))))
    (when (string= status-code "200")
      (let ((body (string-as-unibyte (buffer-string))))
	(puthash uri body twittering-url-data-hash)
	(setq twittering-internal-url-queue
	      (remove uri twittering-internal-url-queue))
	(let ((sentinels (gethash uri twittering-url-request-sentinel-hash)))
	  (when sentinels
	    (remhash uri twittering-url-request-sentinel-hash))
	  (twittering-run-on-idle twittering-url-request-sentinel-delay
				  (lambda (sentinels uri body)
				    (mapc (lambda (func)
					    (funcall func uri body))
					  sentinels)
				    ;; Resolve the rest of requests.
				    (setq twittering-url-request-resolving-p
					  nil)
				    (twittering-resolve-url-request))
				  sentinels uri body)
	  ;;  Without the following nil, it seems that the value of
	  ;; `sentinels' is displayed.
	  nil)))))

(defun twittering-url-retrieve-async-clean-up-sentinel (proc status connection-info)
  (when (memq status '(exit signal closed failed))
    (let* ((uri (cdr (assq 'uri connection-info)))
	   (current (gethash uri twittering-url-data-hash)))
      (when (or (null current) (integerp current))
	;; Increment the counter on failure and then retry retrieval.
	(puthash uri (1+ (or current 0)) twittering-url-data-hash)
	(setq twittering-url-request-resolving-p nil)
	(twittering-resolve-url-request)))))

(defun twittering-url-retrieve-async (url &optional sentinel)
  "Retrieve URL asynchronously and call SENTINEL with the retrieved data.
The request is placed at the last of queries queue. When the data has been
retrieved and Emacs remains idle a certain time specified by
`twittering-url-request-sentinel-delay', SENTINEL will be called as
 (funcall SENTINEL URL url-data).
The retrieved data can be referred as (gethash URL twittering-url-data-hash)."
  (let ((data (gethash url twittering-url-data-hash)))
    (cond
     ((or (null data) (integerp data))
      (add-to-list 'twittering-url-request-list url t)
      (when sentinel
	(let ((current (gethash url twittering-url-request-sentinel-hash)))
	  (unless (member sentinel current)
	    (puthash url (cons sentinel current)
		     twittering-url-request-sentinel-hash))))
      (twittering-resolve-url-request)
      nil)
     (t
      ;; URL has been already retrieved.
      (twittering-run-on-idle twittering-url-request-sentinel-delay
			      sentinel url data)
      data))))

;;;;
;;;; XML parser
;;;;

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

(defvar twittering-unicode-replacement-char
  ;; "Unicode Character 'REPLACEMENT CHARACTER' (U+FFFD)"
  (or (twittering-ucs-to-char-internal #xFFFD)
      ??)
  "*Replacement character returned by `twittering-ucs-to-char' when it fails
to decode a code.")

(defun twittering-ucs-to-char (code-point)
  "Return a character specified by CODE-POINT in Unicode.
If it fails to decode the code, return `twittering-unicode-replacement-char'."
  (or (twittering-ucs-to-char-internal code-point)
      twittering-unicode-replacement-char))

(defadvice decode-char (after twittering-add-fail-over-to-decode-char)
  (when (null ad-return-value)
    (setq ad-return-value twittering-unicode-replacement-char)))

(defun twittering-xml-parse-region (&rest args)
  "Wrapped `xml-parse-region' in order to avoid decoding errors.
After activating the advice `twittering-add-fail-over-to-decode-char',
`xml-parse-region' is called. This prevents `xml-parse-region' from
exiting abnormally by decoding unknown numeric character reference."
  (let ((activated (ad-is-active 'decode-char)))
    (ad-enable-advice
     'decode-char 'after 'twittering-add-fail-over-to-decode-char)
    (ad-activate 'decode-char)
    (unwind-protect
	(condition-case err
	    (apply 'xml-parse-region args)
	  (error
	   (message "Failed to parse the retrieved XML.")
	   nil))
      (ad-disable-advice 'decode-char 'after
			 'twittering-add-fail-over-to-decode-char)
      (if activated
	  (ad-activate 'decode-char)
	(ad-deactivate 'decode-char)))))

;;;;
;;;; JSON parser with a fallback character
;;;;

(defconst twittering-surrogate-pair-regexp
  (if (<= 23 emacs-major-version)
      ;; Literal strings such as "\uXXXX" is not allowed in Emacs 21
      ;; and earlier. A character of invalid code point such as U+D800
      ;; is not allowed in Emacs 22.
      ;; To avoid errors caused by literal strings invalid in Emacs 22
      ;; and earlier, the regexp is generated indirectly.
      (format "[%c-%c][%c-%c]"
	      (decode-char 'ucs #xd800)
	      (decode-char 'ucs #xdbff)
	      (decode-char 'ucs #xdc00)
	      (decode-char 'ucs #xdfff))
    ;; A regexp that never matches any strings.
    "\\'\\`")
  "Regexp to match a surrogate pair for CESU-8.
In Emacs 22 and earlier, this variable is initialized by a regexp
that never matches any string because code points for a surrogate pair,
from U+D800 to U+DFFF, are invalid.")

(defun twittering-decode-surrogate-pairs-as-cesu-8 (str)
  "Decode surrogate pairs in STR similarly to CESU-8.
If STR includes surrogate pairs represented by code points from U+D800 to
U+DFFF, decode them with CESU-8 and return the result.

A character not in the Basic Multilingual Plane is represented by a surrogate
pair in JSON (RFC4627). This is similar to CESU-8. But the function
`json-read' in `json.el' does not correctly decode surrogate pairs. Therefore,
`json-read' may return a string including invalid code points from U+D800 to
U+DFFF. This function decodes such invalid code points."
  (let ((str str)
	(prev 0)
	(current 0)
	(result ""))
    (while (setq current
		 (string-match twittering-surrogate-pair-regexp str prev))
      (let* ((next (match-end 0))
	     (decoded-str
	      (decode-coding-string
	       (mapconcat
		(lambda (c)
		  (let* ((code-point (encode-char c 'ucs))
			 (b1 (/ code-point #x100))
			 (b2 (% code-point #x100)))
		    (unibyte-string b1 b2)))
		(match-string 0 str)
		"")
	       'utf-16)))
	(setq result
	      (concat result
		      (substring str prev current)
		      decoded-str))
	(setq prev next)))
    (setq result (concat result (substring str prev)))
    result))

(defadvice json-read-string (after twittering-decode-surrogate-pairs-as-cesu-8)
  (when (<= 23 emacs-major-version)
    (setq ad-return-value
	  (twittering-decode-surrogate-pairs-as-cesu-8 ad-return-value))))

(defun twittering-json-read (&rest args)
  "Wrapped `json-read' in order to avoid decoding errors.
`json-read' is called after activating the advice
`twittering-add-fail-over-to-decode-char'.
This prevents `json-read' from exiting abnormally by decoding an unknown
numeric character reference."
  (let ((activated (ad-is-active 'decode-char))
	(json-activated (ad-is-active 'json-read-string)))
    (ad-enable-advice
     'decode-char 'after 'twittering-add-fail-over-to-decode-char)
    (ad-activate 'decode-char)
    (ad-enable-advice 'json-read-string 'after
		      'twittering-decode-surrogate-pairs-as-cesu-8)
    (ad-activate 'json-read-string)
    (unwind-protect
	(condition-case err
	    (apply 'json-read args)
	  (error
	   (message "Failed to parse the retrieved JSON.")
	   nil))
      (ad-disable-advice 'decode-char 'after
			 'twittering-add-fail-over-to-decode-char)
      (ad-disable-advice 'json-read-string 'after
			 'twittering-decode-surrogate-pairs-as-cesu-8)
      (if activated
	  (ad-activate 'decode-char)
	(ad-deactivate 'decode-char))
      (if json-activated
	  (ad-activate 'json-read-string)
	(ad-deactivate 'json-read-string))
      )))

;;;;
;;;; Window configuration
;;;;

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

(defun twittering-pop-to-buffer (buf)
  "Select the buffer BUF in some window.
The behavior is determined by the function specified by
`twittering-pop-to-buffer-function'."
  (funcall twittering-pop-to-buffer-function buf))

(defun twittering-pop-to-buffer-simple (buf)
  "Select the buffer BUF by using `pop-to-buffer'."
  (let ((win (selected-window)))
    (pop-to-buffer buf)
    ;; This is required because the new window generated by `pop-to-buffer'
    ;; may hide the region following the current position.
    (twittering-ensure-whole-of-status-is-visible win)))

(defun twittering-pop-to-buffer-in-current-window (buf &optional win)
  "Select the buffer BUF in the window WIN by splitting it.
If WIN is nil, the selected window is splitted."
  (let* ((win (or win (selected-window)))
	 (size
	  (let ((rest (- (window-height win) 15)))
	    (if (<= rest 3)
		;; To avoid an error due to a too small window.
		nil
	      rest)))
	 (new-win (split-window win size)))
    (select-window new-win)
    (switch-to-buffer buf)))

(defun twittering-pop-to-buffer-in-largest-window (buf)
  "Select the buffer BUF in the largest window by splitting it."
  (let ((win
	 (lexical-let ((max-area 0)
		       (largest-win nil))
	   (walk-windows
	    (lambda (win)
	      (let ((area (* (window-height win) (window-width win))))
		(when (< max-area area)
		  (setq max-area area)
		  (setq largest-win win)))))
	   largest-win)))
    (twittering-pop-to-buffer-in-current-window buf win)))

(defun twittering-pop-to-buffer-in-bottom-largest-window (buf)
  "Select the buffer BUF in the window largest on bottom by splitting it."
  (let* ((bottom-win-list
	  (lexical-let ((win-list '())
			(max-bottom 0))
	    (walk-windows
	     (lambda (win)
	       (let ((bottom (nth 3 (window-edges win))))
		 (cond
		  ((< max-bottom bottom)
		   (setq max-bottom bottom)
		   (setq win-list `(,win)))
		  ((= max-bottom bottom)
		   (setq win-list (cons win win-list)))
		  (t
		   nil)))))
	    win-list))
	 (win
	  (lexical-let ((max-area 0)
			(largest-win nil))
	    (mapc (lambda (win)
		    (let ((area (* (window-height win) (window-width win))))
		      (when (< max-area area)
			(setq largest-win win)
			(setq max-area area))))
		  bottom-win-list)
	    largest-win)))
    (twittering-pop-to-buffer-in-current-window buf win)))

;;;;
;;;; URI shortening
;;;;

(defun twittering-tinyurl-get (longurl &optional service)
  "Shorten LONGURL with the service specified by `twittering-tinyurl-service'."
  (let* ((service (or service twittering-tinyurl-service))
	 (api (cdr (assq service twittering-tinyurl-services-map)))
	 (request-generator (when (listp api) (elt api 0)))
	 (post-process (when (listp api) (elt api 1)))
	 (encoded-url (twittering-percent-encode longurl))
	 (request
	  (cond
	   ((stringp api)
	    (twittering-make-http-request-from-uri
	     "GET" nil (concat api encoded-url)))
	   ((stringp request-generator)
	    (twittering-make-http-request-from-uri
	     "GET" nil (concat request-generator encoded-url)))
	   ((functionp request-generator)
	    (funcall request-generator service longurl))
	   (t
	    (error "%s is invalid. try one of %s"
		   (symbol-name service)
		   (mapconcat (lambda (x) (symbol-name (car x)))
			      twittering-tinyurl-services-map ", "))
	    nil)))
	 (additional-info `((longurl . ,longurl))))
    (cond
     ((null request)
      (error "Failed to generate a HTTP request for shortening %s with %s"
	     longurl (symbol-name service))
      nil)
     (t
      (lexical-let ((result 'queried))
	(let ((proc
	       (twittering-send-http-request
		request additional-info
		(lambda (proc status connection-info header-info)
		  (let ((status-line (cdr (assq 'status-line header-info)))
			(status-code (cdr (assq 'status-code header-info))))
		    (case-string
		     status-code
		     (("200")
		      (setq result (buffer-string))
		      nil)
		     (t
		      (setq result nil)
		      (format "Response: %s" status-line)))))
		(lambda (proc status connection-info)
		  (when (and (not (twittering-process-alive-p proc))
			     (eq result 'queried))
		    (setq result nil))))))
	  (twittering-wait-while nil 0.1
				 (and (eq result 'queried)
				      (twittering-process-alive-p proc)))
	  (when (and (eq result 'queried)
		     (not (twittering-process-alive-p proc)))
	    ;; If the process has been dead, wait a moment because
	    ;; Emacs may be in the middle of evaluating the sentinel.
	    (twittering-wait-while 10 0.1
				   (eq result 'queried)
				   nil
				   ;; Reset `result' on timeout.
				   (setq result nil))))
	(let ((processed-result (if (and result (functionp post-process))
				    (funcall post-process service result)
				  result)))
	  (if processed-result
	      processed-result
	    (error "Failed to shorten a URL %s with %s"
		   longurl (symbol-name service))
	    nil)))))))

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

(defun twittering-make-http-request-for-bitly (service longurl)
  "Make a HTTP request for URL shortening service bit.ly or j.mp.
Before calling this, you have to configure `twittering-bitly-login' and
`twittering-bitly-api-key'."
  (let* ((query-string
	  (mapconcat
	   (lambda (entry)
	     (concat (car entry) "=" (cdr entry)))
	   `(("login" . ,twittering-bitly-login)
	     ("apiKey" . ,twittering-bitly-api-key)
	     ("format" . "txt")
	     ("longUrl" . ,(twittering-percent-encode longurl)))
	   "&"))
	 (prefix
	  (cdr (assq service '((bit.ly . "http://api.bit.ly/v3/shorten?")
			       (j.mp . "http://api.j.mp/v3/shorten?")))))
	 (uri (concat prefix query-string)))
    (twittering-make-http-request-from-uri "GET" nil uri)))

;;;;
;;;; Timeline spec
;;;;

;;; Timeline spec as S-expression
;;; - (user USER): timeline of the user whose name is USER. USER is a string.
;;; - (list USER LIST):
;;;     the list LIST of the user USER. LIST and USER are strings.
;;;
;;; - (direct_messages): received direct messages.
;;; - (direct_messages_sent): sent direct messages.
;;; - (favorites): favorites timeline for the current user.
;;; - (favorites USER): favorites timeline for the specified user.
;;; - (friends): friends timeline.
;;; - (home): home timeline.
;;; - (mentions): mentions timeline.
;;;     mentions (status containing @username) for the authenticating user.
;;; - (public): public timeline.
;;; - (replies): replies.
;;; - (retweeted_by_me): retweets posted by the authenticating user.
;;; - (retweeted_by_user USER): retweets posted by the user.
;;; - (retweeted_to_me): retweets posted by the authenticating user's friends.
;;; - (retweeted_to_user USER): retweets posted to the user.
;;; - (retweets_of_me):
;;;     tweets of the authenticated user that have been retweeted by others.
;;; - (single ID): the single tweet specified by ID.
;;;
;;; - (search STRING): the result of searching with query STRING.
;;;
;;; - (exclude-if FUNC SPEC):
;;;     the same timeline as SPEC, except that it does not include tweets
;;;     that FUNC returns non-nil for.
;;; - (exclude-re REGEXP-STRING SPEC):
;;;     the same timeline as SPEC, except that it does not include tweets
;;;     that matches the regular expression specified by REGEXP-STRING.
;;;
;;; - (merge SPEC1 SPEC2 ...): result of merging timelines SPEC1 SPEC2 ...
;;;

;;; Timeline spec string
;;;
;;; SPEC ::= PRIMARY | COMPOSITE
;;; PRIMARY ::= USER | LIST | DIRECT_MESSSAGES | DIRECT_MESSSAGES_SENT
;;;             | FRIENDS | HOME | MENTIONS | PUBLIC | REPLIES
;;;             | RETWEETED_BY_ME | RETWEETED_BY_USER
;;;             | RETWEETED_TO_ME | RETWEETED_TO_USER | RETWEETS_OF_ME
;;;             | SEARCH
;;; COMPOSITE ::= EXCLUDE-IF | EXCLUDE-RE | MERGE
;;;
;;; USER ::= /[a-zA-Z0-9_-]+/
;;; LIST ::= USER "/" LISTNAME
;;; LISTNAME ::= /[a-zA-Z0-9_-]+/
;;; DIRECT_MESSSAGES ::= ":direct_messages"
;;; DIRECT_MESSSAGES_SENT ::= ":direct_messages_sent"
;;; FAVORITES ::= ":favorites" | ":favorites/" USER
;;; FRIENDS ::= ":friends"
;;; HOME ::= ":home" | "~"
;;; MENTIONS ::= ":mentions"
;;; PUBLIC ::= ":public"
;;; REPLIES ::= ":replies" | "@"
;;; RETWEETED_BY_ME ::= ":retweeted_by_me"
;;; RETWEETED_BY_USER ::= ":retweeted_by_user/" USER
;;; RETWEETED_TO_ME ::= ":retweeted_to_me"
;;; RETWEETED_TO_USER ::= ":retweeted_to_user/" USER
;;; RETWEETS_OF_ME ::= ":retweets_of_me"
;;; SINGLE ::= ":single/" ID
;;; ID ::= /[0-9]+/
;;;
;;; SEARCH ::= ":search/" QUERY_STRING "/"
;;; QUERY_STRING ::= any string, where "/" is escaped by a backslash.
;;;
;;; EXCLUDE-IF ::= ":exclude-if/" FUNC "/" SPEC
;;; FUNC ::= LAMBDA EXPRESSION | SYMBOL
;;; EXCLUDE-RE ::= ":exclude-re/" REGEXP "/" SPEC
;;;
;;; MERGE ::= "(" MERGED_SPECS ")"
;;; MERGED_SPECS ::= SPEC | SPEC "+" MERGED_SPECS
;;;

(defvar twittering-regexp-hash
  (let ((full-width-number-sign (twittering-ucs-to-char #xff03)))
    ;; Unicode Character 'FULLWIDTH NUMBER SIGN' (U+FF03)
    (concat "\\(?:#\\|" (char-to-string full-width-number-sign) "\\)")))

(defvar twittering-regexp-atmark
  (let ((full-width-commercial-at (twittering-ucs-to-char #xff20)))
    ;; Unicode Character 'FULLWIDTH COMMERCIAL AT' (U+FF20)
    (concat "\\(?:@\\|" (char-to-string full-width-commercial-at) "\\)")))

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
     ((eq type 'favorites)
      (if value
	  (concat ":favorites/" (car value))
	":favorites"))
     ((eq type 'friends) ":friends")
     ((eq type 'home) (if shorten "~" ":home"))
     ((eq type 'mentions) ":mentions")
     ((eq type 'public) ":public")
     ((eq type 'replies) (if shorten "@" ":replies"))
     ((eq type 'retweeted_by_me) ":retweeted_by_me")
     ((eq type 'retweeted_by_user) (concat ":retweeted_by_user/" (car value)))
     ((eq type 'retweeted_to_me) ":retweeted_to_me")
     ((eq type 'retweeted_to_user) (concat ":retweeted_to_user/" (car value)))
     ((eq type 'retweets_of_me) ":retweets_of_me")
     ((eq type 'single) (concat ":single/" (car value)))
     ((eq type 'search)
      (let ((query (car value)))
	(concat ":search/"
		(replace-regexp-in-string "\\(\\\\\\|/\\)" "\\\\\\1" query)
		"/")))
     ;; composite
     ((eq type 'exclude-if)
      (let ((func (car value))
	    (spec (cadr value))
	    (print-level nil))
	(concat ":exclude-if/" (prin1-to-string func) "/"
		(twittering-timeline-spec-to-string spec))))
     ((eq type 'exclude-re)
      (let ((regexp-str (car value))
	    (spec (cadr value))
	    (print-level nil))
	(concat ":exclude-re/"
		(replace-regexp-in-string "/" "\\\\\/" regexp-str)
		"/"
		(twittering-timeline-spec-to-string spec))))
     ((eq type 'merge)
      (concat "("
	      (mapconcat 'twittering-timeline-spec-to-string value "+")
	      ")"))
     (t
      nil))))

(eval-and-compile
  (defmacro twittering-make-user-timeline-spec-direct (user)
    `(list 'user ,user))
  (defmacro twittering-make-list-timeline-spec-direct (owner listname)
    `(list 'list ,owner ,listname))
  (defmacro twittering-make-hashtag-timeline-spec-direct (tag)
    `(list 'search (concat "#" ,tag)))
  (defmacro twittering-make-hashtag-timeline-spec-string-direct (tag)
    `(concat "#" ,tag)))

(defun twittering-extract-timeline-spec (str &optional unresolved-aliases)
  "Extract one timeline spec from STR.
Return cons of the spec and the rest string."
  (cond
   ((null str)
    (error "STR is nil")
    nil)
   ((string-match "^\\([a-zA-Z0-9_-]+\\)/\\([[:word:]_-]+\\)" str)
    (let ((user (match-string 1 str))
	  (listname (match-string 2 str))
	  (rest (substring str (match-end 0))))
      `(,(twittering-make-list-timeline-spec-direct user listname) . ,rest)))
   ((string-match "^\\([a-zA-Z0-9_-]+\\)" str)
    (let ((user (match-string 1 str))
	  (rest (substring str (match-end 0))))
      `(,(twittering-make-user-timeline-spec-direct user) . ,rest)))
   ((string-match "^~" str)
    `((home) . ,(substring str (match-end 0))))
   ((string-match (concat "^" twittering-regexp-atmark) str)
    `((mentions) . ,(substring str (match-end 0))))
   ((string-match (concat "^" twittering-regexp-hash "\\([[:alpha:]0-9_-]+\\)")
		  str)
    (let* ((tag (match-string 1 str))
	   (rest (substring str (match-end 0))))
      `(,(twittering-make-hashtag-timeline-spec-direct tag) . ,rest)))
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
       ((string= type "favorites")
	(if (string-match "^:favorites/\\([a-zA-Z0-9_-]+\\)" str)
	    (let ((rest (substring str (match-end 0))))
	      `((favorites ,(match-string 1 str)) . ,rest))
	  `((favorites) . ,following)))
       ((string-match "^:retweeted_by_user/\\([a-zA-Z0-9_-]+\\)" str)
	(let ((user (match-string 1 str))
	      (rest (substring str (match-end 0))))
	  `((retweeted_by_user ,user) . ,rest)))
       ((string-match "^:retweeted_to_user/\\([a-zA-Z0-9_-]+\\)" str)
	(let ((user (match-string 1 str))
	      (rest (substring str (match-end 0))))
	  `((retweeted_to_user ,user) . ,rest)))
       ((string-match "^:single/\\([0-9]+\\)" str)
	(let ((id (match-string 1 str))
	      (rest (substring str (match-end 0))))
	  `((single ,id) . ,rest)))
       ((string= type "search")
	(if (string-match "^:search/\\(\\(.*?[^\\]\\)??\\(\\\\\\\\\\)*\\)??/"
			  str)
	    (let* ((escaped-query (or (match-string 1 str) ""))
		   (query (replace-regexp-in-string "\\\\\\(\\\\\\|/\\)" "\\1"
						    escaped-query))
		   (rest (substring str (match-end 0))))
	      (if (not (string= "" escaped-query))
		  `((search ,query) . ,rest)
		(error "\"%s\" has no valid regexp" str)
		nil))))
       ((string= type "exclude-if")
	(let ((result-pair
	       (cond
		((string-match "^:exclude-if/\\([^(/]+\\)/" str)
		 `(,(intern (match-string 1 str)) . ,(match-end 1)))
		((string-match "^:exclude-if/" str)
		 (condition-case err
		     (read-from-string str (match-end 0))
		   (error
		    nil))))))
	  (if result-pair
	      (let ((func (car result-pair))
		    (pos (cdr result-pair)))
		(cond
		 ((not (functionp func))
		  (error "\"%s\" has an invalid function" str)
		  nil)
		 ((<= (length str) (1+ pos))
		  (error "\"%s\" has no timeline spec" str)
		  nil)
		 ((not (char-equal ?/ (aref str pos)))
		  (error "\"%s\" has no delimiter" str)
		  nil)
		 (t
		  (let* ((pair (twittering-extract-timeline-spec
				(substring str (1+ pos)) unresolved-aliases))
			 (spec (car pair))
			 (rest (cdr pair)))
		    `((exclude-if ,func ,spec) . ,rest)))))
	    (error "\"%s\" has an invalid function" str)
	    nil)))
       ((string= type "exclude-re")
	(cond
	 ((string-match "^:exclude-re/\\(\\(.*?[^\\]\\)??\\(\\\\\\\\\\)*\\)??/"
			str)
	  (let* ((escaped-regexp (or (match-string 1 str) ""))
		 (regexp
		  (replace-regexp-in-string "\\\\/" "/" escaped-regexp nil t))
		 (following (substring str (match-end 0))))
	    (cond
	     ((string= "" escaped-regexp)
	      (error "\"%s\" has no valid regexp" str)
	      nil)
	     (t
	      (let* ((pair (twittering-extract-timeline-spec
			    following unresolved-aliases))
		     (spec (car pair))
		     (rest (cdr pair)))
		`((exclude-re ,regexp ,spec) . ,rest))))))
	 (t
	  (error "\"%s\" has no valid regexp" str)
	  nil)))
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
		 (twittering-remove-duplicates
		  (apply 'append
			 (mapcar (lambda (x) (if (eq 'merge (car x))
						 (cdr x)
					       (list x)))
				 (reverse result))))))
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

(defun twittering-string-to-timeline-spec (spec-str &optional noerror)
  "Convert SPEC-STR into a timeline spec.
If SPEC-STR is invalid as a timeline spec string, raise an error or return
nil if NOERROR is non-nil."
  (let ((result-pair
	 (condition-case err
	     (twittering-extract-timeline-spec spec-str)
	   (error
	    (if noerror
		nil
	      (signal (car err) (cdr err))
	      nil)))))
    (if (and result-pair (string= "" (cdr result-pair)))
	(car result-pair)
      nil)))

(defun twittering-timeline-spec-primary-p (spec)
  "Return non-nil if SPEC is a primary timeline spec.
`primary' means that the spec is not a composite timeline spec such as
`merge'."
  (let ((primary-spec-types
	 '(user list
		direct_messages direct_messages_sent
		favorites friends home mentions public replies
		search
		retweeted_by_me retweeted_by_user
		retweeted_to_me retweeted_to_user
		retweets_of_me
		single))
	(type (car spec)))
    (memq type primary-spec-types)))

(defun twittering-timeline-spec-composite-p (spec)
  "Return non-nil if SPEC is a composite timeline spec.
`composite' means that the spec depends on other timelines."
  (let ((composite-spec-types
	 '(exclude-if exclude-re merge))
	(type (car spec)))
    (memq type composite-spec-types)))

(defun twittering-timeline-spec-depending-on-p (spec base-spec)
  "Return non-nil if SPEC depends on BASE-SPEC."
  (cond
   ((twittering-timeline-spec-primary-p spec)
    (equal spec base-spec))
   ((equal spec base-spec)
    t)
   (t
    (remove
     nil
     (mapcar
      (lambda (direct-base-spec)
	(twittering-timeline-spec-depending-on-p direct-base-spec base-spec))
      (twittering-get-base-timeline-specs spec))))))

(defun twittering-timeline-spec-is-user-p (spec)
  "Return non-nil if SPEC is a user timeline."
  (and (consp spec) (eq 'user (car spec))))

(defun twittering-timeline-spec-is-direct-messages-p (spec)
  "Return non-nil if SPEC is a timeline spec which is related of
direct_messages."
  (and spec
       (memq (car spec) '(direct_messages direct_messages_sent))))

(defun twittering-timeline-spec-is-search-p (spec)
  "Return non-nil if SPEC is a search timeline spec."
  (and (consp spec)
       (eq 'search (car spec))))

(defun twittering-extract-query-string-from-search-timeline-spec (spec)
  "Return the query string if SPEC is a search timeline spec.
If SPEC is not a search timeline spec, return nil."
  (and (eq 'search (car spec))
       (cadr spec)))

(defun twittering-equal-string-as-timeline (spec-str1 spec-str2)
  "Return non-nil if SPEC-STR1 equals SPEC-STR2 as a timeline spec.
If either SPEC-STR1 or SPEC-STR2 is invalid as a timeline spec string,
return nil."
  (if (and (stringp spec-str1) (stringp spec-str2))
      (let ((spec1 (twittering-string-to-timeline-spec spec-str1 t))
	    (spec2 (twittering-string-to-timeline-spec spec-str2 t)))
	(equal spec1 spec2))
    nil))

(defun twittering-get-base-timeline-specs (spec)
  "Return the timeline specs on which the timeline SPEC depends.
If SPEC is primary, returns a list consisting of itself.
The result timelines may be a composite timeline."
  (let ((type (car spec)))
    (cond
     ((twittering-timeline-spec-primary-p spec)
      `(,spec))
     ((memq type '(exclude-if exclude-re))
      `(,(elt spec 2)))
     ((eq type 'merge)
      (cdr spec))
     (t
      nil))))

(defun twittering-get-primary-base-timeline-specs (spec)
  "Return the primary timeline specs on which the timeline SPEC depends.
If SPEC is primary, returns a list consisting of itself.
The result timelines are primary."
  (if (twittering-timeline-spec-primary-p spec)
      `(,spec)
    (twittering-remove-duplicates
     (apply 'append
	    (mapcar 'twittering-get-primary-base-timeline-specs
		    (twittering-get-base-timeline-specs spec))))))

(defun twittering-get-dependent-timeline-specs (base-spec)
  "Return a list of timeline specs that depend on BASE-SPEC.
If BASE-SPEC is a primary timeline spec, the return value consists of
BASE-SPEC and composite timeline specs that depend on BASE-SPEC and are
bound to a live buffer.
If BASE-SPEC is a composite timeline spec, the return value consists of
composite timeline specs that depend on BASE-SPEC and are bound to a live
buffer."
  (twittering-remove-duplicates
   `(;; BASE-SPEC may not be bound to a live buffer.
     ,@(when (twittering-timeline-spec-primary-p base-spec)
	 `(,base-spec))
     ,@(remove
	nil
	(mapcar
	 (lambda (spec)
	   (when (twittering-timeline-spec-depending-on-p spec base-spec)
	     spec))
	 (mapcar 'twittering-get-timeline-spec-for-buffer
		 (twittering-get-buffer-list)))))))

(defun twittering-generate-composite-timeline (spec base-spec base-statuses)
  "Generate statuses for the timeline SPEC from BASE-STATUSES.
BASE-STATUSES must originate from the BASE-SPEC timeline.
If SPEC is a primary timeline and equals BASE-SPEC, just return BASE-STATUSES.
If SPEC is a primary timeline and does not equal BASE-SPEC, return nil."
  (let ((type (car spec)))
    (cond
     ((twittering-timeline-spec-primary-p spec)
      (if (equal spec base-spec)
	  base-statuses
	nil))
     ((eq type 'exclude-if)
      (let* ((direct-base (car (twittering-get-base-timeline-specs spec)))
	     (direct-base-statuses
	      (twittering-generate-composite-timeline direct-base
						      base-spec base-statuses))
	     (func (elt spec 1)))
	(remove nil
		(mapcar (lambda (status)
			  (unless (funcall func status)
			    status))
			direct-base-statuses))))
     ((eq type 'exclude-re)
      (let* ((direct-base (car (twittering-get-base-timeline-specs spec)))
	     (direct-base-statuses
	      (twittering-generate-composite-timeline direct-base
						      base-spec base-statuses))
	     (regexp (elt spec 1)))
	(remove nil
		(mapcar
		 (lambda (status)
		   (unless (string-match regexp (cdr (assq 'text status)))
		     status))
		 direct-base-statuses))))
     ((eq type 'merge)
      (sort
       (apply 'append
	      (mapcar (lambda (direct-base-spec)
			;; `copy-sequence' is required because `sort'
			;; modifies the appended list that may include
			;; `base-statuses' as a tail.
			;; `base-statuses' may refer to the original list
			;; which already retrieved tweets are registered
			;; with. It must not be modified.
			(copy-sequence
			 (twittering-generate-composite-timeline
			  direct-base-spec base-spec base-statuses)))
		      (twittering-get-base-timeline-specs spec)))
       (lambda (status1 status2)
	 (let ((id1 (cdr (assq 'id status1)))
	       (id2 (cdr (assq 'id status2))))
	   (twittering-status-id< id2 id1)))))
     (t
      nil))))

;;;;
;;;; Retrieved statuses (timeline data)
;;;;

(defun twittering-current-timeline-id-table (&optional spec)
  (let ((spec (or spec (twittering-current-timeline-spec))))
    (if spec
	(elt (gethash spec twittering-timeline-data-table) 0)
      nil)))

(defun twittering-current-timeline-referring-id-table (&optional spec)
  "Return the hash from a ID to the ID of the first observed status
referring the former ID."
  (let* ((spec (or spec (twittering-current-timeline-spec)))
	 (type (car spec)))
    (cond
     ((null spec)
      nil)
     ((memq type '(exclude-if exclude-re merge))
      ;; Use the first non-nil table instead of merging the all tables
      ;; because it may take a long time to merge them.
      (car
       (remove
	nil
	(mapcar (lambda (base-spec)
		  (elt (gethash base-spec twittering-timeline-data-table) 1))
		(twittering-get-primary-base-timeline-specs spec)))))
     ((eq type 'single)
      ;; Single tweet timelines are registered in a special way.
      ;; See `twittering-retrieve-single-tweet-sentinel'.
      (elt (gethash '(:single) twittering-timeline-data-table) 1))
     (t
      (elt (gethash spec twittering-timeline-data-table) 1)))))

(defun twittering-current-timeline-data (&optional spec)
  (let* ((spec (or spec (twittering-current-timeline-spec)))
	 (type (car spec)))
    (cond
     ((null spec)
      nil)
     ((eq type 'single)
      (let* ((id (cadr spec))
	     (status (twittering-find-status id)))
	(if status
	    `(,status)
	  nil)))
     ((memq type '(exclude-if exclude-re merge))
      (let ((primary-base-specs
	     (twittering-get-primary-base-timeline-specs spec)))
	(sort
	 (apply
	  'append
	  (mapcar
	   (lambda (primary-spec)
	     ;; `copy-sequence' is required to prevent `sort'
	     ;; from modifying lists of statuses in the database
	     ;; `twittering-timeline-data-table'.
	     ;; The result of `twittering-generate-composite-timeline'
	     ;; may include a list in the database. If so, the simply
	     ;; appended list include it as a tail.
	     (copy-sequence
	      (twittering-generate-composite-timeline
	       spec
	       primary-spec (twittering-current-timeline-data primary-spec))))
	   primary-base-specs))
	 (lambda (status1 status2)
	   (let ((id1 (cdr (assq 'id status1)))
		 (id2 (cdr (assq 'id status2))))
	     (twittering-status-id< id2 id1))))))
     (t
      (elt (gethash spec twittering-timeline-data-table) 2)))))

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
     (lambda (spec)
       (let ((buffer (twittering-get-buffer-from-spec spec)))
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
     (twittering-remove-duplicates
      (apply 'append
	     (mapcar
	      (lambda (data)
		(let ((spec (car data)))
		  ;; Update the entry for `spec' in
		  ;; `twittering-timeline-data-table' with the new
		  ;; timeline-data that does not include `status'.
		  (puthash spec (cdr data) twittering-timeline-data-table)
		  (twittering-get-dependent-timeline-specs spec)))
	      modified-spec))))))

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
  "Add STATUSES as new statuses for SPEC and update derived timelines.
The function returns a list of lists including an updated timeline spec
string and the number of new statuses for the timeline."
  (let* ((spec (or spec (twittering-current-timeline-spec)))
	 (id-table
	  (or (twittering-current-timeline-id-table spec)
	      (make-hash-table :test 'equal)))
	 (referring-id-table
	  (or (twittering-current-timeline-referring-id-table spec)
	      (make-hash-table :test 'equal)))
	 (timeline-data (twittering-current-timeline-data spec)))
    (let* ((new-statuses
	    (remove nil
		    (mapcar
		     (lambda (status)
		       (let ((id (cdr (assq 'id status)))
			     (retweeted-id (cdr (assq 'retweeted-id status))))
			 (unless (or (not retweeted-id)
				     (gethash retweeted-id referring-id-table))
			   ;; Store the id of the first observed tweet
			   ;; that refers `retweeted-id'.
			   (puthash retweeted-id id referring-id-table))
			 (if (gethash id id-table)
			     nil
			   (puthash id status id-table)
			   (puthash id id referring-id-table)
			   `((source-spec . ,spec)
			     ,@status))))
		     statuses)))
	   (new-statuses
	    ;; Sort tweets by ID.
	    ;; This is necessary because `twittering-render-timeline' assumes
	    ;; that given tweets are ordered.
	    (sort new-statuses
		  (lambda (status1 status2)
		    (let ((id1 (cdr (assq 'id status1)))
			  (id2 (cdr (assq 'id status2))))
		      (twittering-status-id< id2 id1))))))
      (when new-statuses
	(let ((new-timeline-data
	       (sort (append new-statuses timeline-data)
		     (lambda (status1 status2)
		       (let ((id1 (cdr (assq 'id status1)))
			     (id2 (cdr (assq 'id status2))))
			 (twittering-status-id< id2 id1))))))
	  (puthash spec `(,id-table ,referring-id-table ,new-timeline-data)
		   twittering-timeline-data-table))
	(let ((twittering-new-tweets-spec spec)
	      (twittering-new-tweets-statuses new-statuses)
	      (twittering-new-tweets-count (length new-statuses)))
	  (run-hooks 'twittering-new-tweets-hook)))
      ;; Update timelines derived from SPEC and return the number of
      ;; new tweets for each updated timeline.
      (remove
       nil
       (mapcar
	(lambda (buffer)
	  (let ((other-spec (twittering-get-timeline-spec-for-buffer buffer))
		(other-spec-string
		 (twittering-get-timeline-spec-string-for-buffer buffer)))
	    (when (twittering-timeline-spec-depending-on-p other-spec spec)
	      (let* ((twittering-new-tweets-spec other-spec)
		     (twittering-new-tweets-statuses
		      (twittering-generate-composite-timeline
		       other-spec spec new-statuses))
		     (twittering-new-tweets-count
		      (length twittering-new-tweets-statuses))
		     (rendered-tweets
		      (twittering-render-timeline
		       buffer twittering-new-tweets-statuses t)))
		(when rendered-tweets
		  (run-hooks 'twittering-new-tweets-hook)
		  `(,other-spec-string ,(length rendered-tweets)))))))
	(twittering-get-buffer-list))))))

;;;;
;;;; URIs related to a tweet
;;;;

(defun twittering-get-status-url (username &optional id)
  "Generate a URL of a user or a specific status."
  (let ((func
	 (cdr (assq
	       'status-url
	       (assq twittering-service-method
		     twittering-service-method-table)))))
    (funcall func username id)))

(defun twittering-get-status-url-from-alist (status)
  "Generate a URL of a tweet specified by an alist STATUS."
  (let ((username (cdr (assq 'user-screen-name status)))
	(id (cdr (assq 'id status)))
	(func
	 (cdr (assq
	       'status-url
	       (assq twittering-service-method
		     twittering-service-method-table)))))
    (funcall func username id)))

(defun twittering-get-list-url (username listname)
  "Generate a URL of a specific list."
  (let ((func
	 (cdr (assq
	       'status-url
	       (assq twittering-service-method
		     twittering-service-method-table))))
	(str (concat username "/" listname)))
    (funcall func str nil)))

(defun twittering-get-status-url-twitter (username &optional id)
  "Generate status URL for Twitter."
  (if id
      (format "http://%s/%s/status/%s" twittering-web-host username id)
    (format "http://%s/%s" twittering-web-host username)))

(defun twittering-get-status-url-statusnet (username &optional id)
  "Generate status URL for StatusNet."
  (if id
      (format "http://%s/%s/notice/%s" twittering-web-host twittering-web-path-prefix id)
    (format "http://%s/%s/%s" twittering-web-host twittering-web-path-prefix username)))

(defun twittering-get-search-url (query-string)
  "Generate a URL for searching QUERY-STRING."
  (let ((func (cdr (assq
		    'search-url (assq twittering-service-method
				      twittering-service-method-table)))))
    (funcall func query-string)))

(defun twittering-get-search-url-twitter (query-string)
  (format "http://%s/search?q=%s"
	  twittering-web-host (twittering-percent-encode query-string)))

(defun twittering-get-search-url-statusnet (query-string)
  (if (string-match "^#\\(.+\\)" query-string)
      (format "http://%s/%s/tag/%s"
	      twittering-web-host
	      twittering-web-path-prefix
	      (twittering-percent-encode (match-string 1 query-string)))
    (format "http://%s/search?q=%s"
	    twittering-web-host (twittering-percent-encode query-string))))

(defun twittering-extract-id-from-url (url-string)
  "Extract the ID from URL-STRING.
Return nil if URL-STRING cannot be interpreted as a URL pointing a tweet."
  (when (string-match
	 "\\`https?://twitter.com/\\(?:#!/\\)?[^/]+/status\\(?:es\\)?/\\([0-9]+\\)/?\\'"
	 url-string)
    (match-string 1 url-string)))

;;;;
;;;; Utility of status IDs
;;;;

(defun twittering-status-id< (id1 id2)
  (let ((len1 (length id1))
	(len2 (length id2)))
    (cond
     ((= len1 len2) (string< id1 id2))
     ((< len1 len2) t)
     (t nil))))

(defun twittering-status-id= (id1 id2)
  (equal id1 id2))

(defun twittering-snowflake-epoch-time ()
  "Return the epoch time of Snowflake."
  (require 'calc)
  (let ((epoch-str
	 ;; This corresponds to 2010-11-04 01:42:54+00:00 in RFC3339.
	 ;; The value comes from the following page.
	 ;; https://github.com/twitter/snowflake/blob/6d4634aa490de26e22425538291fe0a03071a170/src/main/scala/com/twitter/service/snowflake/IdWorker.scala#L22
	 ;; 22    val twepoch = 1288834974657L
	 "1288834974657"))
    (let ((str
	   (calc-eval `(,(concat "floor(10#" epoch-str "/10#1000)")
			calc-word-size 64 calc-number-radix 16)))
	  (milisec-str
	   (calc-eval `(,(concat "10#" epoch-str "%10#1000")
			calc-word-size 64 calc-number-radix 10))))
      (mapcar (lambda (s) (string-to-number s 16))
	      `(,(substring str 3 7) ,(substring str 7)
		,milisec-str)))))

(defun twittering-id-to-time (id)
  "Return the time corresonding to ID generated by Snowflake.
If ID is a string consisting of 12 or more digits, return the corresponding
time in Emacs internal representation the same as `encode-time'.
Otherwise, return nil.

This is because ID consisting of 11 or less digits may not be generated
by Snowflake."
  (require 'calc)
  (cond
   ((not (stringp id))
    nil)
   ((< (length id) 12)
    ;; The given ID may not be generated by Snowflake.
    ;;
    ;; The first tweet in the example response of
    ;; https://dev.twitter.com/docs/api/1/get/statuses/home_timeline
    ;; has "18700887835" as the ID.
    ;; Assuming that it is generated by Snowflake, it corresponds to
    ;; "2010-11-04 01:42:58+00:00".
    ;; However, the created_at is "Fri Jul 16 16:58:46 +0000 2010".
    ;; It can be confirmed at http://twitter.com/cindyli/status/18700887835 .
    ;;
    ;; Therefore we can not suppose that an ID is generated by Snowflake
    ;; if the ID consists of 11-digits.
    ;; Assuming that an ID reaches 10^11 before Snowflake is turned on at
    ;; Nov 04 2010, the average number of tweets per day will exceed
    ;; 533 million.
    ;; ( 10^11 - 18,700,887,835 > 80,000,000,000
    ;;   80 billion / 5 month (from Jul 16 to Nov 4) > 533 million / day )
    ;;
    ;; It is impossible.
    ;; So, I assume that an ID is generated by Snowflake if the ID consists of
    ;; 12 or more digits.
    ;; The first ID with 12-digits, 100,000,000,000 (10^11), corresponds to
    ;; 2010-11-04 01:43:17+00:00.
    nil)
   (t
    (let* ((epoch (twittering-snowflake-epoch-time))
	   (str (calc-eval
		 `(,(format "floor(rsh(10#%s,22)/1000)" id)
		   calc-word-size 64 calc-number-radix 16)))
	   (milisec
	    (string-to-number
	     (calc-eval
	      `(,(format "rsh(10#%s,22)%%1000" id)
		calc-word-size 64 calc-number-radix 10))
	     10)))
      (when (string-match "^16#\\([[:xdigit:]]+\\)" str)
	(let* ((hex-str (match-string 1 str))
	       (hex-str
		(if (< (length hex-str) 4)
		    (concat "0000" hex-str)
		  hex-str)))
	  (time-add epoch
		    `(,(string-to-number
			(substring hex-str 0 (- (length hex-str) 4)) 16)
		      ,(string-to-number
			(substring hex-str (- (length hex-str) 4)) 16)
		      ,(* milisec 1000)
		      ))))))))

(defun twittering-time-to-id (time)
  "Return the ID corresponding to TIME by Snowflake.
Bits other than timestamp are zero. The least significant 22 bits are zero.
TIME must be an Emacs internal representation as a return value of
`current-time'."
  (require 'calc)
  (let* ((epoch-time (twittering-snowflake-epoch-time))
	 (dt (if (time-less-p epoch-time time)
		 (time-subtract time epoch-time)
	       nil))
	 (sec-high (nth 0 dt))
	 (sec-low (nth 1 dt))
	 (microsec (or (nth 2 dt) "0")))
    (when dt
      (calc-eval
       `(,(format "lsh((16#%04x%04x) * 1000 + floor(%d/1000), 22)"
		  sec-high sec-low microsec)
	 calc-word-size 64 calc-number-radix 10)))))

;;;;
;;;; Process info
;;;;

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

;;;;
;;;; Server info
;;;;

(defun twittering-update-api-table (spec api-string)
  "Register a pair of a timeline spec and an API for retrieving the timeline.
SPEC is a timeline spec. API-STRING is an identifier of an API for retrieving
the timeline."
  (let ((current (assoc spec twittering-timeline-spec-to-api-table)))
    (if (null current)
	(add-to-list 'twittering-timeline-spec-to-api-table
		     `(,spec . ,api-string))
      (setcdr current api-string))))

(defun twittering-make-rate-limit-alist (header-info)
  "Make a rate-limit information alist from HEADER-INFO.
Key symbols of a returned alist are following; limit, remaining, reset-time.
Values bound to limit and remaining is a positive integer and
one bound to reset-time is an Emacs time (result of `seconds-to-time')."
  (let ((symbol-table
	 '(("X-Rate-Limit-Limit" . limit)
	   ("X-Rate-Limit-Remaining" . remaining)
	   ("X-Rate-Limit-Reset" . reset-time)
	   ;; For Twitter API v1.0.
	   ("X-RateLimit-Limit" . limit)
	   ("X-RateLimit-Remaining" . remaining)
	   ("X-RateLimit-Reset" . reset-time))))
    (remove
     nil
     (mapcar (lambda (entry)
	       (let ((sym
		      (cdr
		       (twittering-assoc-string (car entry) symbol-table t))))
		 (cond
		  ((memq sym '(limit remaining))
		   `(,sym . ,(string-to-number (cdr entry))))
		  ((eq sym 'reset-time)
		   `(,sym
		     . ,(seconds-to-time (string-to-number (cdr entry)))))
		  (t
		   nil))))
	     header-info))))

(defun twittering-update-rate-limit-info (api-string spec header-info)
  "Register rate-limit information.
API-STRING is an identifier of an API. SPEC is a timeline spec that had been
retrieved by the API. HEADER-INFO is an alist generated from the HTTP response
header of the API."
  (let* ((api-string
	  (if (eq twittering-service-method 'twitter)
	      ;; The key for Twitter API v1.0 is nil.
	      nil
	    api-string))
	 (current (assoc api-string twittering-api-limit-info-alist))
	 (rate-limit-alist (twittering-make-rate-limit-alist header-info)))
    (twittering-update-api-table spec api-string)
    (if (null current)
	(add-to-list 'twittering-api-limit-info-alist
		     `(,api-string . ,rate-limit-alist))
      (setcdr current rate-limit-alist))))

(defun twittering-update-server-info (connection-info header-info)
  (let* ((new-entry-list (mapcar 'car header-info))
	 (account-info (cdr (assq 'account-info connection-info)))
	 (account
	  (twittering-get-from-account-info "screen_name" account-info))
	 (spec (cdr (assq 'timeline-spec connection-info)))
	 (api-string
	  (cdr (assq 'uri-without-query (assq 'request connection-info)))))
    (twittering-update-rate-limit-info api-string spec header-info)
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
    ;; cookie
    (let* ((new-cookies
	    (twittering-extract-cookie connection-info header-info))
	   (old-cookies (cdr (assoc account twittering-cookie-alist)))
	   (updated-cookies
	    (append new-cookies
		    (remove nil
			    (mapcar (lambda (cookie)
				      (unless (assoc (car cookie) new-cookies)
					cookie))
				    old-cookies)))))
      (setq twittering-cookie-alist
	    (cons (cons account updated-cookies)
		  (remove nil
			  (mapcar (lambda (entry)
				    (unless (equal account (car entry))
				      entry))
				  twittering-cookie-alist)))))
    header-info))

(defun twittering-extract-cookie (connection-info header-info)
  (remove
   nil
   (mapcar
    (lambda (entry)
      (let ((header-item (car entry))
	    (header-value (cdr entry)))
	(when (and (string= header-item "Set-Cookie")
		   (string-match "\\([^= ]*\\) *= *\\([^; ]*\\) *;? *"
				 header-value))
	  ;; For ease of implementation, the followings are assumed.
	  ;; 1. Each response header includes only one cookie.
	  ;; 2. `value' of cookie is a token, not a quoted string.
	  ;; 3. Attributes except `domain', `expires' and `path' are ignored.
	  (let* ((name (downcase (match-string 1 header-value)))
		 (value (match-string 2 header-value))
		 (attributes
		  (mapcar
		   (lambda (str)
		     (when (string-match "\\` *\\([^ ]*\\) *= *\\(.*\\)\\'"
					 str)
		       (let ((attr (downcase (match-string 1 str)))
			     (value (match-string 2 str)))
			 (cond
			  ((string= attr "domain")
			   `(domain . ,value))
			  ((string= attr "expires")
			   `(expires
			     . ,(apply 'encode-time
				       (parse-time-string
					(replace-regexp-in-string
					 "-" " " value)))))
			  ((string= attr "path")
			   `(path . ,value))
			  (t
			   nil)))))
		   (split-string (substring header-value (match-end 0))
				 " *; *")))
		 (additional-attributes
		  `(,@(let* ((domain (cdr (assq 'domain attributes)))
			     (request (cdr (assq 'request connection-info)))
			     (host (cdr (assq 'host request)))
			     (prefix
			      (if domain
				  (regexp-quote domain)
				(concat "\\`" (regexp-quote host)))))
			`((domain-regexp . ,(concat prefix "\\'")))))))
	    `(,name
	      (value . ,value)
	      ,@attributes
	      ,@additional-attributes)))))
    header-info)))

(defun twittering-make-cookie-string (request account-info)
  (let ((account
	 (twittering-get-from-account-info "screen_name" account-info))
	(current-time (current-time))
	(host (cdr (assq 'host request))))
    (when account
      (mapconcat
       'identity
       (remove nil
	       (mapcar
		(lambda (entry)
		  (let* ((expires (cdr (assq 'expires entry)))
			 (not-expired (or (null expires)
					  (time-less-p current-time expires)))
			 (domain-regexp (cdr (assq 'domain-regexp entry))))
		    (when (and not-expired
			       (string-match domain-regexp host))
		      (format "%s=%s" (car entry) (cdr (assq 'value entry))))))
		(cdr (assoc account twittering-cookie-alist))))
       ";"))))

(defun twittering-get-ratelimit-alist (&optional spec)
  (let ((api-string
	 (cdr (assoc spec twittering-timeline-spec-to-api-table))))
    (cdr (assoc api-string twittering-api-limit-info-alist))))

(defun twittering-get-ratelimit-remaining (&optional spec)
  (or (cdr (assq 'remaining (twittering-get-ratelimit-alist spec)))
      0))

(defun twittering-get-ratelimit-limit (&optional spec)
  (or (cdr (assq 'limit (twittering-get-ratelimit-alist spec)))
      0))

(defun twittering-get-ratelimit-indicator-string (&optional spec)
  "Make an indicator string of rate-limit information of SPEC."
  (cond
   ((eq twittering-service-method 'twitter)
    ;; Twitter API v1.0.
    (format "%d/%d"
	    (twittering-get-ratelimit-remaining)
	    (twittering-get-ratelimit-limit)))
   (t
    (mapconcat
     (lambda (api-string)
       (let* ((alist (cdr (assoc api-string twittering-api-limit-info-alist)))
	      (remaining (cdr (assq 'remaining alist)))
	      (limit (cdr (assq 'limit alist))))
	 (format "%s/%s"
		 (if remaining (number-to-string remaining) "?")
		 (if limit (number-to-string limit) "?"))))
     (twittering-remove-duplicates
      (mapcar (lambda (spec)
		(cdr (assoc spec twittering-timeline-spec-to-api-table)))
	      (twittering-get-primary-base-timeline-specs spec)))
     "+"))))

;;;;
;;;; Abstract layer for Twitter API
;;;;

(defun twittering-api-path (&rest params)
  (mapconcat 'identity `(,twittering-api-prefix ,@params) ""))

(defun twittering-call-api (command args-alist &optional additional-info)
  "Call Twitter API and return the process object for the request.
Invoke `twittering-call-api-with-account' with the main account specified
by `twittering-get-main-account-info'.
For details of arguments, see `twittering-call-api-with-account'."
  (let ((account-info-alist (twittering-get-main-account-info)))
    (twittering-call-api-with-account account-info-alist command args-alist
				      additional-info)))

(defun twittering-call-api-with-account (account-info-alist command args-alist &optional additional-info)
  "Call Twitter API and return the process object for the request.
COMMAND is a symbol specifying API. ARGS-ALIST is an alist specifying
arguments for the API corresponding to COMMAND. Each key of ARGS-ALIST is a
symbol.
ACCOUNT-INFO-ALIST is an alist storing account information, which has
the following key;
\"screen_name\", \"oauth_token\" and \"oauth_token_secret\" for OAuth/xAuth,
\"screen_name\" and \"password\" for basic authentication.
ADDITIONAL-INFO is used as an argument ADDITIONAL-INFO of
`twittering-send-http-request'. Sentinels associated to the returned process
receives it as the fourth argument. See also the function
`twittering-send-http-request'.

The valid symbols as COMMAND follows:
retrieve-timeline -- Retrieve a timeline.
  Valid key symbols in ARGS-ALIST:
    timeline-spec -- the timeline spec to be retrieved.
    timeline-spec-string -- the string representation of the timeline spec.
    format -- (optional) the symbol specifying the format.
    number -- (optional) how many tweets are retrieved. It must be an integer.
      If nil, `twittering-number-of-tweets-on-retrieval' is used instead.
      The maximum for search timeline is 100, and that for other timelines is
      `twittering-max-number-of-tweets-on-retrieval'.
      If the given number exceeds the maximum, the maximum is used instead.
    max_id -- (optional) the maximum ID of retrieved tweets.
    since_id -- (optional) the minimum ID of retrieved tweets.
    sentinel -- (optional) the sentinel that processes the buffer consisting
      of retrieved data.. This is used as an argument SENTINEL of
      `twittering-send-http-request' via `twittering-http-get'.
      If nil, `twittering-http-get-default-sentinel' is used.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request' via
      `twittering-http-get'.
    page -- (optional and valid only for favorites timeline) which page will
      be retrieved.
retrieve-single-tweet -- Retrieve a single tweet.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the tweet to be retrieved.
    username -- (optional) the screen name of the author of the tweet.
    format -- (optional) the symbol specifying the format.
    sentinel -- (optional) the sentinel that processes the buffer consisting
      of retrieved data.. This is used as an argument SENTINEL of
      `twittering-send-http-request' via `twittering-http-get'.
      If nil, `twittering-http-get-default-sentinel' is used.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request' via
      `twittering-http-get'.
get-list-index -- Retrieve list names owned by a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username.
    sentinel -- the sentinel that processes retrieved strings. This is used
      as an argument SENTINEL of `twittering-send-http-request'
      via `twittering-http-get'.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request' via
      `twittering-http-get'.
get-list-subscriptions -- Retrieve list names followed by a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username.
    sentinel -- the sentinel that processes retrieved strings. This is used
      as an argument SENTINEL of `twittering-send-http-request'
      via `twittering-http-get'.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request' via
      `twittering-http-get'.
create-friendships -- Follow a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username which will be followed.
destroy-friendships -- Unfollow a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username which will be unfollowed.
create-favorites -- Mark a tweet as a favorite.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
destroy-favorites -- Remove a mark of a tweet as a favorite.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
update-status -- Post a tweet.
  Valid key symbols in ARGS-ALIST:
    status -- the string to be posted.
    in-reply-to-status-id -- (optional) the ID of a status that this post is
      in reply to.
destroy-status -- Destroy a tweet posted by the authenticated user itself.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
retweet -- Retweet a tweet.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
verify-credentials -- Verify the current credentials.
  Valid key symbols in ARGS-ALIST:
    sentinel -- the sentinel that processes returned information. This is used
      as an argument SENTINEL of `twittering-send-http-request'
      via `twittering-http-get'.
    clean-up-sentinel -- the clean-up sentinel that post-processes the buffer
      associated to the process. This is used as an argument CLEAN-UP-SENTINEL
      of `twittering-send-http-request' via `twittering-http-get'.
send-direct-message -- Send a direct message.
  Valid key symbols in ARGS-ALIST:
    username -- the username who the message is sent to.
    status -- the sent message.
block -- Block a user.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be blocked.
    username -- the username who will be blocked.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API.
block-and-report-as-spammer -- Block a user and report him or her as a spammer.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be blocked.
    username -- the username who will be blocked.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API.
get-service-configuration -- Get the configuration of the server.
  Valid key symbols in ARGS-ALIST:
    sentinel -- the sentinel that processes retrieved strings. This is used
      as an argument SENTINEL of `twittering-send-http-request'.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request'."
  (let* ((additional-info
	  `(,@additional-info
	    (service-method . ,twittering-service-method))))
    (cond
     ((memq twittering-service-method '(twitter statusnet))
      (twittering-call-api-with-account-in-api1.0
       account-info-alist command args-alist additional-info))
     ((eq twittering-service-method 'twitter-api-v1.1)
      (cond
       ((not (require 'json nil t))
	(error "`json.el' is required to use the Twitter REST API v1.1")
	nil)
       ((not twittering-use-ssl)
	(error "SSL is required to use the Twitter REST API v1.1")
	nil)
       (t
	(twittering-call-api-with-account-in-api1.1
	 account-info-alist command args-alist additional-info))))
     (t
      (error "`twittering-service-method' is an invalid service method")
      ))))

(defun twittering-call-api-with-account-in-api1.0 (account-info-alist command args-alist &optional additional-info)
  "Call the Twitter REST API v1.0 and return the process object for the request."
  (cond
   ((eq command 'retrieve-timeline)
    ;; Retrieve a timeline.
    (let* ((spec (cdr (assq 'timeline-spec args-alist)))
	   (spec-string (cdr (assq 'timeline-spec-string args-alist)))
	   (spec-type (car-safe spec))
	   (max-number (if (eq 'search spec-type)
			   100 ;; FIXME: refer to defconst.
			 twittering-max-number-of-tweets-on-retrieval))
	   (number
	    (let ((number
		   (or (cdr (assq 'number args-alist))
		       (let* ((default-number 20)
			      (n twittering-number-of-tweets-on-retrieval))
			 (cond
			  ((integerp n) n)
			  ((string-match "^[0-9]+$" n) (string-to-number n 10))
			  (t default-number))))))
	      (min (max 1 number) max-number)))
	   (number-str (number-to-string number))
	   (max_id (cdr (assq 'max_id args-alist)))
	   (page (cdr (assq 'page args-alist)))
	   (since_id (cdr (assq 'since_id args-alist)))
	   (word (when (eq 'search spec-type)
		   (cadr spec)))
	   (parameters
	    (cond
	     ((eq spec-type 'favorites)
	      `(("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when page `(("page" . ,page)))))
	     ((eq spec-type 'retweeted_by_user)
	      (let ((username (elt spec 1)))
		`(("count" . ,number-str)
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("include_entities" . "true")
		  ("screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id))))))
	     ((eq spec-type 'retweeted_to_user)
	      (let ((username (elt spec 1)))
		`(("count" . ,number-str)
		  ("include_entities" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id))))))
	     (t
	      `(,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))
		,@(cond
		   ((eq spec-type 'search)
		    `(("include_entities" . "true")
		      ("q" . ,word)
		      ("rpp" . ,number-str)
		      ("with_twitter_user_id". "true")))
		   ((eq spec-type 'list)
		    (let ((username (elt spec 1))
			  (list-name (elt spec 2)))
		      `(("include_entities" . "true")
			("include_rts" . "true")
			("owner_screen_name" . ,username)
			("per_page" . ,number-str)
			("slug" . ,list-name))))
		   ((eq spec-type 'user)
		    (let ((username (elt spec 1)))
		      `(("count" . ,number-str)
			("include_entities" . "true")
			("include_rts" . "true")
			("screen_name" . ,username))))
		   ((memq spec-type '(friends mentions public))
		    `(("include_entities" . "true")
		      ("count" . ,number-str)
		      ("include_rts" . "true")))
		   (t
		    ;; direct_messages
		    ;; direct_messages_sent
		    ;; home
		    ;; replies
		    ;; retweeted_by_me
		    ;; retweeted_to_me
		    ;; retweets_of_me
		    `(("include_entities" . "true")
		      ("count" . ,number-str))))))))
	   (format
	    (let ((format (cdr (assq 'format args-alist))))
	      (cond
	       ((and format (symbolp format))
		format)
	       ((eq spec-type 'search)
		'atom)
	       (t
		'xml))))
	   (format-str (symbol-name format))
	   (simple-spec-list
	    '((direct_messages . "direct_messages")
	      (direct_messages_sent . "direct_messages/sent")
	      (friends . "statuses/friends_timeline")
	      (home . "statuses/home_timeline")
	      (mentions . "statuses/mentions")
	      (public . "statuses/public_timeline")
	      (replies . "statuses/replies")
	      (retweeted_by_me . "statuses/retweeted_by_me")
	      (retweeted_to_me . "statuses/retweeted_to_me")
	      (retweets_of_me . "statuses/retweets_of_me")
	      (user . "statuses/user_timeline")))
	   (host (cond ((eq spec-type 'search) twittering-api-search-host)
		       (t twittering-api-host)))
	   (method
	    (cond
	     ((eq spec-type 'list)
	      (twittering-api-path "lists/statuses"))
	     ((eq spec-type 'favorites)
	      (let ((user (elt spec 1)))
		(if user
		    (twittering-api-path "favorites/" user)
		  (twittering-api-path "favorites"))))
	     ((eq spec-type 'retweeted_by_user)
	      (twittering-api-path "statuses/retweeted_by_user"))
	     ((eq spec-type 'retweeted_to_user)
	      (twittering-api-path "statuses/retweeted_to_user"))
	     ((eq spec-type 'search)
	      twittering-search-api-method)
	     ((assq spec-type simple-spec-list)
	      (twittering-api-path (cdr (assq spec-type simple-spec-list))))
	     (t nil)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info (format . ,format))))
      (cond
       ((eq spec-type 'single)
	(let ((id (cadr spec))
	      (sentinel (or sentinel
			    'twittering-retrieve-single-tweet-sentinel)))
	  (if (twittering-find-status id)
	      ;; If the status has already retrieved, do nothing.
	      nil
	    (twittering-call-api 'retrieve-single-tweet
				 `((id . ,id)
				   (format . ,format)
				   (sentinel . ,sentinel)
				   (clean-up-sentinel . ,clean-up-sentinel))
				 additional-info))))
       ((and host method)
	(twittering-http-get account-info-alist host method parameters
			     format-str
			     additional-info sentinel clean-up-sentinel))
       (t
	(error "Invalid timeline spec")))))
   ((eq command 'retrieve-single-tweet)
    (let* ((id (cdr (assq 'id args-alist)))
	   (user-screen-name (cdr (assq 'username args-alist)))
	   (format
	    (let ((format (cdr (assq 'format args-alist))))
	      (cond
	       ((and format (symbolp format))
		format)
	       (t
		'xml))))
	   (format-str (symbol-name format))
	   (parameters '(("include_entities" . "true")))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info
			      (id . ,id)
			      (user-screen-name . ,user-screen-name)
			      (format . ,format))))
      (twittering-http-get account-info-alist twittering-api-host
			   (twittering-api-path "statuses/show/" id)
			   parameters format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'get-list-index)
    ;; Get list names.
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist twittering-api-host
			   (twittering-api-path username "/lists")
			   nil format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'get-list-subscriptions)
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist twittering-api-host
			   (twittering-api-path username "/lists/subscriptions")
			   nil format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'create-friendships)
    ;; Create a friendship.
    (let ((username (cdr (assq 'username args-alist))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "friendships/create")
			    `(("screen_name" . ,username))
			    nil additional-info)))
   ((eq command 'destroy-friendships)
    ;; Destroy a friendship
    (let ((username (cdr (assq 'username args-alist))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "friendships/destroy")
			    `(("screen_name" . ,username))
			    nil additional-info)))
   ((eq command 'create-favorites)
    ;; Create a favorite.
    (let ((id (cdr (assq 'id args-alist))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "favorites/create/" id)
			    nil nil additional-info)))
   ((eq command 'destroy-favorites)
    ;; Destroy a favorite.
    (let ((id (cdr (assq 'id args-alist))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "favorites/destroy/" id)
			    nil nil additional-info)))
   ((eq command 'update-status)
    ;; Post a tweet.
    (let* ((status (cdr (assq 'status args-alist)))
	   (id (cdr (assq 'in-reply-to-status-id args-alist)))
	   (parameters
	    `(("status" . ,status)
	      ,@(when (eq twittering-auth-method 'basic)
		  '(("source" . "twmode")))
	      ,@(when id `(("in_reply_to_status_id" . ,id))))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "statuses/update")
			    parameters nil additional-info)))
   ((eq command 'destroy-status)
    ;; Destroy a status.
    (let* ((id (cdr (assq 'id args-alist)))
	   (format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format)))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "statuses/destroy/" id)
			    nil format-str additional-info
			    'twittering-http-post-destroy-status-sentinel)))
   ((eq command 'retweet)
    ;; Post a retweet.
    (let ((id (cdr (assq 'id args-alist))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "statuses/retweet/" id)
			    nil nil additional-info)))
   ((eq command 'verify-credentials)
    ;; Verify the account.
    (let ((sentinel (cdr (assq 'sentinel args-alist)))
	  (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist twittering-api-host
			   (twittering-api-path "account/verify_credentials")
			   nil nil additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'send-direct-message)
    ;; Send a direct message.
    (let ((parameters
	   `(("screen_name" . ,(cdr (assq 'username args-alist)))
	     ("text" . ,(cdr (assq 'status args-alist))))))
      (twittering-http-post account-info-alist twittering-api-host
			    (twittering-api-path "direct_messages/new")
			    parameters nil additional-info)))
   ((eq command 'block)
    ;; Block a user.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (parameters (if user-id
			   `(("user_id" . ,user-id))
			 `(("screen_name" . ,username)))))
      (twittering-http-post twittering-api-host
			    (twittering-api-path "blocks/create")
			    parameters nil additional-info)))
   ((eq command 'block-and-report-as-spammer)
    ;; Report a user as a spammer and block him or her.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (parameters (if user-id
			   `(("user_id" . ,user-id))
			 `(("screen_name" . ,username)))))
      (twittering-http-post twittering-api-host
			    (twittering-api-path "report_spam")
			    parameters nil additional-info)))
   ((eq command 'get-service-configuration)
    (let* ((format (if (require 'json nil t) 'json 'xml))
	   (format-str (symbol-name format))
	   (request
	    (twittering-make-http-request-from-uri
	     "GET" nil
	     (concat (if twittering-use-ssl
			 "https"
		       "http")
		     "://" twittering-api-host
		     "/"
		     (twittering-api-path "help/configuration." format-str))))
	   (additional-info nil)
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-send-http-request request additional-info
				    sentinel clean-up-sentinel)))
   (t
    nil)))

(defun twittering-call-api-with-account-in-api1.1 (account-info-alist command args-alist &optional additional-info)
  "Call the Twitter REST API v1.1 and return the process object for the request."
  (cond
   ((eq command 'retrieve-timeline)
    ;; Retrieve a timeline.
    (let* ((args-alist
	    (let* ((spec (cdr (assq 'timeline-spec args-alist)))
		   (spec-type (car-safe spec))
		   (table '((friends . (home))
			    (replies . (mentions)))))
	      (cond
	       ((memq spec-type '(friends replies))
		(let* ((alternative (cdr (assq spec-type table)))
		       (alternative-str
			(twittering-timeline-spec-to-string alternative)))
		  (message
		   "Timeline spec %s is not supported in the Twitter REST API v1.1"
		   spec)
		  `((timeline-spec . ,alternative)
		    (timeline-spec-string . ,alternative-str)
		    ,@args-alist)))
	       (t
		args-alist))))
	   (spec (cdr (assq 'timeline-spec args-alist)))
	   (spec-string (cdr (assq 'timeline-spec-string args-alist)))
	   (spec-type (car-safe spec))
	   (max-number (if (eq 'search spec-type)
			   100 ;; FIXME: refer to defconst.
			 twittering-max-number-of-tweets-on-retrieval))
	   (number
	    (let ((number
		   (or (cdr (assq 'number args-alist))
		       (let* ((default-number 20)
			      (n twittering-number-of-tweets-on-retrieval))
			 (cond
			  ((integerp n) n)
			  ((string-match "^[0-9]+$" n) (string-to-number n 10))
			  (t default-number))))))
	      (min (max 1 number) max-number)))
	   (number-str (number-to-string number))
	   (max_id (cdr (assq 'max_id args-alist)))
	   (since_id (cdr (assq 'since_id args-alist)))
	   (word (when (eq 'search spec-type)
		   (cadr spec)))
	   (parameters
	    (cond
	     ((eq spec-type 'user)
	      (let ((username (elt spec 1)))
		`("api.twitter.com"
		  "1.1/statuses/user_timeline"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ("include_rts" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id)))
		  )))
	     ((eq spec-type 'list)
	      (let ((username (elt spec 1))
		    (list-name (elt spec 2)))
		`("api.twitter.com"
		  "1.1/lists/statuses"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ("include_rts" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("owner_screen_name" . ,username)
		  ,@(when since_id `(("since_id" . ,since_id)))
		  ("slug" . ,list-name))))
	     ((eq spec-type 'direct_messages)
	      `("api.twitter.com"
		"1.1/direct_messages"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))))
	     ((eq spec-type 'direct_messages_sent)
	      `("api.twitter.com"
		"1.1/direct_messages/sent"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))))
	     ((eq spec-type 'favorites)
	      (let ((user (elt spec 1)))
		`("api.twitter.com"
		  "1.1/favorites/list"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ,@(when user `(("screen_name" . ,user)))
		  ,@(when since_id `(("since_id" . ,since_id))))))
	     ((eq spec-type 'home)
	      `("api.twitter.com"
		"1.1/statuses/home_timeline"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))))
	     ((eq spec-type 'mentions)
	      `("api.twitter.com"
		"1.1/statuses/mentions_timeline"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))))
	     ((eq spec-type 'public)
	      (error
	       "Timeline spec %s is not supported in the Twitter REST API v1.1"
	       spec)
	      nil)
	     ((memq spec-type '(retweeted_by_me
				retweeted_by_user
				retweeted_to_me
				retweeted_to_user))
	      (error
	       "Timeline spec %s is not supported in the Twitter REST API v1.1"
	       spec)
	      nil)
	     ((eq spec-type 'retweets_of_me)
	      `("api.twitter.com"
		"1.1/statuses/retweets_of_me"
		("count" . ,number-str)
		("include_entities" . "true")
		,@(when max_id `(("max_id" . ,max_id)))
		,@(when since_id `(("since_id" . ,since_id)))))
	     ((eq spec-type 'single)
	      (let ((id (elt spec 1)))
		`("api.twitter.com"
		  "1.1/statuses/show"
		  ("id" . ,id)
		  ("include_entities" . "true"))))
	     ((eq spec-type 'search)
	      (let ((word (elt spec 1)))
		`("api.twitter.com"
		  "1.1/search/tweets"
		  ("count" . ,number-str)
		  ("include_entities" . "true")
		  ,@(when max_id `(("max_id" . ,max_id)))
		  ("q" . ,word)
		  ("result_type" . "mixed")
		  ,@(when since_id `(("since_id" . ,since_id))))))
	     (t
	      (error
	       "Timeline spec %s is unknown"
	       spec-string)
	      nil)))
	   (format 'json)
	   (format-str (symbol-name format))
	   (host (elt parameters 0))
	   (method (elt parameters 1))
	   (http-parameters (nthcdr 2 parameters))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info (format . ,format)))
	   ;; special treatment for single timeline.
	   (id (cdr (assoc "id" http-parameters)))
	   (sentinel (or sentinel
			 (when (eq spec-type 'single)
			   'twittering-retrieve-single-tweet-sentinel))))
      (cond
       ((null parameters)
	nil)
       ((not (and (string= format-str "json")
		  (require 'json nil t)))
	(error "The Twitter REST API v1.1 supports only JSON")
	nil)
       ((and (eq spec-type 'single)
	     (twittering-find-status id))
	;; If the status has already retrieved, do nothing.
	nil)
       ((and host method)
	(twittering-http-get account-info-alist host method http-parameters
			     format-str
			     additional-info sentinel clean-up-sentinel))
       (t
	(error "Invalid timeline spec")
	nil))))
   ((eq command 'retrieve-single-tweet)
    (let* ((id (cdr (assq 'id args-alist)))
	   (user-screen-name (cdr (assq 'username args-alist)))
	   (format
	    (let ((format (cdr (assq 'format args-alist))))
	      (cond
	       ((and format (symbolp format))
		format)
	       (t
		'json))))
	   (format-str (symbol-name format))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist)))
	   (additional-info `(,@additional-info
			      (id . ,id)
			      (user-screen-name . ,user-screen-name)
			      (format . ,format))))
      (twittering-call-api-with-account-in-api1.1
       account-info-alist 'retrieve-timeline
       `((timeline-spec . (single ,id))
	 (format . ,format)
	 (sentinel . ,sentinel)
	 (clean-up-sentinel . ,clean-up-sentinel))
       additional-info)))
   ((eq command 'get-list-index)
    ;; Get list names.
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/lists/list")
	   (http-parameters `(("screen_name" . ,username)))
	   (format-str "json")
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'get-list-subscriptions)
    (let* ((username (cdr (assq 'username args-alist)))
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/lists/subscriptions")
	   (http-parameters
	    `(("count" . "20")
	      ("screen_name" . ,username)))
	   (format-str "json")
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'create-friendships)
    ;; Create a friendship.
    (let* ((username (cdr (assq 'username args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/friendships/create")
	   (http-parameters
	    `(("screen_name" . ,username)))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'destroy-friendships)
    ;; Destroy a friendship
    (let* ((username (cdr (assq 'username args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/friendships/destroy")
	   (http-parameters
	    `(("screen_name" . ,username)))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'create-favorites)
    ;; Create a favorite.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/favorites/create")
	   (http-parameters `(("id" . ,id)))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'destroy-favorites)
    ;; Destroy a favorite.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/favorites/destroy")
	   (http-parameters `(("id" . ,id)))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'update-status)
    ;; Post a tweet.
    (let* ((status (cdr (assq 'status args-alist)))
	   (id (cdr (assq 'in-reply-to-status-id args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/statuses/update")
	   (http-parameters
	    `(("status" . ,status)
	      ,@(when id `(("in_reply_to_status_id" . ,id)))))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'destroy-status)
    ;; Destroy a status.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host "api.twitter.com")
	   (method (format "1.1/statuses/destroy/%s" id))
	   (http-parameters nil)
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info
			    'twittering-http-post-destroy-status-sentinel)))
   ((eq command 'retweet)
    ;; Post a retweet.
    (let* ((id (cdr (assq 'id args-alist)))
	   (host "api.twitter.com")
	   (method (format "1.1/statuses/retweet/%s" id))
	   (http-parameters nil)
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'verify-credentials)
    ;; Verify the account.
    (let* ((host "api.twitter.com")
	   (method "1.1/account/verify_credentials")
	   (http-parameters nil)
	   (format-str "json")
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   ((eq command 'send-direct-message)
    ;; Send a direct message.
    (let* ((host "api.twitter.com")
	   (method "1.1/direct_messages/new")
	   (http-parameters
	    `(("screen_name" . ,(cdr (assq 'username args-alist)))
	      ("text" . ,(cdr (assq 'status args-alist)))))
	   (format-str "json"))
      (twittering-http-post account-info-alist host method http-parameters
			    format-str additional-info)))
   ((eq command 'block)
    ;; Block a user.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/blocks/create")
	   (http-parameters (if user-id
				`(("user_id" . ,user-id))
			      `(("screen_name" . ,username))))
	   (format-str "json"))
      (twittering-http-post host method http-parameters
			    format-str additional-info)))
   ((eq command 'block-and-report-as-spammer)
    ;; Report a user as a spammer and block him or her.
    (let* ((user-id (cdr (assq 'user-id args-alist)))
	   (username (cdr (assq 'username args-alist)))
	   (host "api.twitter.com")
	   (method "1.1/users/report_spam")
	   (http-parameters (if user-id
				`(("user_id" . ,user-id))
			      `(("screen_name" . ,username))))
	   (format-str "json"))
      (twittering-http-post host method http-parameters
			    format-str additional-info)))
   ((eq command 'get-service-configuration)
    (let* ((host "api.twitter.com")
	   (method "1.1/help/configuration")
	   (http-parameters nil)
	   (format-str "json")
	   (additional-info nil)
	   (sentinel (cdr (assq 'sentinel args-alist)))
	   (clean-up-sentinel (cdr (assq 'clean-up-sentinel args-alist))))
      (twittering-http-get account-info-alist host method http-parameters
			   format-str additional-info
			   sentinel clean-up-sentinel)))
   (t
    nil)))

;;;;
;;;; Service configuration
;;;;

(defconst twittering-service-configuration-default
  '((short_url_length . 19)
    (short_url_length_https . 20))
  "Default value of `twittering-service-configuration'.")
(defvar twittering-service-configuration nil
  "Current server configuration.")
(defvar twittering-service-configuration-queried nil)
(defvar twittering-service-configuration-update-interval 86400
  "*Interval of updating `twittering-service-configuration'.")

(defun twittering-get-service-configuration (entry)
  (let ((pair (assq entry twittering-service-configuration)))
    (if (null pair)
	(cdr (assq entry twittering-service-configuration-default))
      (cdr pair))))

(defun twittering-update-service-configuration (&optional ignore-time)
  "Update `twittering-service-configuration' if necessary."
  (when (and
	 (memq twittering-service-method '(twitter twitter-api-v1.1))
	 (null twittering-service-configuration-queried)
	 (or ignore-time
	     (let ((current (twittering-get-service-configuration 'time))
		   (interval
		    (seconds-to-time
		     twittering-service-configuration-update-interval)))
	       (if (null current)
		   t
		 ;; If time passed more than `interval',
		 ;; update the configuration.
		 (time-less-p interval (time-since current))))))
    (setq twittering-service-configuration-queried t)
    (twittering-call-api
     'get-service-configuration
     '((sentinel . twittering-update-service-configuration-sentinel)
       (clean-up-sentinel
	. twittering-update-service-configuration-clean-up-sentinel)))))

(defun twittering-update-service-configuration-sentinel (proc status connection-info header-info)
  (let ((status-line (cdr (assq 'status-line header-info)))
	(status-code (cdr (assq 'status-code header-info)))
	(format
	 (twittering-get-content-subtype-symbol-from-header-info header-info)))
    (case-string
     status-code
     (("200")
      (let* ((conf-alist
	      (cond
	       ((eq format 'xml)
		(let ((xml
		       (twittering-xml-parse-region (point-min) (point-max))))
		  (mapcar
		   (lambda (entry)
		     `(,(car entry) . ,(elt entry 2)))
		   (cddr (assq 'configuration xml)))))
	       ((eq format 'json)
		(twittering-json-read))
	       (t
		(error "Format \"%s\" is not supported" format)
		nil)))
	     (entries '(short_url_length short_url_length_https)))
	(setq twittering-service-configuration
	      `((time . ,(current-time))
		,@(mapcar (lambda (entry)
			    (let ((value (cdr (assq entry conf-alist))))
			      (cons
			       entry
			       (cond
				((stringp value)
				 (string-to-number value))
				(t
				 value)))))
			  entries)))
	(setq twittering-service-configuration-queried nil)
	nil))
     (("400")
      ;; Rate limit exceeded.
      (setq twittering-service-configuration-queried nil)
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info)))
     (t
      (setq twittering-service-configuration-queried nil)
      (format "Response: %s"
	      (twittering-get-error-message header-info connection-info))))))

(defun twittering-update-service-configuration-clean-up-sentinel (proc status connection-info)
  (when (not (twittering-process-alive-p proc))
    (setq twittering-service-configuration-queried nil)))

;;;;
;;;; Account authorization
;;;;

(defun twittering-register-account-info (account-info)
  (setq twittering-oauth-access-token-alist account-info))

(defun twittering-get-main-account-info ()
  (cond
   ((eq twittering-auth-method 'basic)
    `(("screen_name" . ,twittering-username)
      ("password" . ,twittering-password)))
   ((memq twittering-auth-method '(oauth xauth))
    twittering-oauth-access-token-alist)))

(defun twittering-get-from-account-info (param account-info)
  (cdr (assoc param account-info)))

(defun twittering-get-username ()
  (let ((account-info (twittering-get-main-account-info)))
    (twittering-get-from-account-info "screen_name" account-info)))

(defun twittering-get-password ()
  (let ((account-info (twittering-get-main-account-info)))
    (twittering-get-from-account-info "password" account-info)))

(defun twittering-make-basic-authentication-string (account-info)
  (concat "Basic "
	  (base64-encode-string
	   (concat (cdr (assoc "screen_name" account-info))
		   ":" (cdr (assoc "password" account-info))))))

(defun twittering-make-oauth-authentication-string (account-info request)
  (let ((method (cdr (assq 'method request)))
	(access-token (cdr (assoc "oauth_token" account-info)))
	(access-token-secret (cdr (assoc "oauth_token_secret" account-info))))
    (unless (and (stringp access-token)
		 (stringp access-token-secret))
      (error "`account-info' has no valid OAuth token"))
    (twittering-oauth-auth-str-access
     method
     (cdr (assq 'uri-without-query request))
     (cdr (assq 'encoded-query-alist request))
     twittering-oauth-consumer-key twittering-oauth-consumer-secret
     access-token access-token-secret)))

(defun twittering-account-authorized-p ()
  (eq twittering-account-authorization 'authorized))
(defun twittering-account-authorization-queried-p ()
  (eq twittering-account-authorization 'queried))

(defun twittering-prepare-account-info ()
  "Return a pair of username and password.
If `twittering-username' is nil, read it from the minibuffer.
If `twittering-password' is nil, read it from the minibuffer."
  (let* ((username (or twittering-username
		       (read-string "your twitter username: ")))
	 (password (or twittering-password
		       (read-passwd (format "%s's twitter password: "
					    username)))))
    `(,username . ,password)))

(defun twittering-has-oauth-access-token-p ()
  (let* ((required-entries '("oauth_token"
			     "oauth_token_secret"
			     "user_id"
			     "screen_name"))
	 (value-list
	  (mapcar
	   (lambda (key)
	     (cdr (assoc key twittering-oauth-access-token-alist)))
	   required-entries)))
    (null (remove t (mapcar 'stringp value-list)))))

(defun twittering-verify-credentials ()
  "Verify the account.

This function is an internal function, which should be called from
`twittering-ensure-account-verification'.

If the account has been authorized already, return t.
Otherwise, this function tries to authorize the account.
If the authorization succeeded, return t.
If the authorization failed, return nil."
  (cond
   ((twittering-account-authorized-p)
    ;; The account has been authorized already.
    t)
   ((not (twittering-account-authorization-queried-p))
    ;; This function must be invoked from
    ;; `twittering-ensure-account-verification', which updates the variable
    ;; `twittering-account-authorization' into the symbol `queried'.
    (error "`twittering-verify-credentials' is invoked multiple times.")
    nil)
   ((and (memq twittering-auth-method '(oauth xauth))
	 (or (null twittering-oauth-consumer-key)
	     (null twittering-oauth-consumer-secret)))
    (message "Consumer for OAuth is not specified.")
    nil)
   ((twittering-has-oauth-access-token-p)
    (let* ((username (cdr (assoc "screen_name"
				 (twittering-get-main-account-info))))
	   (proc
	    (twittering-call-api-with-account
	     (twittering-get-main-account-info)
	     'verify-credentials
	     `((sentinel
		. twittering-http-get-verify-credentials-sentinel)
	       (clean-up-sentinel
		. twittering-http-get-verify-credentials-clean-up-sentinel)))))
      (cond
       ((null proc)
	(message "Process invocation for authorizing \"%s\" failed." username)
	;; Failed to authorize the account.
	nil)
       (t
	;; wait for verification to finish.
	(twittering-wait-while nil 0.1
			       (and
				(twittering-account-authorization-queried-p)
				(twittering-process-alive-p proc)))
	(if (not (twittering-account-authorization-queried-p))
	    ;; The query is completed.
	    (twittering-account-authorized-p)
	  ;; If the process has been dead, wait a moment because
	  ;; Emacs may be in the middle of evaluating the sentinel.
	  (twittering-wait-while
	   10 0.1
	   (twittering-account-authorization-queried-p)
	   ;; Succeeded in authorizing the account.
	   t
	   ;; Display a message.
	   (message
	    "Status of Authorization process is `%s'. Type M-x twit to retry."
	    (process-status proc))
	   ;; Failed to authorize the account.
	   nil))))))
   ((eq twittering-auth-method 'oauth)
    (let* ((scheme (if twittering-oauth-use-ssl
		       "https"
		     "http"))
	   (request-token-url
	    (concat scheme twittering-oauth-request-token-url-without-scheme))
	   (access-token-url
	    (concat scheme twittering-oauth-access-token-url-without-scheme))
	   (token-alist
	    (twittering-oauth-get-access-token
	     request-token-url
	     (lambda (token)
	       (concat scheme
		       twittering-oauth-authorization-url-base-without-scheme
		       token))
	     access-token-url
	     twittering-oauth-consumer-key twittering-oauth-consumer-secret
	     "twittering-mode")))
      (cond
       ((and (assoc "oauth_token" token-alist)
	     (assoc "oauth_token_secret" token-alist)
	     (assoc "screen_name" token-alist))
	(let ((username (cdr (assoc "screen_name" token-alist))))
	  (twittering-register-account-info token-alist)
	  (message "Authorization for the account \"%s\" succeeded."
		   username)
	  (when (and twittering-use-master-password
		     (twittering-capable-of-encryption-p)
		     (not (file-exists-p twittering-private-info-file)))
	    (twittering-save-private-info-with-guide))
	  ;; Succeeded in authorizing the account.
	  t))
       (t
	;; There is no global account info that should be invalidated.
	;; Failed to authorize the account.
	(message "Authorization via OAuth failed. Type M-x twit to retry.")
	nil))))
   ((eq twittering-auth-method 'xauth)
    (let* ((account-info (twittering-prepare-account-info))
	   (scheme (if twittering-oauth-use-ssl
		       "https"
		     "http"))
	   (access-token-url
	    (concat scheme twittering-oauth-access-token-url-without-scheme))
	   (token-alist
	    (twittering-xauth-get-access-token
	     access-token-url
	     twittering-oauth-consumer-key twittering-oauth-consumer-secret
	     (car account-info)
	     (cdr account-info))))
      ;; Dispose of password as recommended by Twitter.
      ;; http://dev.twitter.com/pages/xauth
      (setcdr account-info nil)
      (cond
       ((and token-alist
	     (assoc "oauth_token" token-alist)
	     (assoc "oauth_token_secret" token-alist))
	(twittering-register-account-info token-alist)
	(message "Authorization for the account \"%s\" succeeded."
		 (twittering-get-username))
	(when (and twittering-use-master-password
		   (twittering-capable-of-encryption-p)
		   (not (file-exists-p twittering-private-info-file)))
	  (twittering-save-private-info-with-guide))
	;; Succeeded in authorizing the account.
	t)
       (t
	;; Failed to authorize the account.
	(message "Authorization via xAuth failed. Type M-x twit to retry.")
	nil))))
   ((eq twittering-auth-method 'basic)
    (let* ((account-info
	    (let ((pair (twittering-prepare-account-info)))
	      `(("screen_name" . ,(car pair))
		("password" . ,(cdr pair)))))
	   ;; Bind account information locally to ensure that
	   ;; the variables are reset when the verification fails.
	   (twittering-username (car account-info))
	   (twittering-password (cdr account-info))
	   (proc
	    (twittering-call-api-with-account
	     account-info
	     'verify-credentials
	     `((sentinel . twittering-http-get-verify-credentials-sentinel)
	       (clean-up-sentinel
		. twittering-http-get-verify-credentials-clean-up-sentinel)))))
      (cond
       ((null proc)
	(message "Process invocation for authorizing \"%s\" failed."
		 (twittering-get-from-account-info "screen_name" account-info))
	;; Failed to authorize the account.
	nil)
       (t
	;; wait for verification to finish.
	(twittering-wait-while nil 0.1
			       (and
				(twittering-account-authorization-queried-p)
				(twittering-process-alive-p proc)))
	(if (not (twittering-account-authorization-queried-p))
	    ;; The query is finished.
	    (twittering-account-authorized-p)
	  ;; If the process has been dead, wait a moment because
	  ;; Emacs may be in the middle of evaluating the sentinel.
	  (twittering-wait-while
	   10 0.1
	   (twittering-account-authorization-queried-p)
	   ;; Succeeded in authorizing the account.
	   t
	   ;; Display a message.
	   (message
	    "Status of Authorization process is `%s'. Type M-x twit to retry."
	    (process-status proc))
	   ;; Failed to authorize the account.
	   nil))))))
   (t
    (message "%s is invalid as an authorization method."
	     twittering-auth-method)
    nil)))

(defun twittering-http-get-verify-credentials-sentinel (proc status connection-info header-info)
  (let* ((status-line (cdr (assq 'status-line header-info)))
	 (status-code (cdr (assq 'status-code header-info)))
	 (account-info (cdr (assq 'account-info connection-info)))
	 (username
	  (twittering-get-from-account-info "screen_name" account-info))
	 (password
	  (twittering-get-from-account-info "password" account-info)))
    (case-string
     status-code
     (("200")
      (twittering-register-account-info account-info)
      (setq twittering-account-authorization 'authorized)
      (message "Authorization for the account \"%s\" succeeded." username)
      nil)
     (("401")
      (setq twittering-account-authorization nil)
      (let ((error-mes
	     (format "Authorization for the account \"%s\" failed. Type M-x twit to retry with correct information."
		     username)))
	;; Invalidate the account info.
	(twittering-register-account-info nil)
	(message "%s" error-mes)
	nil))
     (t
      (setq twittering-account-authorization nil)
      (let ((error-mes
	     (format "Authorization for the account \"%s\" failed due to \"%s\"."
		     username status-line)))
	(message "%s" error-mes)
	nil)))))

(defun twittering-http-get-verify-credentials-clean-up-sentinel (proc status connection-info)
  (when (and (memq status '(exit signal closed failed))
	     (eq twittering-account-authorization 'queried))
    (setq twittering-account-authorization nil)
    (let ((exit-status (cond
			((processp proc) (process-exit-status proc))
			(t 0)))
	  (command (process-command proc)))
      (if (= 0 exit-status)
	  (message "Authorization failed. Type M-x twit to retry.")
	(message "Authorization failed: %s exited abnormally (exit-status=%s)."
		 (car command) exit-status)))
    (setq twittering-username nil)
    (setq twittering-password nil)))

(defun twittering-ensure-account-verification ()
  "Ensure verification of an account.

If an account has been already authorized, return t.
If a query of authorization is being processed, return nil.

Otherwise, this function tries to authorize an account by calling
`twittering-verify-credentials'.
If the authorization succeeded, return t.
If the authorization failed, return nil."
  (cond
   ((twittering-account-authorized-p)
    ;; The account has been already authorized.
    t)
   ((twittering-account-authorization-queried-p)
    ;; The account has not been authorized yet.
    nil)
   (t
    (setq twittering-account-authorization 'queried)
    (let ((result nil))
      (unwind-protect
	  (setq result (twittering-verify-credentials))
	(if result
	    (setq twittering-account-authorization 'authorized)
	  (setq twittering-account-authorization nil)))
      result))))

;;;;
;;;; Status retrieval
;;;;

(defun twittering-add-timeline-history (spec-string)
  (when (or (null twittering-timeline-history)
	    (not (string= spec-string (car twittering-timeline-history))))
    (twittering-add-to-history 'twittering-timeline-history spec-string))
  (let ((spec (twittering-string-to-timeline-spec spec-string)))
    (when (and (twittering-timeline-spec-is-user-p spec)
	       (or (null twittering-user-history)
		   (not (string= spec-string (car twittering-user-history)))))
      (twittering-add-to-history 'twittering-user-history (cadr spec)))))

(defun twittering-remove-timeline-spec-string-from-history (spec-string)
  (setq twittering-timeline-history
	(remove nil
		(mapcar
		 (lambda (str)
		   (if (twittering-equal-string-as-timeline spec-string str)
		       nil
		     str))
		 twittering-timeline-history))))

(defun twittering-make-alist-of-forbidden-tweet (id &optional user-screen-name)
  (let ((created-at
	 (or
	  (twittering-id-to-time id)
	  (apply 'encode-time
		 (parse-time-string "Jan 01 00:00:00 +0000 2012")))))
  `((forbidden . t)
    (id . ,id)
    (created-at . ,created-at)
    (user-name . nil)
    (user-screen-name . ,user-screen-name)
    (text . "SORRY, YOU ARE NOT AUTHORIZED TO SEE THIS TWEET.")
    )))

(defun twittering-make-alist-of-non-existent-tweet (id &optional user-screen-name)
  (let ((created-at
	 (or
	  (twittering-id-to-time id)
	  (apply 'encode-time
		 (parse-time-string "Jan 01 00:00:00 +0000 2012")))))
  `((forbidden . t)
    (id . ,id)
    (created-at . ,created-at)
    (user-name . nil)
    (user-screen-name . ,user-screen-name)
    (text . ,(format "THE TWEET WITH ID %s DOES NOT EXIST." id))
    )))

(defun twittering-atom-xmltree-to-status-datum (atom-xml-entry)
  (let* ((id-str (car (cddr (assq 'id atom-xml-entry))))
	 (time-str (car (cddr (assq 'updated atom-xml-entry))))
	 (author-str (car (cddr (assq 'name (assq 'author atom-xml-entry)))))
	 (formatted-time-str
	  ;; ISO 8601
	  ;; Twitter -> "2010-05-08T05:59:41Z"
	  ;; StatusNet -> "2010-05-08T08:44:39+00:00"
	  (cond
	   ((string-match
	     "\\(.*\\)T\\(.*\\)\\(Z\\|\\([-+][0-2][0-9]\\):?\\([0-5][0-9]\\)\\)"
	     time-str)
	    ;; time-str is formatted as
	    ;; "Combined date and time in UTC:" in ISO 8601.
	    (let ((timezone (match-string 3 time-str)))
	      (format "%s %s %s"
		      (match-string 1 time-str) (match-string 2 time-str)
		      (if (string= "Z" timezone)
			  "+0000"
			(concat (match-string 4 time-str)
				(match-string 5 time-str))))))
	   (t
	    ;; unknown format?
	    time-str))))
    `((created-at . ,(date-to-time formatted-time-str))
      (id . ,(progn
	       (string-match ":\\([0-9]+\\)$" id-str)
	       (match-string 1 id-str)))
      ,@(let ((source (twittering-decode-html-entities
		       (car (cddr (assq 'twitter:source atom-xml-entry))))))
	  `(,@(if (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				source)
		  (let ((uri (match-string-no-properties 1 source))
			(caption (match-string-no-properties 2 source)))
		    `((source . ,caption)
		      (source-uri . ,uri)))
		`((source . ,source)
		  (source-uri . "")))))
      (text . ,(twittering-decode-html-entities
		(car (cddr (assq 'title atom-xml-entry)))))
      ,@(cond
	 ((and (eq twittering-service-method 'statusnet)
	       (string-match "^\\([^ ]+\\)\\( (\\(.*\\))\\)?$" author-str))
	  ;; StatusNet
	  `((user-screen-name . ,(match-string 1 author-str))
	    (user-name . ,(or (match-string 3 author-str) ""))))
	 ((string-match "^\\([^ ]+\\) (\\(.*\\))$" author-str)
	  ;; Twitter (default)
	  `((user-screen-name . ,(match-string 1 author-str))
	    (user-name . ,(match-string 2 author-str))))
	 (t
	  '((user-screen-name . "PARSING FAILED!!")
	    (user-name . ""))))
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
		     (cond
		      ((and (eq twittering-service-method 'statusnet)
			    (member '(rel . "related") item))
		       ;; StatusNet
		       (cdr (assq 'href item)))
		      ((member '(rel . "image") item)
		       ;; Twitter (default)
		       (cdr (assq 'href item)))
		      (t
		       nil)))
		   link-items)))
	    (car-safe (remq nil image-urls)))))))

(defun twittering-atom-xmltree-to-status (atom-xmltree)
  (let ((entry-list
	 (apply 'append
		(mapcar (lambda (x)
		 	  (if (eq (car-safe x) 'entry) `(,x) nil))
			(cdar atom-xmltree)))))
    (mapcar 'twittering-atom-xmltree-to-status-datum
	    entry-list)))

(eval-and-compile
  (defsubst twittering-make-gap-list (text)
    "Return a list representing index gaps between TEXT and the encoded text.
Indices included in entities in XML response from Twitter mean the positions
of characters in the encoded text, where \"<\" and \">\" are encoded as
\"&lt;\" and \"&gt;\" respectively.
Therefore, the indices differ from the positions in the decoded text.

This function calculates the gaps from TEXT, which is assumed to be decoded.
This function returns a list of pairs representing the gaps.
For each pair, the car means the position in the encoded text and the cdr
means the gap. The (car pair)-th character in the encoded text corresponds
to the (- (car pair) (cdr pair))-th character in the decoded text."
    (let ((result nil)
	  (pos 0)
	  (gap 0))
      (while (string-match "[<>]" text pos)
	(let ((shift 3))
	  (setq result
		(cons `(,(+ gap (match-end 0)) . ,(+ gap shift)) result))
	  (setq gap (+ shift gap)))
	(setq pos (match-end 0)))
      (reverse result)))

  (defun twittering-get-gap (pos gap-list)
    "Return the gap at the specific position.
GAP-LIST must be generated by `twittering-make-gap-list'."
    (let ((rest-gaps gap-list)
	  (gap 0))
      (while (and rest-gaps (< (caar rest-gaps) pos))
	(setq gap (cdar rest-gaps))
	(setq rest-gaps (cdr rest-gaps)))
      gap)))

(defun twittering-normalize-raw-status (raw-status &optional ignore-retweet)
  (let* ((status-data (cddr raw-status))
	 (raw-retweeted-status (assq 'retweeted_status status-data)))
    (cond
     ((and raw-retweeted-status
	   (not ignore-retweet))
      (let ((retweeted-status
	     (twittering-normalize-raw-status raw-retweeted-status t))
	    (retweeting-status
	     (twittering-normalize-raw-status raw-status t))
	    (items-overwritten-by-retweet
	     '(id)))
	`(,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 (if (memq sym items-overwritten-by-retweet)
		     (let ((value-on-retweet
			    (cdr (assq sym retweeting-status))))
		       ;; Replace the value in `retweeted-status' with
		       ;; that in `retweeting-status'.
		       `(,sym . ,value-on-retweet))
		   `(,sym . ,value))))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeted-" (symbol-name sym)))
		   . ,value)))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeting-" (symbol-name sym)))
		   . ,value)))
	     retweeting-status))))
     (t
      (flet ((assq-get (item seq)
		       (car (cddr (assq item seq)))))
	`(,@(mapcar
	     (lambda (entry)
	       (let* ((sym (elt entry 0))
		      (sym-in-data (elt entry 1))
		      (encoded (elt entry 2))
		      (data (assq-get sym-in-data status-data)))
		 `(,sym . ,(if encoded
			       (twittering-decode-entities-after-parsing-xml
				data)
			     data))))
	     '(;; Raw entries.
	       (id id)
	       (in-reply-to-screen-name in_reply_to_screen_name)
	       (in-reply-to-status-id in_reply_to_status_id)
	       (recipient-screen-name recipient_screen_name)
	       ;; Encoded entries.
	       (text text t)
	       ))
	  ;; created_at
	  (created-at . ,(date-to-time (assq-get 'created_at status-data)))
	  ;; Replace "true" and "false" into t and nil.
	  ,@(mapcar (lambda (sym)
		      `(,sym . ,(string= "true" (assq-get sym status-data))))
		    '(favorited truncated))
	  ;; Entities.
	  ,(let* ((entity-data (cddr (assq 'entities status-data)))
		  (encoded-text (assq-get 'text status-data))
		  (text
		   (twittering-decode-entities-after-parsing-xml encoded-text))
		  (gap-list (twittering-make-gap-list text)))
	     (list
	      'entity
	      ;; hashtags
	      (cons
	       'hashtags
	       (remove nil
		       (mapcar
			(lambda (entry)
			  (when (and (consp entry)
				     (eq 'hashtag (car entry)))
			    (let* ((data (cdr entry))
				   (start-str (cdr (assq 'start (car data))))
				   (end-str (cdr (assq 'end (car data))))
				   (start (if (stringp start-str)
					      (string-to-number start-str)
					    0))
				   (end (if (stringp end-str)
					    (string-to-number end-str)
					  0))
				   (gap (twittering-get-gap start gap-list)))
			      `((start . ,(- start gap))
				(end . ,(- end gap))
				(text . ,(elt (assq 'text data) 2))))))
			(assq 'hashtags entity-data))))
	      ;; mentions
	      (cons
	       'mentions
	       (remove nil
		       (mapcar
			(lambda (entry)
			  (when (and (consp entry)
				     (eq 'user_mention (car entry)))
			    (let* ((data (cdr entry))
				   (start-str (cdr (assq 'start (car data))))
				   (end-str (cdr (assq 'end (car data))))
				   (start (if (stringp start-str)
					      (string-to-number start-str)
					    0))
				   (end (if (stringp end-str)
					    (string-to-number end-str)
					  0))
				   (gap (twittering-get-gap start gap-list)))
			      `((start . ,(- start gap))
				(end . ,(- end gap))
				(id . ,(elt (assq 'id data) 2))
				(screen-name
				 . ,(elt (assq 'screen_name data) 2))
				(name
				 . ,(elt (assq 'name data) 2))))))
			(assq 'user_mentions entity-data))))
	      ;; urls
	      (cons
	       'urls
	       (remove nil
		       (mapcar
			(lambda (entry)
			  (when (and (consp entry)
				     (eq 'url (car entry)))
			    (let* ((data (cdr entry))
				   (start-str (cdr (assq 'start (car data))))
				   (end-str (cdr (assq 'end (car data))))
				   (start (if (stringp start-str)
					      (string-to-number start-str)
					    0))
				   (end (if (stringp end-str)
					    (string-to-number end-str)
					  0))
				   (gap (twittering-get-gap start gap-list)))
			      `((start . ,(- start gap))
				(end . ,(- end gap))
				(url . ,(elt (assq 'url data) 2))
				(display-url
				 . ,(elt (assq 'display_url data) 2))
				(expanded-url
				 . ,(elt (assq 'expanded_url data) 2))))))
			(assq 'urls entity-data))))))
	  ;; Source.
	  ,@(let ((source (twittering-decode-html-entities
			   (assq-get 'source status-data))))
	      (if (and source
		       (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				     source))
		  (let ((uri (match-string-no-properties 1 source))
			(caption (match-string-no-properties 2 source)))
		    `((source . ,caption)
		      (source-uri . ,uri)))
		`((source . ,source)
		  (source-uri . ""))))
	  ;; Items related to the user that posted the tweet.
	  ,@(let ((user-data (cddr (assq 'user status-data))))
	      (mapcar
	       (lambda (entry)
		 (let* ((sym (elt entry 0))
			(sym-in-user-data (elt entry 1))
			(encoded (elt entry 2))
			(value (assq-get sym-in-user-data user-data)))
		   `(,sym . ,(if encoded
				 (twittering-decode-html-entities value)
			       value))))
	       '(;; Raw entries.
		 (user-id id)
		 (user-profile-image-url profile_image_url)
		 (user-url url)
		 ;; Encoded entries.
		 (user-name name t)
		 (user-screen-name screen_name t)
		 (user-location location t)
		 (user-description description t))))
	  ,@(let ((user-data (cddr (assq 'user status-data))))
	      (mapcar (lambda (entry)
			`(,(car entry)
			  . ,(string= "true"
				      (assq-get (cdr entry) user-data))))
		      '((user-protected . protected))))))))))

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
			  (user nil ,@(cdddr (assq 'sender c-node)))
			  (entities nil ,@(cdddr (assq 'entities c-node)))))
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

  (mapcar #'twittering-normalize-raw-status
 	  ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
 	  ;; On Emacs22, there may be blank strings
	  (remove nil (mapcar (lambda (x)
				(if (consp x) x))
			      xmltree))))

(defun twittering-decode-entities-after-parsing-xml (encoded-str)
  "Decode ENCODED-STR retrieved by parsing XML and return the result.
On Emacs 22 and later, `xml-parse-region' resolves numeric character
references. It is redundant to resolve numeric character references
again. However, in a XML response from Twitter, the two characters,
\"<\" and \">\", are doubly escaped as \"&amp;lt;\" and \"&amp;gt;\",
respectively. Then, they are represented as \"&lt;\" and \"&gt;\" in
the result of `xml-parse-region'. This function decodes them.

On Emacs 21, `xml-parse-region' does not resolve numeric character
references. This function decodes them."
  (cond
   ((null encoded-str)
    "")
   ((> 22 emacs-major-version)
    (replace-regexp-in-string
     "&#\\([0-9]+\\);"
     (lambda (str)
       (let ((number-entity
	      (progn
		(string-match "&#\\([0-9]+\\);" str)
		(match-string 1 str))))
	 (char-to-string
	  (twittering-ucs-to-char (string-to-number number-entity)))))
     encoded-str))
   (t
    (replace-regexp-in-string
     "&\\(?:\\(gt\\)\\|\\(lt\\)\\);"
     (lambda (str)
       (if (match-beginning 1)
	   ">"
	 "<"))
     encoded-str))))

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

;; JSON
(defun twittering-extract-common-element-from-json (json-object)
  "Extract common parameters of a tweet from JSON-OBJECT.
Return an alist including text, created_at and entities, which are common
to JSON objects from ordinary timeline and search timeline."
  (let* ((encoded-text (cdr (assq 'text json-object)))
	 (text
	  (twittering-decode-entities-after-parsing-xml encoded-text))
	 (gap-list (twittering-make-gap-list text))
	 (entities (cdr (assq 'entities json-object)))
	 (urls (cdr (assq 'urls entities)))
	 (hashtags (cdr (assq 'hashtags entities)))
	 (mentions (cdr (assq 'user_mentions entities))))
    `((text . ,text)
      (created-at
       . ,(apply 'encode-time
		 (parse-time-string (cdr (assq 'created_at json-object)))))
      (entity
       (hashtags . ,(mapcar (lambda (entry)
			      (let* ((indices (cdr (assq 'indices entry)))
				     (start (elt indices 0))
				     (end (elt indices 1))
				     (gap
				      (twittering-get-gap start gap-list)))
				`((start . ,(- start gap))
				  (end . ,(- end gap))
				  (text . ,(cdr (assq 'text entry))))))
			    hashtags))
       (mentions . ,(mapcar (lambda (entry)
			      (let* ((indices (cdr (assq 'indices entry)))
				     (start (elt indices 0))
				     (end (elt indices 1))
				     (gap
				      (twittering-get-gap start gap-list)))
				`((start . ,(- start gap))
				  (end . ,(- end gap))
				  (id . ,(cdr (assq 'id_str entry)))
				  (name . ,(cdr (assq 'name entry)))
				  (screen-name
				   . ,(cdr (assq 'screen_name entry))))))
			    mentions))
       (urls . ,(mapcar (lambda (entry)
			  (let* ((indices (cdr (assq 'indices entry)))
				 (start (elt indices 0))
				 (end (elt indices 1))
				 (gap (twittering-get-gap start gap-list)))
			    `((start . ,(- start gap))
			      (end . ,(- end gap))
			      (url . ,(cdr (assq 'url entry)))
			      (display-url
			       . ,(cdr (assq 'display_url entry)))
			      (expanded-url
			       . ,(cdr (assq 'expanded_url entry))))))
			urls))))))

(defun twittering-json-object-to-a-status (json-object)
  "Convert JSON-OBJECT representing a tweet into an alist representation.
JSON-OBJECT must originate in an ordinary timeline, not a search timeline.
To convert a JSON object from a search timeline, use
`twittering-json-object-to-a-status-on-search'."
  (let* ((raw-retweeted-status (cdr (assq 'retweeted_status json-object))))
    (cond
     (raw-retweeted-status
      (let ((retweeted-status
	     (twittering-json-object-to-a-status-base raw-retweeted-status))
	    (retweeting-status
	     (twittering-json-object-to-a-status-base json-object))
	    (items-overwritten-by-retweet
	     '(id)))
	`(,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 (if (memq sym items-overwritten-by-retweet)
		     (let ((value-on-retweet
			    (cdr (assq sym retweeting-status))))
		       ;; Replace the value in `retweeted-status' with
		       ;; that in `retweeting-status'.
		       `(,sym . ,value-on-retweet))
		   `(,sym . ,value))))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeted-" (symbol-name sym)))
		   . ,value)))
	     retweeted-status)
	  ,@(mapcar
	     (lambda (entry)
	       (let ((sym (car entry))
		     (value (cdr entry)))
		 `(,(intern (concat "retweeting-" (symbol-name sym)))
		   . ,value)))
	     retweeting-status))))
     (t
      (twittering-json-object-to-a-status-base json-object)))))

(defun twittering-json-object-to-a-status-base (json-object)
  (let ((user-data (cdr (assq 'user json-object))))
    `(,@(twittering-extract-common-element-from-json json-object)
      ,@(let ((symbol-table
	       '((favorited . favorited)
		 (id_str . id)
		 (in_reply_to_screen_name . in-reply-to-screen-name)
		 (in_reply_to_status_id_str . in-reply-to-status-id)
		 (recipient_screen_name . recipient-screen-name)
		 (truncated . truncated))))
	  (remove nil
		  (mapcar
		   (lambda (entry)
		     (let* ((sym (car entry))
			    (value (cdr entry))
			    (value
			     (if (and (memq sym '(favorited truncated))
				      (eq value :json-false))
				 nil
			       value))
			    (dest (cdr (assq sym symbol-table))))
		       (when (and dest value)
			 `(,dest . ,value))))
		   json-object)))
      ;; source
      ,@(let ((source (cdr (assq 'source json-object))))
	  (if (and source
		   (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				 source))
	      (let ((uri (match-string-no-properties 1 source))
		    (caption (match-string-no-properties 2 source)))
		`((source . ,caption)
		  (source-uri . ,uri)))
	    `((source . ,source)
	      (source-uri . ""))))
      ;; user data
      ,@(let ((symbol-table
	       '((id_str . user-id)
		 (profile_image_url . user-profile-image-url)
		 (url . user-url)
		 (protected . user-protected)
		 (name . user-name)
		 (screen_name . user-screen-name)
		 (location . user-location)
		 (description . user-description))))
	  (remove nil
		  (mapcar (lambda (entry)
			    (let* ((sym (car entry))
				   (value (cdr entry))
				   (value
				    (if (and (eq sym 'protected)
					     (eq value :json-false))
					nil
				      value)))
			      (when value
				(let ((dest (cdr (assq sym symbol-table))))
				  (when dest
				    `(,dest . ,value))))))
			  user-data))))))

(defun twittering-json-object-to-a-status-on-search (json-object)
  "Convert JSON-OBJECT representing a tweet into an alist representation.
JSON-OBJECT must originate in a search timeline.
To convert a JSON object from other timelines, use
`twittering-json-object-to-a-status'."
  `(,@(twittering-extract-common-element-from-json json-object)
    ,@(let ((symbol-table
	     '((id_str . id)
	       (to_user . in-reply-to-screen-name)
	       (in_reply_to_status_id_str . in-reply-to-status-id)
	       ;; user data
	       (from_user_id_str . user-id)
	       (profile_image_url . user-profile-image-url)
	       (from_user_name . user-name)
	       (from_user . user-screen-name))))
	  (remove nil
		  (mapcar
		   (lambda (entry)
		     (let* ((sym (car entry))
			    (value (cdr entry))
			    (dest (cdr (assq sym symbol-table))))
		       (when (and dest value)
			 `(,dest . ,value))))
		   json-object)))
    ;; source
    ,@(let ((source
	       (twittering-decode-html-entities
		(cdr (assq 'source json-object)))))
	  (if (and source
		   (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
				 source))
	      (let ((uri (match-string-no-properties 1 source))
		    (caption (match-string-no-properties 2 source)))
		`((source . ,caption)
		  (source-uri . ,uri)))
	    `((source . ,source)
	      (source-uri . ""))))))

(defun twittering-json-object-to-a-status-on-direct-messages (json-object)
  "Convert JSON-OBJECT representing a tweet into an alist representation.
JSON-OBJECT must originate in timelines related to direct messages.
To convert a JSON object from other timelines, use
`twittering-json-object-to-a-status'."
  `(,@(twittering-extract-common-element-from-json json-object)
    ,@(let ((symbol-table
	     '((id_str . id)
	       (recipient_screen_name . recipient-screen-name))))
	(remove nil
		  (mapcar
		   (lambda (entry)
		     (let* ((sym (car entry))
			    (value (cdr entry))
			    (dest (cdr (assq sym symbol-table))))
		       (when (and dest value)
			 `(,dest . ,value))))
		   json-object)))
    ;; sender
    ,@(let ((symbol-table
	     '((id_str . user-id)
	       (name . user-name)
	       (profile_image_url . user-profile-image-url)
	       (protected . user-protected)
	       (screen_name . user-screen-name))))
	(remove nil
		(mapcar
		 (lambda (entry)
		   (let* ((sym (car entry))
			  (value (cdr entry))
			  (value
			   (cond
			    ((eq sym 'protected)
			     (if (eq value :json-false)
				 nil
			       t))
			    ((eq value :json-false)
			     nil)
			    (t
			     value))))
		     (when value
		       (let ((dest (cdr (assq sym symbol-table))))
			 (when dest
			   `(,dest . ,value))))))
		 (cdr (assq 'sender json-object)))))))

;;;;
;;;; List info retrieval
;;;;

(defun twittering-get-list-index (username)
  (twittering-call-api
   'get-list-index
   `((username . ,username)
     (sentinel . twittering-http-get-list-index-sentinel))))

(defun twittering-get-list-subscriptions (username)
  (twittering-call-api
   'get-list-subscriptions
   `((username . ,username)
     (sentinel . twittering-http-get-list-subscriptions-sentinel))))

(defun twittering-get-list-sync (username function)
  (setq twittering-list-index-retrieved nil)
  (let ((proc (funcall function username)))
    (when proc
      (twittering-wait-while nil 0.1
			     (and (not twittering-list-index-retrieved)
				  (twittering-process-alive-p proc)))
      (when (and (not twittering-list-index-retrieved)
		 (not (twittering-process-alive-p proc)))
	;; If the process has been dead, wait a moment because
	;; Emacs may be in the middle of evaluating the sentinel.
	(twittering-wait-while 10 0.1
			       (not twittering-list-index-retrieved)))))
  (cond
   ((null twittering-list-index-retrieved)
    (message "Failed to retrieve %s's lists." username)
    nil)
   ((stringp twittering-list-index-retrieved)
    (if (string= "" twittering-list-index-retrieved)
	(message "%s does not have a list." username)
      (message "%s" twittering-list-index-retrieved))
    nil)
   ((listp twittering-list-index-retrieved)
    twittering-list-index-retrieved)))

(defun twittering-get-list-index-sync (username)
  (twittering-get-list-sync username 'twittering-get-list-index))

(defun twittering-get-list-subscriptions-sync (username)
  (twittering-get-list-sync username 'twittering-get-list-subscriptions))

;;;;
;;;; Buffer info
;;;;

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

(defun twittering-buffer-related-p ()
  "Return t if current buffer relates to `twittering-mode'."
  (or (twittering-buffer-p)
      (eq major-mode 'twittering-edit-mode)
      (string= (buffer-name (current-buffer))
	       twittering-debug-buffer)))

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
    (twittering-add-timeline-history spec-string)
    (let ((buffer (twittering-get-buffer-from-spec spec)))
      (if buffer
	  (progn
	    (twittering-replace-spec-string-for-buffer buffer spec-string)
	    (twittering-update-mode-line)
	    buffer)
	(let ((buffer (generate-new-buffer spec-string))
	      (start-timer (null twittering-buffer-info-list)))
	  (add-to-list 'twittering-buffer-info-list buffer t)
	  (with-current-buffer buffer
	    (twittering-mode-setup spec-string)
	    (twittering-rerender-timeline-all buffer)
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

;;;;
;;;; Icon mode
;;;;

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
      (twittering-rerender-timeline-all (current-buffer) t))))

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

(defvar twittering-fallback-image-format 'xpm
  "*Fallback format used for displaying an image without a supproted format.
Images which Emacs does not supports are converted into the fallback image
format.")

(defvar twittering-use-profile-image-api nil
  "*Whether to use `profile_image' API for retrieving scaled icon images.
NOTE: This API is rate limited and is obsolete in the Twitter REST API v1.1.")

(defvar twittering-icon-storage-file
  (expand-file-name "~/.twittering-mode-icons.gz")
  "*The file to which icon images are stored.
`twittering-icon-storage-limit' determines the number icons stored in the
file.
The file is loaded with `with-auto-compression-mode'.")

(defvar twittering-use-icon-storage nil
  "*Whether to use the persistent icon storage.
If this variable is non-nil, icon images are stored to the file specified
by `twittering-icon-storage-file'.")

(defvar twittering-icon-storage-recent-icons nil
  "List of recently rendered icons.")

(defvar twittering-icon-storage-limit 500
  "*How many icons are stored in the persistent storage.
If `twittering-use-icon-storage' is nil, this variable is ignored.
If a positive integer N, `twittering-save-icon-properties' saves N icons that
have been recently rendered.
If nil, the function saves all icons.")

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

(defun twittering-update-icon-storage-recent-icons (size image-url spec)
  (unless (null twittering-icon-storage-limit)
    (let ((dummy-icon-properties (twittering-make-display-spec-for-icon
				  twittering-error-icon-data-pair)))
      (unless (equal spec dummy-icon-properties)
	(let ((history-delete-duplicates t))
	  (twittering-add-to-history 'twittering-icon-storage-recent-icons
				     (list size image-url)
				     twittering-icon-storage-limit))))))

(defun twittering-get-display-spec-for-icon (image-url)
  (let ((hash
	 (gethash twittering-convert-fix-size twittering-icon-prop-hash)))
    (when hash
      (let ((spec (gethash image-url hash))
	    (size twittering-convert-fix-size))
	(when spec
	  (twittering-update-icon-storage-recent-icons size image-url spec)
	  spec)))))

(defun twittering-convert-image-data (image-data dest-type &optional src-type)
  "Convert IMAGE-DATA into XPM format and return it. If it fails to convert,
return nil."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (buffer-disable-undo)
    (let ((coding-system-for-read 'binary)
	  (coding-system-for-write 'binary)
	  (require-final-newline nil)
	  ;; Bind `default-directory' to the temporary directory
	  ;; because it is possible that the directory pointed by
	  ;; `default-directory' has been already removed.
	  (default-directory temporary-file-directory))
      (insert image-data)
      (let* ((args
	      `(,@(when (<= emacs-major-version 22)
		    ;; Emacs22 and earlier raises "Color allocation error"
		    ;; on decoding a XPM image with opacity. To ignore
		    ;; opacity, the option "+matte" is added.
		    '("+matte"))
		,@(unless (fboundp 'create-animated-image)
		    '("-flatten"))
		,(if src-type (format "%s:-" src-type) "-")
		,@(when (integerp twittering-convert-fix-size)
		    `("-resize"
		      ,(format "%dx%d" twittering-convert-fix-size
			       twittering-convert-fix-size)))
		,(format "%s:-" dest-type)))
	     (exit-status
	      (apply 'call-process-region (point-min) (point-max)
		     twittering-convert-program t `(t nil) nil args)))
	(if (equal 0 exit-status)
	    (buffer-string)
	  ;; failed to convert the image.
	  nil)))))

(defun twittering-create-image-pair (image-data)
  "Return a pair of image type and image data.
IMAGE-DATA is converted by `convert' if the image type of IMAGE-DATA is not
available and `twittering-use-convert' is non-nil."
  (let* ((image-type (and image-data (image-type-from-data image-data)))
	 (image-pair `(,image-type . ,image-data))
	 (converted-size
	  `(,twittering-convert-fix-size . ,twittering-convert-fix-size)))
    (cond
     ((null image-data)
      twittering-error-icon-data-pair)
     ((and (image-type-available-p image-type)
	   (or (fboundp 'create-animated-image)
	       (not (and twittering-use-convert
			 (eq image-type 'gif))))
	   (or (not (integerp twittering-convert-fix-size))
	       (equal (image-size (create-image image-data image-type t) t)
		      converted-size)))
      image-pair)
     (twittering-use-convert
      (let ((converted-data
	     (twittering-convert-image-data image-data
					    twittering-fallback-image-format)))
	(if converted-data
	    `(,twittering-fallback-image-format . ,converted-data)
	  twittering-error-icon-data-pair)))
     (t
      twittering-error-icon-data-pair))))

(defun twittering-register-image-spec (image-url spec size)
  (let ((hash (gethash size twittering-icon-prop-hash)))
    (unless hash
      (setq hash (make-hash-table :test 'equal))
      (puthash size hash twittering-icon-prop-hash))
    (puthash image-url spec hash)))

(defun twittering-register-image-data (image-url image-data &optional size)
  (let ((image-pair (twittering-create-image-pair image-data))
	(size (or size twittering-convert-fix-size)))
    (when image-pair
      (let ((spec (twittering-make-display-spec-for-icon image-pair)))
	(twittering-register-image-spec image-url spec size)
	spec))))

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
     ((and (integerp image-data)
	   (<= twittering-url-request-retry-limit image-data))
      ;; Try to retrieve the image no longer.
      (twittering-register-image-data image-url nil)
      (twittering-make-icon-string beg end image-url))
     ((and image-data (not (integerp image-data)))
      (twittering-register-image-data image-url image-data)
      (twittering-make-icon-string beg end image-url))
     (t
      (put-text-property 0 (length icon-string)
			 'need-to-be-updated
			 `(twittering-make-icon-string ,image-url)
			 icon-string)
      (twittering-url-retrieve-async image-url 'twittering-register-image-data)
      icon-string))))

(defun twittering-save-icon-properties (&optional filename)
  (let ((filename (or filename twittering-icon-storage-file))
	(stored-data
	 (cond
	  ((null twittering-icon-storage-limit)
	   (let ((result nil)
		 (dummy-icon-properties (twittering-make-display-spec-for-icon
					 twittering-error-icon-data-pair)))
	     (maphash
	      (lambda (size hash)
		(maphash (lambda (url properties)
			   (unless (equal properties dummy-icon-properties)
			     (setq result (cons (list size url) result))))
			 hash))
	      twittering-icon-prop-hash)
	     result))
	  (t
	   (reverse twittering-icon-storage-recent-icons))))
	;; Bind `default-directory' to the temporary directory
	;; because it is possible that the directory pointed by
	;; `default-directory' has been already removed.
	(default-directory temporary-file-directory))
    (when (require 'jka-compr nil t)
      (with-auto-compression-mode
	(let ((coding-system-for-write 'binary))
	  (with-temp-file filename
	    (insert "( 2 ")
	    (prin1 (cons 'emacs-version emacs-version) (current-buffer))
	    (insert "(icon-list ")
	    (mapc
	     (lambda (entry)
	       (let* ((size (elt entry 0))
		      (url (elt entry 1))
		      (properties
		       (gethash url
				(gethash size twittering-icon-prop-hash))))
		 (insert (if size
			     (format "(%d " size)
			   "(nil "))
		 (prin1 url (current-buffer))
		 (insert " ")
		 (prin1 properties (current-buffer))
		 (insert ")\n")))
	     stored-data)
	    (insert "))")))))))

(defun twittering-load-icon-properties (&optional filename)
  (let* ((filename (or filename twittering-icon-storage-file))
	 ;; Bind `default-directory' to the temporary directory
	 ;; because it is possible that the directory pointed by
	 ;; `default-directory' has been already removed.
	 (default-directory temporary-file-directory)
	 (data
	  (with-temp-buffer
	    (condition-case err
		(cond
		 ((and (require 'jka-compr)
		       (file-exists-p filename))
		  (with-auto-compression-mode
		    (let ((coding-system-for-read 'binary)
			  (coding-system-for-write 'binary))
		      (insert-file-contents filename)))
		  (read (current-buffer)))
		 (t
		  nil))
	      (error
	       (message "Failed to load icon images. %s" (cdr err))
	       nil)))))
    (cond
     ((equal 2 (car data))
      (let ((version (cdr (assq 'emacs-version data))))
	(cond
	 ((or (equal version emacs-version)
	      (y-or-n-p
	       (format "%s is generated by Emacs %s! Use it?"
		       filename version)))
	  (mapc (lambda (entry)
		  (let ((size (elt entry 0))
			(url (elt entry 1))
			(properties (elt entry 2)))
		    (twittering-update-icon-storage-recent-icons size url
								 properties)
		    (twittering-register-image-spec url properties size)))
		(cdr (assq 'icon-list data))))
	 (t
	  (message "Stopped loading icons")))))
     (t
      (mapc (lambda (entry)
	      (let ((size (car entry))
		    (prop-alist (cdr entry)))
		(mapc (lambda (entry)
			(let ((url (car entry))
			      (properties (cdr entry)))
			  (twittering-update-icon-storage-recent-icons
			   size url properties)
			  (twittering-register-image-spec url properties
							  size)))
		      prop-alist)))
	    data)))))

;;;;
;;;; Mode-line
;;;;

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

(defconst twittering-modeline-properties
  (when (display-mouse-p)
    `(local-map
      ,(purecopy (make-mode-line-mouse-map
		  'mouse-2 #'twittering-toggle-activate-buffer))
      help-echo "mouse-2 toggles activate buffer")))

(defconst twittering-modeline-active
  (if twittering-active-indicator-image
      (apply 'propertize " "
	     `(display ,twittering-active-indicator-image
		       ,@twittering-modeline-properties))
    " "))

(defconst twittering-modeline-inactive
  (if twittering-inactive-indicator-image
      (apply 'propertize "INACTIVE"
	     `(display ,twittering-inactive-indicator-image
		       ,@twittering-modeline-properties))
    "INACTIVE"))

(defun twittering-mode-line-buffer-identification ()
  (let ((active-mode-indicator
	 (if twittering-active-mode
	     twittering-modeline-active
	   twittering-modeline-inactive))
	(enabled-options
	 `(,(if twittering-display-connection-method
		(concat
		 (when twittering-use-ssl (concat twittering-modeline-ssl ":"))
		 (twittering-get-connection-method-name twittering-use-ssl))
	      (when twittering-use-ssl twittering-modeline-ssl))
	   ,@(when twittering-jojo-mode '("jojo"))
	   ,@(when twittering-icon-mode '("icon"))
	   ,@(when twittering-reverse-mode '("reverse"))
	   ,@(when twittering-proxy-use '("proxy")))))
    (concat active-mode-indicator
	    (when twittering-display-remaining
	      (let ((spec (twittering-current-timeline-spec)))
		(twittering-get-ratelimit-indicator-string spec)))
	    (when enabled-options
	      (concat "[" (mapconcat 'identity enabled-options " ") "]")))))

(defun twittering-update-mode-line ()
  "Update mode line."
  (force-mode-line-update))

;;;;
;;;; Format of a status
;;;;

(eval-and-compile
  (defsubst twittering-make-common-properties (status)
    "Generate a property list that tweets should have irrespective of format."
    (apply 'append
	   (mapcar (lambda (entry)
		     (let ((prop-sym (if (consp entry) (car entry) entry))
			   (status-sym (if (consp entry) (cdr entry) entry)))
		       (list prop-sym (cdr (assq status-sym status)))))
		   '(id retweeted-id source-spec
			(username . user-screen-name) text)))))

(defun twittering-get-common-properties (pos)
  "Get a common property list of the tweet rendered at POS.
The common property list is added to each rendered tweet irrespective
of format. The common properties follows:
 properites generated by `twittering-make-common-properties',
 `field' and `rendered-as' generated by `twittering-render-a-field' or
 `twittering-make-properties-of-popped-ancestors'."
  (apply 'append
	 (mapcar (lambda (prop)
		   (let ((value (get-text-property pos prop)))
		     (when value
		       `(,prop ,value))))
		 '(field id rendered-as retweeted-id source-spec
			 text username))))

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

(eval-and-compile
  (defsubst twittering-make-string-with-user-name-property (str status)
    (if str
	(let* ((user-screen-name (cdr (assq 'user-screen-name status)))
	       (uri (twittering-get-status-url user-screen-name))
	       (spec
		(twittering-make-user-timeline-spec-direct user-screen-name)))
	  (propertize str
		      'mouse-face 'highlight
		      'keymap twittering-mode-on-uri-map
		      'uri uri
		      'screen-name-in-text user-screen-name
		      'goto-spec spec
		      'face 'twittering-username-face
		      'front-sticky nil
		      'rear-nonsticky t))
      ""))

  (defsubst twittering-make-string-with-source-property (str status)
    (if str
	(let ((uri (cdr (assq 'source-uri status))))
	  (propertize str
		      'mouse-face 'highlight
		      'keymap twittering-mode-on-uri-map
		      'uri uri
		      'face 'twittering-uri-face
		      'source str
		      'front-sticky nil
		      'rear-nonsticky t))
      ""))

  (defsubst twittering-make-string-with-uri-property (str status)
    (if str
	(let ((uri
	       (if (assq 'retweeted-id status)
		   (twittering-get-status-url
		    (cdr (assq 'retweeted-user-screen-name status))
		    (cdr (assq 'retweeted-id status)))
		 (twittering-get-status-url
		  (cdr (assq 'user-screen-name status))
		  (cdr (assq 'id status))))))
	  (propertize str
		      'mouse-face 'highlight
		      'keymap twittering-mode-on-uri-map
		      'uri uri
		      'face 'twittering-uri-face
		      'front-sticky nil
		      'rear-nonsticky t))
      "")))

(defun twittering-make-fontified-tweet-text (str-expr regexp-hash regexp-atmark)
  (let ((regexp-str
	 (mapconcat
	  'identity
	  (list
	   ;; hashtag
	   (concat regexp-hash "\\([[:alpha:]0-9_-]+\\)")
	   ;; @USER/LIST
	   (concat regexp-atmark
		   "\\(\\([a-zA-Z0-9_-]+\\)/\\([a-zA-Z0-9_-]+\\)\\)")
	   ;; @USER
	   (concat regexp-atmark "\\([a-zA-Z0-9_-]+\\)")
	   ;; URI
	   "\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)")
	  "\\|")))
    `(let ((pos 0)
	   (str (copy-sequence ,str-expr)))
       (while (string-match ,regexp-str str pos)
	 (let* ((beg (match-beginning 0))
		(end (match-end 0))
		(range-and-properties
		 (cond
		  ((get-text-property beg 'face str)
		   ;; The matched substring has been already fontified.
		   ;; The fontification with entities must fontify the
		   ;; head of the matched string.
		   nil)
		  ((match-string 1 str)
		   ;; hashtag
		   (let* ((hashtag (match-string 1 str))
			  (spec-string
			   (twittering-make-hashtag-timeline-spec-string-direct
			    hashtag))
			  (url (twittering-get-search-url
				(concat "#" hashtag))))
		     (list
		      beg end
		      'mouse-face 'highlight
		      'keymap twittering-mode-on-uri-map
		      'uri url
		      'goto-spec spec-string
		      'face 'twittering-username-face)))
		  ((match-string 2 str)
		   ;; @USER/LIST
		   (let ((owner (match-string 3 str))
			 (list-name (match-string 4 str))
			 ;; Properties are added to the matched part only.
			 ;; The prefixes `twittering-regexp-atmark' will not
			 ;; be highlighted.
			 (beg (match-beginning 2)))
		     (list
		      beg end
		      'mouse-face 'highlight
		      'keymap twittering-mode-on-uri-map
		      'uri (twittering-get-list-url owner list-name)
		      'goto-spec
		      (twittering-make-list-timeline-spec-direct owner
								 list-name)
		      'face 'twittering-username-face)))
		  ((match-string 5 str)
		   ;; @USER
		   (let ((screen-name (match-string 5 str))
			 ;; Properties are added to the matched part only.
			 ;; The prefixes `twittering-regexp-atmark' will not
			 ;; be highlighted.
			 (beg (match-beginning 5)))
		     (list
		      beg end
		      'mouse-face 'highlight
		      'keymap twittering-mode-on-uri-map
		      'uri (twittering-get-status-url screen-name)
		      'screen-name-in-text screen-name
		      'goto-spec
		      (twittering-make-user-timeline-spec-direct screen-name)
		      'face 'twittering-uri-face)))
		  ((match-string 6 str)
		   ;; URI
		   (let ((uri (match-string 6 str)))
		     (list
		      beg end
		      'mouse-face 'highlight
		      'keymap twittering-mode-on-uri-map
		      'uri uri
		      'uri-origin 'explicit-uri-in-tweet
		      'face 'twittering-uri-face)))))
		(beg (if range-and-properties
			 (car range-and-properties)
		       beg))
		(end (if range-and-properties
			 (cadr range-and-properties)
		       end))
		(properties
		 `(,@(cddr range-and-properties)
		   front-sticky nil
		   rear-nonsticky t)))
	   (when range-and-properties
	     (add-text-properties beg end properties str))
	   (setq pos end)))
       str)))

(eval-and-compile
  (defsubst twittering-make-fontified-tweet-text-with-entity (status)
    (let* ((text (copy-sequence (cdr (assq 'text status))))
	   (text-length (length text))
	   (entities (cdr (assq 'entity status))))
      ;; hashtags
      (mapc (lambda (hashtag)
	      (let* ((start (cdr (assq 'start hashtag)))
		     (end (min (cdr (assq 'end hashtag)) text-length))
		     (tag (cdr (assq 'text hashtag)))
		     (spec-string
		      (twittering-make-hashtag-timeline-spec-string-direct tag)))
		(set-text-properties
		 start end
		 `(mouse-face
		   highlight
		   keymap ,twittering-mode-on-uri-map
		   uri ,(twittering-get-search-url (concat "#" tag))
		   goto-spec ,spec-string
		   face twittering-username-face
		   front-sticky nil
		   rear-nonsticky t)
		 text)))
	    (cdr (assq 'hashtags entities)))
      ;; mentions
      (mapc (lambda (mention)
	      (let ((start (cdr (assq 'start mention)))
		    (end (min (cdr (assq 'end mention)) text-length))
		    (screen-name (cdr (assq 'screen-name mention))))
		(set-text-properties
		 start end
		 `(mouse-face
		   highlight
		   keymap ,twittering-mode-on-uri-map
		   uri ,(twittering-get-status-url screen-name)
		   screen-name-in-text ,screen-name
		   goto-spec
		   ,(twittering-make-user-timeline-spec-direct screen-name)
		   face twittering-uri-face
		   front-sticky nil
		   rear-nonsticky t)
		 text)))
	    (cdr (assq 'mentions entities)))
      ;; urls
      (let ((offset 0))
	(mapc (lambda (url-info)
		(let* ((text-length (length text))
		       (start (cdr (assq 'start url-info)))
		       (end (cdr (assq 'end url-info)))
		       (url (cdr (assq 'url url-info)))
		       (expanded-url
			;; If the `url' is short and not wrapped,
			;; `expanded-url' is nil.
			(or (cdr (assq 'expanded-url url-info))
			    url))
		       (replacement
			(propertize
			 expanded-url
			 'mouse-face 'highlight
			 'keymap twittering-mode-on-uri-map
			 'uri url
			 'uri-origin 'explicit-uri-in-tweet
			 'expanded-uri expanded-url
			 'face 'twittering-uri-face
			 'front-sticky nil
			 'rear-nonsticky t)))
		  (setq text
			(concat
			 (substring text 0 (min (+ offset start) text-length))
			 replacement
			 (substring text (min (+ offset end) text-length))))
		  (setq offset
			(+ offset (- (length expanded-url) (- end start))))))
	      (cdr (assq 'urls entities))))
      text)))

(defun twittering-generate-format-table (status-sym prefix-sym)
  `(("%" . "%")
    ("}" . "}")
    ("#" . (cdr (assq 'id ,status-sym)))
    ("'" . (when (cdr (assq 'truncated ,status-sym))
	     "..."))
    ("c" .
     (let ((system-time-locale "C"))
       (format-time-string "%a %b %d %H:%M:%S %z %Y"
			   (cdr (assq 'created-at ,status-sym)))))
    ("d" . (cdr (assq 'user-description ,status-sym)))
    ("f" .
     (twittering-make-string-with-source-property
      (cdr (assq 'source ,status-sym)) ,status-sym))
    ("i" .
     (when (and twittering-icon-mode window-system)
       (let ((url
	      (cond
	       ((and twittering-use-profile-image-api
		     (eq twittering-service-method 'twitter)
		     (or (null twittering-convert-fix-size)
			 (member twittering-convert-fix-size '(48 73))))
		(let ((user (cdr (assq 'user-screen-name ,status-sym)))
		      (size
		       (if (or (null twittering-convert-fix-size)
			       (= 48 twittering-convert-fix-size))
			   "normal"
			 "bigger")))
		  (format "http://%s/%s/%s.xml?size=%s" twittering-api-host
			  (twittering-api-path "users/profile_image") user size)))
	       (t
		(cdr (assq 'user-profile-image-url ,status-sym))))))
	 (twittering-make-icon-string nil nil url))))
    ("j" . (cdr (assq 'user-id ,status-sym)))
    ("L" .
     (let ((location (or (cdr (assq 'user-location ,status-sym)) "")))
       (unless (string= "" location)
	 (concat " [" location "]"))))
    ("l" . (cdr (assq 'user-location ,status-sym)))
    ("p" . (when (cdr (assq 'user-protected ,status-sym))
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
		     'keymap twittering-mode-on-uri-map
		     'uri url
		     'front-sticky nil
		     'rear-nonsticky t)))
	 (when (and str url)
	   (concat " " (apply 'propertize str properties))))))
    ("R" .
     (let ((retweeted-by
	    (or (cdr (assq 'retweeting-user-screen-name ,status-sym)) "")))
       (unless (string= "" retweeted-by)
	 (concat " (retweeted by " retweeted-by ")"))))
    ("S" .
     (twittering-make-string-with-user-name-property
      (cdr (assq 'user-name ,status-sym)) ,status-sym))
    ("s" .
     (twittering-make-string-with-user-name-property
      (cdr (assq 'user-screen-name ,status-sym)) ,status-sym))
    ("T" .
     ,(twittering-make-fontified-tweet-text
       `(twittering-make-fontified-tweet-text-with-entity ,status-sym)
       twittering-regexp-hash twittering-regexp-atmark))
    ("t" .
     ,(twittering-make-fontified-tweet-text
       `(twittering-make-fontified-tweet-text-with-entity ,status-sym)
       twittering-regexp-hash twittering-regexp-atmark))
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
	  `((let* ((created-at (cdr (assq 'created-at ,status-sym)))
		   (url
		    (if (assq 'retweeted-id ,status-sym)
			(twittering-get-status-url
			 (cdr (assq 'retweeted-user-screen-name ,status-sym))
			 (cdr (assq 'retweeted-id ,status-sym)))
		      (twittering-get-status-url
		       (cdr (assq 'user-screen-name ,status-sym))
		       (cdr (assq 'id ,status-sym)))))
		   (properties
		    (list 'mouse-face 'highlight 'face 'twittering-uri-face
			  'keymap twittering-mode-on-uri-map
			  'uri url
			  'front-sticky nil
			  'rear-nonsticky t)))
	      (twittering-make-passed-time-string
	       nil nil created-at ,time-format properties))
	    . ,rest)))
       ((string-match "\\`C\\({\\([^}]*\\)}\\)?" following)
	(let ((time-format (or (match-string 2 following) "%H:%M:%S"))
	      (rest (substring following (match-end 0))))
	  `((let* ((created-at (cdr (assq 'created-at ,status-sym))))
	      (twittering-make-string-with-uri-property
	       (format-time-string ,time-format created-at) ,status-sym))
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
       ((string-match "\\`RT{" following)
	(let* ((str-after-brace (substring following (match-end 0)))
	       (pair (twittering-generate-formater-for-current-level
		      str-after-brace 'retweeting prefix-sym))
	       (braced-body (car pair))
	       (rest (cdr pair)))
	  `((when (assq 'retweeted-id ,status-sym)
	      (let ((retweeting
		     (mapcar (lambda (entry)
			       (let ((key-str (symbol-name (car entry)))
				     (value (cdr entry)))
				 (when (string-match "\\`retweeting-" key-str)
				   (let ((new-key
					  (intern (substring key-str
							     (match-end 0)))))
				     (cons new-key value)))))
			     ,status-sym)))
		(concat ,@braced-body)))
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
	 (let* ((common-properties (twittering-make-common-properties status))
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
  "Update the format for rendering a tweet.
If FORMAT-STR is nil, `twittering-status-format' is used in place of
FORMAT-STR.

If FORMAT-STR is valid as a format, `twittering-format-status-function'
is replaced by the result of `twittering-generate-format-status-function'
for FORMAT-STR.
If FORMAT-STR is invalid as a format, an error is signaled and
`twittering-format-status-function' is not updated."
  (let ((format-str (or format-str twittering-status-format)))
    (unless (string= format-str twittering-format-status-function-source)
      (let* ((before (get-buffer "*Compile-Log*"))
	     (func (twittering-generate-format-status-function format-str)))
	(cond
	 ((and func (functionp func))
	  (setq twittering-format-status-function-source format-str)
	  (setq twittering-format-status-function (byte-compile func))
	  (setq twittering-format-status-function-without-compile func)
	  (setq twittering-status-format format-str)
	  (let ((current (get-buffer "*Compile-Log*")))
	    (when (and (null before) current (= 0 (buffer-size current)))
	      (kill-buffer current))))
	 (t
	  (error "Invalid format: %s" format-str)
	  nil))))))

(defun twittering-format-status (status &optional prefix)
  "Format a STATUS by using `twittering-format-status-function'.
Specification of FORMAT-STR is described in the document for the
variable `twittering-status-format'."
  (funcall twittering-format-status-function status prefix))

(defun twittering-format-status-for-redisplay (beg end status &optional prefix)
  (twittering-format-status status prefix))

;;;;
;;;; Rendering
;;;;

(defun twittering-field-id< (field1 field2)
  (string< field1 field2))

(defun twittering-field-id= (field1 field2)
  (string= field1 field2))

(defun twittering-make-field-id-from-id (id &optional base-id)
  "Generate a field property for the tweet corresponding to ID.
Tweets are rendered in order of the field on `twittering-mode'.

If BASE-ID is non-nil, generate a field id for a tweet rendered
as a popped ancestor tweet by `twittering-show-replied-statuses'.
In the case, BASE-ID means the ID of the descendant."
  (let ((format-func (lambda (id) (format "%02d-%s" (length id) id))))
    (cond
     (base-id
      (format "O:%s:5:ancestor:%s"
	      (funcall format-func base-id)
	      (funcall format-func id)))
     (t
      (format "O:%s:8" (funcall format-func id))))))

(defun twittering-make-field-id (status &optional base-id)
  "Generate a field property for STATUS.
Tweets are rendered in order of the field on `twittering-mode'.

If BASE-ID is non-nil, generate a field id for a tweet rendered
as a popped ancestor tweet by `twittering-show-replied-statuses'.
In the case, BASE-ID means the ID of the descendant."
  (let ((id (cdr (assq 'id status))))
    (twittering-make-field-id-from-id id base-id)))

(defun twittering-make-properties-of-popped-ancestors (base-id)
  `(rendered-as ((ancestor-of . ,base-id))))

(defun twittering-make-field-id-of-timeline-oldest-end (spec-string)
  "Return the field ID for the oldest end.
This is given to a special field, header or footer, which does not correspond
to a tweet.
It must be less than IDs made by `twittering-make-field-id' for any other
normal fields in the meaning of `twittering-field-id<'."
  (format "H:%s" spec-string))

(defun twittering-make-field-id-of-timeline-latest-end (spec-string)
  "Return the field ID for the oldest end.
This is given to a special field, header or footer, which does not correspond
to a tweet.
It must be greater than IDs made by `twittering-make-field-id' for any other
normal fields in the meaning of `twittering-field-id<'."
  (format "U:%s" spec-string))

(defun twittering-field-id-is-timeline-oldest-end (field-id)
  "Return non-nil if FIELD-ID corresponds to the oldest end field.
Return non-nil if FIELD-ID is made by
`twittering-make-field-id-of-timeline-oldest-end'."
  (and (stringp field-id) (string= (substring field-id 0 2) "H:")))

(defun twittering-field-id-is-timeline-latest-end (field-id)
  "Return non-nil if FIELD-ID corresponds to the latest end field.
Return non-nil if FIELD-ID is made by
`twittering-make-field-id-of-timeline-latest-end'."
  (and (stringp field-id) (string= (substring field-id 0 2) "U:")))

(defun twittering-rendered-as-ancestor-status-p (&optional pos)
  "Return non-nil if the status at POS is rendered as an ancestor.
Ancestor statuses are rendered by `twittering-show-replied-statuses'."
  (let ((pos (or pos (point))))
    (assq 'ancestor-of (get-text-property pos 'rendered-as))))

(defun twittering-get-base-id-of-ancestor-at (&optional pos)
  "Return the base ID of a popped ancestor status rendered at POS.
If the status at POS is not a popped ancestor status or no status is
rendered at POS, return nil."
  (let ((pos (or pos (point))))
    (cdr (assq 'ancestor-of (get-text-property pos 'rendered-as)))))

(eval-and-compile
  (defsubst twittering-fill-string (str &optional adjustment prefix keep-newline)
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
		(fill-region (point-min) (point-max) nil t)
		(remove-text-properties (point-min) (point-max) '(hard nil)))
	    (insert (concat prefix str))
	    (fill-region-as-paragraph (point-min) (point-max)))
	  (buffer-substring (point-min) (point-max))))))

  (defsubst twittering-update-filled-string (beg end formater status prefix local-prefix &optional keep-newline)
    (let* ((str (twittering-fill-string (funcall formater status prefix)
					(length prefix) local-prefix
					keep-newline))
	   (next (next-single-property-change 0 'need-to-be-updated str)))
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

  (defsubst twittering-make-passed-time-string
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
			       (and beg (text-properties-at beg))))
	   (time-string
	    ;; Copy a string and restore properties.
	    (apply 'propertize time-string properties)))
      (if (< secs 84600)
	  (put-text-property 0 (length time-string)
			     'need-to-be-updated
			     `(twittering-make-passed-time-string
			       ,encoded-created-at ,time-format)
			     time-string)
	;; Remove the property required no longer.
	(remove-text-properties 0 (length time-string)
				'(need-to-be-updated nil)
				time-string))
      time-string)))

(defmacro twittering-render-a-field (pos field-id generator &optional without-separator)
  "Render a field on the current buffer managed by `twittering-mode'.
Insert a field to the position pointed by FIELD-ID. The position is searched
after POS. The string for the field is generated by the GENERATOR expression.
This function does not render the status if a status with the same field ID
as FIELD-ID is already rendered.
Return non-nil if the status is rendered. Otherwise, return nil."
  `(lexical-let ((pos ,pos)
		 (field-id ,field-id))
     (while
	 (let ((buf-field-id (get-text-property pos 'field)))
	   (if (and buf-field-id
		    (if twittering-reverse-mode
			(twittering-field-id< buf-field-id field-id)
		      (twittering-field-id< field-id buf-field-id)))
	       (let ((next-pos
		      (twittering-get-next-status-head pos)))
		 (setq pos (or next-pos (point-max)))
		 next-pos)
	     nil)))
     (goto-char pos)
     (unless (twittering-field-id= field-id (get-text-property pos 'field))
       (let ((formatted-status (propertize ,generator 'field ,field-id))
	     (separator (if ,without-separator
			    ""
			  "\n")))
	 (if (eq pos (point-max))
	     ;; Use `insert' only if no statuses are rendered on the below.
	     (insert formatted-status separator)
	   ;; Use `insert-before-markers' in order to keep
	   ;; which status is pointed by each marker.
	   (insert-before-markers formatted-status separator))
	 t))))

(defun twittering-render-timeline (buffer timeline-data &optional invoke-hook keep-point)
  "Render statuses for BUFFER and return the list of the rendered statuses.
TIMELINE-DATA is a list of statuses being rendered.
If INVOKE-HOOK is non-nil and one or more tweets are rendered, run hooks
specified by `twittering-new-tweets-rendered-hook'.
If KEEP-POINT is nil and BUFFER is empty, this function moves cursor positions
to the latest status.

This function returns a list of the statuses newly rendered by the invocation."
  (with-current-buffer buffer
    (let* ((spec (twittering-get-timeline-spec-for-buffer buffer))
	   (referring-id-table
	    (twittering-current-timeline-referring-id-table spec))
	   (timeline-data
	    ;; Collect visible statuses.
	    (let ((prev-id nil))
	      (remove
	       nil
	       (mapcar
		(lambda (status)
		  (let ((id (cdr (assq 'id status)))
			(retweeted-id (cdr (assq 'retweeted-id status))))
		    (if (twittering-status-id= prev-id id)
			;; `status' is equivalent the previous one.
			nil
		      (setq prev-id id)
		      (cond
		       ((null retweeted-id)
			;; `status' is not a retweet.
			status)
		       ((and retweeted-id
			     (twittering-status-id=
			      id (gethash retweeted-id referring-id-table)))
			;; `status' is the first retweet.
			status)
		       ((null (gethash retweeted-id referring-id-table))
			;; If the first ID referring the retweet is unknown,
			;; render it.
			;; This is necessary because a referring ID table
			;; of a composite timeline may lack information of
			;; some component timelines.
			status)
		       (t
			;; Otherwise, do not render it.
			nil)))))
		timeline-data))))
	   (timeline-data (if twittering-reverse-mode
			      (reverse timeline-data)
			    timeline-data))
	   (rendering-entire (null (twittering-get-first-status-head)))
	   (result-tweets nil)
	   (buffer-read-only nil))
      (twittering-update-status-format)
      (twittering-update-mode-line)
      (save-excursion
	(let ((pos (point-min))
	      (spec-string
	       (twittering-get-timeline-spec-string-for-buffer buffer)))
	  (cond
	   (rendering-entire
	    (let* ((latest-id
		    (twittering-make-field-id-of-timeline-latest-end
		     spec-string))
		   (oldest-id
		    (twittering-make-field-id-of-timeline-oldest-end
		     spec-string))
		   (footer-id
		    (if twittering-reverse-mode
			latest-id
		      oldest-id))
		   (header-id
		    (if twittering-reverse-mode
			oldest-id
		      latest-id)))
	      (setq
	       pos
	       (let ((footer
		      ;; To avoid adding a face to newlines.
		      (mapconcat
		       (lambda (substr)
			 (propertize substr
				     'face twittering-timeline-footer-face))
		       (split-string (or twittering-timeline-footer "") "\n")
		       "\n"))
		     (header
		      ;; To avoid adding a face to newlines.
		      (mapconcat
		       (lambda (substr)
			 (propertize substr
				     'face twittering-timeline-header-face))
		       (split-string (or twittering-timeline-header "") "\n")
		       "\n")))
		 (twittering-render-a-field (point-min) footer-id footer t)
		 (twittering-render-a-field (point-min) header-id header t)
		 (point)))))
	   (t
	    (setq pos (twittering-get-first-status-head))))
	  (goto-char pos)
	  (let* ((rendered-tweets
		  (remove nil
			  (mapcar
			   (lambda (status)
			     (when (twittering-render-a-field
				    (point)
				    (twittering-make-field-id status)
				    (twittering-format-status status))
			       (when twittering-default-show-replied-tweets
				 (twittering-show-replied-statuses
				  twittering-default-show-replied-tweets))
			       status))
			   timeline-data)))
		 (twittering-rendered-new-tweets
		  (if twittering-reverse-mode
		      (reverse rendered-tweets)
		    rendered-tweets))
		 (twittering-rendered-new-tweets-spec spec)
		 (twittering-rendered-new-tweets-spec-string spec-string))
	    (setq result-tweets rendered-tweets)
	    (when (and invoke-hook twittering-rendered-new-tweets)
	      (run-hooks 'twittering-new-tweets-rendered-hook)))))
      (debug-print (current-buffer))
      (cond
       ((and (not keep-point) rendering-entire)
	;; Go to the latest status of buffer after full insertion.
	(let ((dest (if twittering-reverse-mode
			(or (twittering-get-last-normal-field-head)
			    (twittering-get-last-status-head)
			    (point-max))
		      (or (twittering-get-first-normal-field-head)
			  (twittering-get-first-status-head)
			  (point-min))))
	      (window-list (get-buffer-window-list (current-buffer) nil t)))
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
       )
      result-tweets)
    ))

(defun twittering-rerender-timeline-all (buffer &optional restore-point)
  "Re-render statuses on BUFFER after clearing BUFFER.
If RESTORE-POINT is non-nil, positions on buffers bound to the same timeline
will be restored after rendering statuses."
  (with-current-buffer buffer
    (let* ((window-list (get-buffer-window-list (current-buffer) nil t))
	   (point-window-list
	    (mapcar (lambda (window)
		      (cons (window-point window) window))
		    window-list))
	   (original-pos (point)))
      (let ((buffer-read-only nil))
	(erase-buffer))
      (twittering-render-timeline
       (current-buffer) (twittering-current-timeline-data) nil restore-point)
      (when restore-point
	;; Restore points.
	(mapc (lambda (pair)
		(let* ((point (car pair))
		       (window (cdr pair))
		       (dest (max (point-max) point)))
		  (set-window-point window dest)))
	      point-window-list)
	(goto-char original-pos)))))

(defun twittering-retrieve-timeline (spec-string noninteractive api-arguments additional-info)
  "Retrieve and render a timeline specified by SPEC-STRING.
Retrieve a timeline specified by SPEC-STRING, which must be a timeline spec
string. Any timeline spec string including that for composite timeline can be
used as SPEC-STRING, though the primitive function `twittering-call-api'
accepts only a spec of a primary timeline.

NONINTERACTIVE is sent to the sentinel as a parameter `noninteractive' via
an argument `additional-info' of `twittering-call-api'.
API-ARGUMENTS is also sent to `twittering-call-api' as its argument
`args-alist'."
  (let ((spec (twittering-string-to-timeline-spec spec-string)))
    (cond
     ((not (twittering-account-authorized-p))
      ;; ignore any requests if the account has not been authorized.
      (message "No account for Twitter has been authorized.")
      t)
     ((and noninteractive (twittering-process-active-p spec))
      ;; ignore non-interactive request if a process is waiting for responses.
      t)
     ((twittering-timeline-spec-primary-p spec)
      (let* ((args
	      `(,@api-arguments
		(timeline-spec . ,spec)
		(timeline-spec-string . ,spec-string)
		(format . ,(when (require 'json nil t)
			     'json))
		(clean-up-sentinel
		 . ,(lambda (proc status connection-info)
		      (when (memq status '(exit signal closed failed))
			(twittering-release-process proc))))))
	     (additional-info
	      `(,@additional-info
		(noninteractive . ,noninteractive)
		(timeline-spec . ,spec)
		(timeline-spec-string . ,spec-string)))
	     (proc
	      (twittering-call-api 'retrieve-timeline args additional-info)))
	(when proc
	  (twittering-register-process proc spec spec-string)
	  (twittering-initialize-retrieval-count spec))))
     ((twittering-timeline-spec-composite-p spec)
      (mapc
       (lambda (spec)
	 (let* ((buffer (twittering-get-buffer-from-spec spec))
		(spec-string
		 (if buffer
		     (twittering-get-timeline-spec-string-for-buffer buffer)
		   (twittering-timeline-spec-to-string spec))))
	   (twittering-retrieve-timeline spec-string noninteractive
					 api-arguments additional-info)))
       (twittering-get-base-timeline-specs spec)))
     (t
      (let ((type (car spec)))
	(error "%s has not been supported yet" type))))))

(defun twittering-get-and-render-timeline (&optional noninteractive id spec spec-string)
  (let* ((spec (or spec (twittering-current-timeline-spec)))
	 (spec-string
	  (or spec-string (twittering-current-timeline-spec-string)))
	 (latest-status
	  ;; Assume that a list which was returned by
	  ;; `twittering-current-timeline-data' is sorted.
	  (car (twittering-current-timeline-data spec)))
	 (since_id (cdr-safe (assq 'id latest-status)))
	 (args `(,@(cond
		    (id `((max_id . ,id)))
		    (since_id `((since_id . ,since_id)))
		    (t nil)))))
    (twittering-retrieve-timeline spec-string noninteractive args nil)))

;;;;
;;;; Map function for statuses on buffer
;;;;

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

;;;;
;;;; Automatic redisplay of statuses on buffer
;;;;

(defun twittering-redisplay-status-on-buffer ()
  (mapc (lambda (buffer)
	  (unless (with-current-buffer buffer
		    (or (and (fboundp 'use-region-p) (use-region-p))
			(and transient-mark-mode mark-active)))
	    (twittering-redisplay-status-on-each-buffer buffer)))
	(twittering-get-buffer-list)))

(defun twittering-redisplay-status-on-each-buffer (buffer &optional prop)
  "Redisplay regions with the text property PROP on BUFFER."
  (let ((prop (or prop 'need-to-be-updated))
	(deactivate-mark deactivate-mark)
	(window-list (get-buffer-window-list buffer nil t))
	(marker (with-current-buffer buffer (point-marker)))
	(result nil))
    (with-current-buffer buffer
      (save-excursion
	(twittering-for-each-property-region
	 prop
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
	       (let ((common-properties
		      (twittering-get-common-properties beg)))
		 ;; Restore common properties.
		 (delete-region beg end)
		 (goto-char beg)
		 (insert (apply 'propertize updated-str common-properties)))
	       (twittering-restore-window-config-after-modification
		config beg end))))
	 buffer))
      (set-marker marker nil)
      (when (and result (eq (window-buffer) buffer))
	(let ((win (selected-window)))
	  (when (< result (window-start win))
	    (set-window-start win result))
	  (set-window-point win result))))))

;;;;
;;;; Display replied statuses
;;;;

(defun twittering-replied-statuses-visible-p (&optional pos)
  "Return non-nil if a replied status related to POS is visible.
Return non-nil if a replied status has been rendered at POS by
`twittering-show-replied-statuses'.
Return non-nil if a reply is rendered at POS and the replied statuses
has been rendered by `twittering-show-replied-statuses'.
Otherwise, return nil."
  (let* ((pos (twittering-get-current-status-head pos))
	 (id (twittering-get-id-at pos))
	 (prev (twittering-get-previous-status-head pos))
	 (next (twittering-get-next-status-head pos)))
    (when id
      ;; If ID is nil, it means that no normal tweets are rendered at POS.
      (or
       (twittering-get-base-id-of-ancestor-at pos)
       (and prev
	    (twittering-status-id=
	     id (twittering-get-base-id-of-ancestor-at prev)))
       (and next
	    (twittering-status-id=
	     id (twittering-get-base-id-of-ancestor-at next)))))))

(defun twittering-get-beginning-of-visible-replied-statuses (&optional pos)
  "Return the beginning position of visible replied statuses at POS.
If POS is nil, the current position is used instead.
If `twittering-show-replied-statuses' has rendered a replied status at POS,
return the beginning position of the replied statuses with the common base
status.
If a reply is rendered at POS and its ancestors has been rendered by
`twittering-show-replied-statuses', return the beginning position of the
replied statuses.
Otherwise, return nil."
  (let* ((pos (or pos (point)))
	 (base-id (twittering-get-base-id-of-ancestor-at pos)))
    (cond
     (base-id
      ;; A replied status is rendered at POS.
      (while
	  (let* ((prev (twittering-get-previous-status-head pos))
		 (prev-base-id
		  (when prev
		    (twittering-get-base-id-of-ancestor-at prev))))
	    (and prev prev-base-id
		 (twittering-status-id= base-id prev-base-id)
		 (setq pos prev))))
      (or pos (point-min)))
     ((twittering-replied-statuses-visible-p pos)
      ;; A reply is rendered at POS and its replied statuses are visible.
      (if twittering-reverse-mode
	  (twittering-get-beginning-of-visible-replied-statuses
	   (twittering-get-previous-status-head pos))
	(twittering-get-next-status-head pos)))
     (t
      nil))))

(defun twittering-get-end-of-visible-replied-statuses (&optional pos)
  "Return the end position of visible replied statuses at POS.
If POS is nil, the current position is used instead.
If `twittering-show-replied-statuses' has rendered a replied status at POS,
return the end position of the replied statuses with the common base status.
If a reply is rendered at POS and its ancestors has been rendered by
`twittering-show-replied-statuses', return the beginning position of the
replied statuses.
Otherwise, return nil."
  (let* ((pos (or pos (point)))
	 (base-id (twittering-get-base-id-of-ancestor-at pos)))
    (cond
     (base-id
      ;; A replied status is rendered at POS.
      (while
	  (let ((current-base-id (twittering-get-base-id-of-ancestor-at pos)))
	    (and current-base-id
		 (twittering-status-id= base-id current-base-id)
		 (setq pos (twittering-get-next-status-head pos)))))
      (or pos (point-max)))
     ((twittering-replied-statuses-visible-p pos)
      ;; A reply is rendered at POS and its replied statuses are visible.
      (if twittering-reverse-mode
	  (twittering-get-current-status-head pos)
	(twittering-get-end-of-visible-replied-statuses
	 (twittering-get-next-status-head pos))))
     (t
      nil))))

(defun twittering-render-replied-statuses (&optional pos count)
  "Render replied statuses on the position specified by POS.
If POS is nil, the current position is used instead.
If COUNT is a positive integer, it specifies the number of rendered statuses.
If COUNT is nil, all ancestor statuses that have been already retrieved are
rendered.

Return non-nil if one or more statuses are rendered.
Return nil if no statuses are rendered."
  (let* ((pos (or pos (point)))
	 (id
	  ;; nil if no normal statuses are rendered at POS.
	  (twittering-get-id-at pos))
	 (replied-status-are-visible
	  (when id
	    (twittering-replied-statuses-visible-p pos)))
	 (base-id (if replied-status-are-visible
		      (or
		       ;; If a replied status is rendered at POS.
		       (twittering-get-base-id-of-ancestor-at pos)
		       ;; If the base reply is rendered at POS.
		       id)
		    id))
	 (statuses
	  (when base-id
	    (twittering-get-replied-statuses base-id (if (numberp count)
							 count))))
	 (statuses (if twittering-reverse-mode
		       statuses
		     (reverse statuses))))
    (cond
     ((null id)
      ;; No normal statuses are rendered here.
      nil)
     (statuses
      (let ((pos
	     (cond
	      ((twittering-replied-statuses-visible-p pos)
	       ;; Some replied statuses have been already rendered.
	       (twittering-get-beginning-of-visible-replied-statuses pos))
	      (twittering-reverse-mode
	       (twittering-get-current-status-head pos))
	      (t
	       (or (twittering-get-next-status-head pos)
		   (point-max)))))
	    (prefix "  ")
	    (buffer-read-only nil))
	(save-excursion
	  (goto-char pos)
	  (mapc
	   (lambda (status)
	     (twittering-render-a-field
	      (point)
	      (twittering-make-field-id status base-id)
	      (let ((formatted-status (twittering-format-status status prefix))
		    (field-properties
		     (twittering-make-properties-of-popped-ancestors base-id)))
		(add-text-properties 0 (length formatted-status)
				     field-properties formatted-status)
		formatted-status)))
	   statuses)
	  t)))
     (t
      nil))))

(defun twittering-render-a-status-with-delay (beg end id prefix)
  "Render a status with a delay.
It is assumed that this function is used as a property value that is
processed by the function `twittering-redisplay-status-on-each-buffer'."
  (let ((status (twittering-find-status id)))
    (when status
      (let ((properties (and beg (text-properties-at beg))))
	(apply 'propertize (twittering-format-status status prefix)
	       properties)))))

(defun twittering-toggle-or-retrieve-replied-statuses ()
  "Show/Hide all of replied statuses or retrieve a replied status.
If the cursor points to a reply or one of expanded replied statuses and
some of ancestor replied statuses have been already retrieved but they have
not been rendered, render them.
If the cursor points to a reply or one of expanded replied statuses and
all of retrieved ancestor statuses have been already rendered but the oldest
one of them is also a reply, retrieve the replied status.
If the cursor points to a reply or one of expanded replied statuses and
all of ancestor replied statuses have been already rendered, hide them by
`twittering-hide-replied-statuses'."
  (interactive)
  (let* ((pos (point))
	 (pos
	  ;; POS points to the head of the direct reply of the status being
	  ;; retrieved.
	  (cond
	   ((twittering-replied-statuses-visible-p pos)
	    ;; If some replied statuses are visible, find the edge.
	    (if twittering-reverse-mode
		(twittering-get-beginning-of-visible-replied-statuses pos)
	      (twittering-get-previous-status-head
	       (twittering-get-end-of-visible-replied-statuses pos))))
	   (t
	    (twittering-get-current-status-head pos))))
	 (id (twittering-get-id-at pos))
	 (status (twittering-find-status id))
	 (reply-id (cdr (assq 'in-reply-to-status-id status)))
	 (reply-username (cdr (assq 'in-reply-to-screen-name status)))
	 (base-id (or (twittering-get-base-id-of-ancestor-at pos)
		      id)))
    (cond
     ((twittering-find-status reply-id)
      ;; The status corresponding to REPLY-ID has been already retrieved
      ;; but it has not been rendered.
      ;;
      ;; `twittering-render-replied-statuses' additionally renders all
      ;; of already retrieved statuses.
      (twittering-render-replied-statuses))
     (reply-id
      (let* ((pos
	      ;; POS points to the position where the new field will be
	      ;; inserted.
	      (if twittering-reverse-mode
		  pos
		(or (twittering-get-next-status-head pos)
		    (point-max))))
	     (field-id (twittering-make-field-id-from-id reply-id base-id))
	     (prefix "  ")
	     (label "[RETRIEVING...]")
	     (symbol-for-redisplay 'waiting-for-retrieval)
	     (properties
	      `(,@(twittering-make-properties-of-popped-ancestors base-id)
		,symbol-for-redisplay
		(twittering-render-a-status-with-delay ,reply-id ,prefix)))
	     (str (apply 'propertize (concat prefix label) properties))
	     (buffer-read-only nil))
	(twittering-call-api
	 'retrieve-single-tweet
	 `((id . ,reply-id)
	   (username . ,reply-username)
	   (format . ,(when (require 'json nil t)
			'json))
	   (sentinel . twittering-retrieve-single-tweet-sentinel))
	 `((buffer . ,(current-buffer))
	   (property-to-be-redisplayed . ,symbol-for-redisplay)))
	(save-excursion
	  (goto-char pos)
	  (twittering-render-a-field (point) field-id str))
	(goto-char pos)))
     ((twittering-replied-statuses-visible-p)
      ;; All ancestor replied statuses have been rendered.
      (twittering-hide-replied-statuses))
     (t
      ;; The pointed status is not a reply.
      (message "This status is not a reply.")))))

(defun twittering-show-replied-statuses (&optional count interactive)
  (interactive)
  (cond
   ((twittering-replied-statuses-visible-p)
    (when interactive
      (message "The replied statuses were already showed.")))
   ((twittering-render-replied-statuses (point) count)
    t)
   (t
    ;; Failed to render replied statuses.
    (when interactive
      (let ((base-id (twittering-get-id-at)))
	(if (twittering-have-replied-statuses-p base-id)
	    (message "The status this replies to has not been fetched yet.")
	  (message "This status is not a reply.")))))))

(defun twittering-hide-replied-statuses (&optional interactive)
  (interactive)
  (cond
   ((twittering-replied-statuses-visible-p)
    (let* ((pos (twittering-get-current-status-head (point)))
	   (base-id (or (twittering-get-base-id-of-ancestor-at pos)
			(twittering-get-id-at pos)))
	   (pointing-to-base-status
	    (not (twittering-rendered-as-ancestor-status-p pos)))
	   (beg (twittering-get-beginning-of-visible-replied-statuses pos))
	   (end (twittering-get-end-of-visible-replied-statuses pos))
	   (buffer-read-only nil))
      (unless pointing-to-base-status
	(goto-char (if twittering-reverse-mode
		       beg
		     (or (twittering-get-previous-status-head beg)
			 (point-min)))))
      (delete-region beg end)))
   (interactive
    (message "The status this replies to was already hidden."))))

(defun twittering-toggle-show-replied-statuses ()
  (interactive)
  (if (twittering-replied-statuses-visible-p)
      (twittering-hide-replied-statuses (interactive-p))
    (twittering-show-replied-statuses twittering-show-replied-tweets
				      (interactive-p))))

;;;;
;;;; Unread statuses info
;;;;

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
	       twittering-unread-status-info))))))

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
  (let* ((buffer (twittering-get-buffer-from-spec
		  twittering-rendered-new-tweets-spec))
	 (current (or (cadr (assq buffer twittering-unread-status-info)) 0))
	 (result (+ current (length twittering-rendered-new-tweets))))
    (when buffer
      (twittering-set-number-of-unread buffer result))))

(defun twittering-enable-unread-status-notifier ()
  "Enable a notifier of unread statuses on `twittering-mode'."
  (interactive)
  (setq twittering-unread-status-info
	(mapcar (lambda (buffer) `(,buffer ,0))
		(twittering-get-buffer-list)))
  (add-hook 'twittering-new-tweets-rendered-hook
	    'twittering-update-unread-status-info)
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

;;;;
;;;; Timer
;;;;

(defvar twittering-idle-timer-for-redisplay nil)

(defun twittering-timer-action (func)
  (let ((buf (twittering-get-active-buffer-list)))
    (if (null buf)
	(twittering-stop)
      (funcall func)
      )))

(defun twittering-run-on-idle (idle-interval func &rest args)
  "Run FUNC the next time Emacs is idle for IDLE-INTERVAL.
Even if Emacs has been idle longer than IDLE-INTERVAL, run FUNC immediately.
Since immediate invocation requires `current-idle-time', it is available
on Emacs 22 and later.
FUNC is called as (apply FUNC ARGS)."
  (let ((sufficiently-idling
	 (and (fboundp 'current-idle-time)
	      (current-idle-time)
	      (time-less-p (seconds-to-time idle-interval)
			   (current-idle-time)))))
    (if (not sufficiently-idling)
	(apply 'run-with-idle-timer idle-interval nil func args)
      (apply func args)
      nil)))

(defun twittering-run-repeatedly-on-idle (check-interval var idle-interval func &rest args)
  "Run FUNC every time Emacs is idle for IDLE-INTERVAL.
Even if Emacs remains idle longer than IDLE-INTERVAL, run FUNC every
CHECK-INTERVAL seconds. Since this behavior requires `current-idle-time',
invocation on long idle time is available on Emacs 22 and later.
VAR is a symbol of a variable to which the idle-timer is bound.
FUNC is called as (apply FUNC ARGS)."
  (apply 'run-at-time "0 sec"
	 check-interval
	 (lambda (var idle-interval func &rest args)
	   (let ((registerd (symbol-value var))
		 (sufficiently-idling
		  (and (fboundp 'current-idle-time)
		       (current-idle-time)
		       (time-less-p (seconds-to-time idle-interval)
				    (current-idle-time)))))
	     (when (or (not registerd) sufficiently-idling)
	       (when (and registerd sufficiently-idling)
		 (cancel-timer (symbol-value var))
		 (apply func args))
	       (set var (apply 'run-with-idle-timer idle-interval nil
			       (lambda (var func &rest args)
				 (set var nil)
				 (apply func args))
			       var func args)))))
	 var idle-interval func args))

(defun twittering-start (&optional action)
  (interactive)
  (unless twittering-timer
    (let ((action (or action #'twittering-update-active-buffers)))
      ;; Update all active timelines forcibly.
      (twittering-update-active-buffers t)
      (setq twittering-timer
	    (run-at-time (format "%d sec" twittering-timer-interval)
			 twittering-timer-interval
			 #'twittering-timer-action action))))
  (unless twittering-timer-for-redisplaying
    (setq twittering-timer-for-redisplaying
	  (twittering-run-repeatedly-on-idle
	   (* 2 twittering-timer-interval-for-redisplaying)
	   'twittering-idle-timer-for-redisplay
	   twittering-timer-interval-for-redisplaying
	   #'twittering-redisplay-status-on-buffer))))

(defun twittering-stop ()
  (interactive)
  (when twittering-timer
    (cancel-timer twittering-timer)
    (setq twittering-timer nil))
  (when twittering-timer-for-redisplaying
    (when twittering-idle-timer-for-redisplay
      (cancel-timer twittering-idle-timer-for-redisplay)
      (setq twittering-idle-timer-for-redisplay))
    (cancel-timer twittering-timer-for-redisplaying)
    (setq twittering-timer-for-redisplaying nil)))

(defun twittering-get-relative-interval (spec)
  (let* ((spec-string (twittering-timeline-spec-to-string spec))
	 (normalized-alist
	  (apply 'append
		 (mapcar
		  (lambda (entry)
		    (let ((interval (car (last entry)))
			  (regexp-list (butlast entry 1)))
		      (when (integerp interval)
			(mapcar (lambda (regexp) `(,regexp . ,interval))
				regexp-list))))
		  twittering-relative-retrieval-interval-alist)))
	 (rest normalized-alist)
	 (current normalized-alist)
	 (result 0))
    (while (not
	    (or (and (stringp (car current))
		     (string-match (car current) spec-string))
		(eq t (car current))))
      (setq current (car rest))
      (setq rest (cdr rest)))
    (if (integerp (cdr current))
	(cdr current)
      ;; The default relative interval is 1.
      1)))

(defun twittering-get-retrieval-count (spec)
  (cdr (assoc spec twittering-relative-retrieval-count-alist)))

(defun twittering-set-retrieval-count (spec count)
  (let ((current (assoc spec twittering-relative-retrieval-count-alist)))
    (if (null current)
	(add-to-list 'twittering-relative-retrieval-count-alist
		     `(,spec . ,count))
      (setcdr current count))))

(defun twittering-initialize-retrieval-count (spec)
  (twittering-set-retrieval-count spec
				  (twittering-get-relative-interval spec)))

(defun twittering-update-active-buffers (&optional force noninteractive)
  "Update active buffers managed by `twittering-mode' at a certain interval.

If FORCE is nil, each active buffer is updated at a relative interval
determined by `twittering-relative-retrieval-interval-alist'.
If a relative interval of a timeline is 3, the timeline is updated once
by three invocations of this function.

If FORCE is non-nil, all active buffers are updated forcibly."
  (when (twittering-account-authorized-p)
    (twittering-update-service-configuration)
    (let* ((buffer-list (twittering-get-active-buffer-list))
	   (primary-spec-list
	    (twittering-remove-duplicates
	     (apply 'append
		    (mapcar
		     (lambda (buffer)
		       (twittering-get-primary-base-timeline-specs
			(twittering-get-timeline-spec-for-buffer buffer)))
		     buffer-list)))))
      (mapc
       (lambda (spec)
	 (let ((current
		(if force
		    1
		  (twittering-get-retrieval-count spec))))
	   (cond
	    ((null current)
	     ;; Initialize the count if no entry for the primary timeline
	     ;; exists.
	     (twittering-initialize-retrieval-count spec))
	    ((and (integerp current) (= 0 current))
	     ;; Do nothing.
	     )
	    ((and (integerp current) (= 1 current))
	     ;; Retrieve the timeline and initialize count.
	     (let ((spec-string
		    (twittering-timeline-spec-to-string spec)))
	       (twittering-get-and-render-timeline
		noninteractive nil spec spec-string)
	       (twittering-initialize-retrieval-count spec)))
	    ((and (integerp current) (< 1 current))
	     ;; Decrement count.
	     (twittering-set-retrieval-count spec (1- current)))
	    (t
	     nil))))
       primary-spec-list))))

;;;;
;;;; Keymap
;;;;

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
      (define-key km (kbd "C-<down-mouse-3>") 'mouse-set-point)
      (define-key km (kbd "C-<mouse-3>") 'twittering-push-tweet-onto-kill-ring)
      (define-key km (kbd "C-c C-v") 'twittering-view-user-page)
      (define-key km (kbd "C-c D") 'twittering-delete-status)
      (define-key km (kbd "C-c C-w") 'twittering-delete-status)
      (define-key km (kbd "a") 'twittering-toggle-activate-buffer)
      (define-key km (kbd "g") 'twittering-current-timeline)
      (define-key km (kbd "u") 'twittering-update-status-interactive)
      (define-key km (kbd "U") 'twittering-push-uri-onto-kill-ring)
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
      (define-key km (kbd "G") 'twittering-goto-last-status)
      (define-key km (kbd "H") 'twittering-goto-first-status)
      (define-key km (kbd "i") 'twittering-icon-mode)
      (define-key km (kbd "r") 'twittering-toggle-show-replied-statuses)
      (define-key km (kbd "R") 'twittering-toggle-or-retrieve-replied-statuses)
      (define-key km (kbd "t") 'twittering-toggle-proxy)
      (define-key km (kbd "C-c C-p") 'twittering-toggle-proxy)
      (define-key km (kbd "q") 'twittering-kill-buffer)
      (define-key km (kbd "C-c C-q") 'twittering-search)
      nil))

(let ((km twittering-mode-menu-on-uri-map))
  (when km
    (define-key km [ct] '("Copy tweet" . twittering-push-tweet-onto-kill-ring))
    (define-key km [cl] '("Copy link" . twittering-push-uri-onto-kill-ring))
    (define-key km [ll] '("Load link" . twittering-click))
    (let ((km-on-uri twittering-mode-on-uri-map))
      (when km-on-uri
	(define-key km-on-uri (kbd "C-<down-mouse-3>") 'mouse-set-point)
	(define-key km-on-uri (kbd "C-<mouse-3>") km)))))

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


;;;;
;;;; Initialization
;;;;

(defvar twittering-initialized nil)
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
    (defface twittering-timeline-header-face
      `((t ,(face-attr-construct
	     (if (facep 'font-lock-preprocessor-face)
		 'font-lock-preprocessor-face
	       'bold))))
      "Timeline header on twittering-mode" :group 'faces)
    (defface twittering-timeline-footer-face
      `((t ,(face-attr-construct
	     (if (facep 'font-lock-preprocessor-face)
		 'font-lock-preprocessor-face
	       'bold))))
      "Timeline footer on twittering-mode" :group 'faces)
    (twittering-update-status-format)
    (when twittering-use-convert
      (if (null twittering-convert-program)
	  (setq twittering-use-convert nil)
	(with-temp-buffer
	  (let ((coding-system-for-read 'iso-safe)
		(coding-system-for-write 'iso-safe)
		;; Bind `default-directory' to the temporary directory
		;; because it is possible that the directory pointed by
		;; `default-directory' has been already removed.
		(default-directory temporary-file-directory))
	    (call-process twittering-convert-program nil (current-buffer) nil
			  "-version")
	    (goto-char (point-min))
	    (if (null (search-forward-regexp "\\(Image\\|Graphics\\)Magick"
					     nil t))
		(setq twittering-use-convert nil))))))
    (twittering-setup-proxy)
    (when twittering-use-icon-storage
      (cond
       ((require 'jka-compr nil t)
	(twittering-load-icon-properties)
	(add-hook 'kill-emacs-hook 'twittering-save-icon-properties))
       (t
	(setq twittering-use-icon-storage nil)
	(error "Disabled icon-storage because it failed to load jka-compr."))))
    (cond
     ((and
       (boundp 'twittering-sign-simple-string)
       twittering-sign-simple-string
       (or (not (boundp 'twittering-sign-string-function))
	   (null twittering-sign-string-function))
       (eq twittering-edit-skeleton 'none)
       (or (null twittering-edit-skeleton-footer)
	   (string= twittering-edit-skeleton-footer "")))
      ;; Configure `twittering-edit-skeleton' as an alternative of
      ;; `twittering-sign-simple-string'.
      (twittering-edit-skeleton-change-footer
       (format " [%s]" twittering-sign-simple-string))
      (setq twittering-edit-skeleton 'footer)
      (message "Warning: `twittering-sign-simple-string' is obsolete. Use `twittering-edit-skeleton-footer' instead."))
     ((or (boundp 'twittering-sign-simple-string)
	  (boundp 'twittering-sign-string-function))
      (message "Warning: `twittering-sign-simple-string' and `twittering-sign-string-function' are obsolete. Use the new feature `twittering-edit-skeleton'.")
      ))
    (add-hook 'twittering-new-tweets-rendered-hook
	      'twittering-jojo-mode-hook-function)
    (run-hooks 'twittering-mode-init-hook)
    (setq twittering-initialized t)))

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

  ;; Prevent the field property attached to tweets from interfering
  ;; the cursor motion based on logical lines.
  (make-local-variable 'inhibit-field-text-motion)
  (setq inhibit-field-text-motion t)

  (make-local-variable 'twittering-timeline-spec)
  (make-local-variable 'twittering-timeline-spec-string)
  (make-local-variable 'twittering-active-mode)
  (make-local-variable 'twittering-icon-mode)
  (make-local-variable 'twittering-jojo-mode)
  (make-local-variable 'twittering-reverse-mode)

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
  (let ((timeline-spec-list
	 (if (listp twittering-initial-timeline-spec-string)
	     twittering-initial-timeline-spec-string
	   (cons twittering-initial-timeline-spec-string nil))))
    (twittering-visit-timeline (car timeline-spec-list))
    (when (twittering-account-authorized-p)
      (mapc 'twittering-visit-timeline (cdr timeline-spec-list)))))

;;;;
;;;; Preparation for invoking APIs
;;;;

(defun twittering-api-invocation-is-ready-p ()
  "Return non-nil if the preparation for invoking APIs has been completed."
  (and
   ;; The global variables are initialized.
   twittering-initialized
   ;; A connection method is prepared.
   (let ((use-ssl (or twittering-use-ssl twittering-oauth-use-ssl)))
     (twittering-lookup-connection-type use-ssl))
   ;; The account has been already authorized.
   (twittering-account-authorized-p)))

(defun twittering-ensure-preparation-for-api-invocation ()
  "Ensure prerequisites for invoking APIs. Return non-nil in success.
If prerequisites has been already satisifed, just return non-nil.
If prerequisites are not satisfied, this function try to satisfy them.
Then, return non-nil if they has been satisfied and return nil otherwise."
  (twittering-initialize-global-variables-if-necessary)
  (and (twittering-ensure-connection-method)
       (twittering-ensure-private-info)
       (twittering-ensure-account-verification)))

;;;;
;;;; Edit mode skeleton
;;;;

(defvar twittering-edit-skeleton-footer "")
(defvar twittering-edit-skeleton-footer-history nil)
(defvar twittering-edit-skeleton-alist
  '((none . nil)
    (footer . ((nil _ twittering-edit-skeleton-footer)))
    (footer-only-normal
     . ((nil _ twittering-edit-skeleton-footer) . normal))
    (inherit-hashtags
     . [(twittering-edit-skeleton-inherit-hashtags . normal)
	(twittering-edit-skeleton-inherit-hashtags . reply)])
    (inherit-mentions
     . (twittering-edit-skeleton-inherit-mentions . reply))
    (inherit-any
     . [(twittering-edit-skeleton-inherit-mentions . reply)
	(twittering-edit-skeleton-inherit-hashtags . normal)
	(twittering-edit-skeleton-inherit-hashtags . reply)]))
  "*Alist of skeletons performed on `twittering-update-status-interactive'.
A key of the alist is a symbol and each value is nil, (SKELETON . PRED),
 (FUNC . PRED) or a vector of them.

When invoking `twittering-update-status-interactive', the value corresponding
to the key specified `twittering-edit-skeleton' are performed.

The value like (SKELETON . PRED) or (FUNC . PRED) is performed when the
current context matches with PRED.
PRED is nil, a symbol or a function.
If PRED is nil, the value is unconditionally performed.
If PRED is a symbol, the value is performed only when it equals to the
type of the tweet being edited. The type is one of 'direct-message, 'normal,
'organic-retweet and 'reply.
If PRED is a function, the value is performed only when the predicate
function PRED returns non-nil. PRED is invoked with three arguments
TWEET-TYPE, IN-REPLY-TO-ID and CURRENT-SPEC.
TWEET-TYPE is a symbol, which is one of 'direct-message, 'normal,
'organic-retweet and 'reply, specifying which type of tweet will be edited.
If the tweet will be edited as a reply or an organic retweet, IN-REPLY-TO-ID
is a string specifying the replied tweet. Otherwise, IN-REPLY-TO-ID is nil.
CURRENT-SPEC specifies where the action of posting a tweet is performed.
If the action is performed on a twittering-mode buffer, CURRENT-SPEC is
a timeline spec string of the buffer.
If the action is performed on other buffers, CURRENT-SPEC is nil.
If the option IGNORE-CURRENT-SPEC for `twittering-update-status' is non-nil,
CURRENT-SPEC is also nil.

If PRED matches the current context, the value is performed as follows.
The value like (SKELETON . PRED) is performed by directly using SKELETON as an
argument of `skeleton-insert'.
The value like (FUNC . PRED) is performed by invoking FUNC with three
arguments, TWEET-TYPE, IN-REPLY-TO-ID and CURRENT-SPEC as same as PRED.

If the value is a vector, each element is performed in order of elements
in the vector.

Note that the effective skeleton is invoked after inserting a
recipient.")
(defvar twittering-edit-skeleton 'none
  "*A symbol specifying an effective skeleton.
It must be one of a symbol in `twittering-edit-skeleton-alist'.
When entering `twittering-edit-mode', the skeletons in the specified
entry in `twittering-edit-skeleton-alist' are performed.")

(defun twittering-switch-edit-skeleton ()
  (interactive)
  (let ((skeleton-keys
	 (mapcar (lambda (entry) (symbol-name (car entry)))
		 twittering-edit-skeleton-alist))
	(current (symbol-name (or twittering-edit-skeleton 'none))))
    (let ((selected
	   (twittering-completing-read
	    (format "Skeleton (%s): " current)
	    skeleton-keys nil t nil nil current)))
      (when selected
	(setq twittering-edit-skeleton (intern selected)))))
  (when (null twittering-edit-skeleton)
    (setq twittering-edit-skeleton 'none))
  (message "Current skeleton: %s" twittering-edit-skeleton))

(defun twittering-edit-skeleton-change-footer (&optional footer-str)
  (interactive)
  (let ((footer-str
	 (or footer-str
	     (read-from-minibuffer "Footer: " twittering-edit-skeleton-footer
				   nil nil
				   'twittering-edit-skeleton-footer-history))))
    (when footer-str
      (setq twittering-edit-skeleton-footer footer-str)))
  (message "Current footer: [%s]" twittering-edit-skeleton-footer))

(defun twittering-edit-skeleton-insert-base (&optional tweet-type in-reply-to-id current-spec)
  (let ((entry
	 (cdr (assq twittering-edit-skeleton twittering-edit-skeleton-alist))))
    (when entry
      (require 'skeleton)
      (let ((skeletons (if (vectorp entry)
			   entry
			 (list entry))))
	(mapcar (lambda (def)
		  (let ((skeleton-or-func (car def))
			(pred (cdr def)))
		    (when (or (null pred)
			      (and (functionp pred)
				   (funcall pred tweet-type in-reply-to-id))
			      (and (symbolp pred)
				   (eq pred tweet-type)))
		      (cond
		       ((functionp skeleton-or-func)
			(funcall skeleton-or-func tweet-type in-reply-to-id
				 current-spec))
		       (t
			(skeleton-insert skeleton-or-func))))))
		skeletons)))))

(defun twittering-edit-skeleton-insert (&optional tweet-type in-reply-to-id current-spec)
  (if (> 22 emacs-major-version)
      ;; This prevents Emacs21 from inserting skeletons before the cursor.
      (let ((current (point))
	    (pair (with-temp-buffer
		    (twittering-edit-skeleton-insert-base tweet-type
							  in-reply-to-id
							  current-spec)
		    `(,(buffer-string) . ,(point)))))
	(insert (car pair))
	(goto-char (+ -1 current (cdr pair))))
    (twittering-edit-skeleton-insert-base tweet-type in-reply-to-id
					  current-spec)))

(defun twittering-edit-skeleton-inherit-hashtags (tweet-type in-reply-to-id current-spec)
  (cond
   (in-reply-to-id
    (let* ((status (twittering-find-status in-reply-to-id))
	   (text (cdr (assq 'text status)))
	   (hashtags
	    (twittering-extract-matched-substring-all
	     (concat twittering-regexp-hash
		     "\\([[:alpha:]0-9_-]+\\)")
	     text))
	   (footer
	    (mapconcat (lambda (tag) (concat "#" tag))
		       hashtags " ")))
      (when hashtags
	(skeleton-insert `(nil _ " " ,footer)))))
   ((twittering-timeline-spec-is-search-p current-spec)
    (let* ((query-string
	    (twittering-extract-query-string-from-search-timeline-spec
	     current-spec))
	   (hashtag-list
	    (twittering-extract-matched-substring-all
	     (concat "\\(" twittering-regexp-hash "[[:alpha:]0-9_-]+\\)")
	     query-string)))
      (when hashtag-list
	(let ((footer (mapconcat 'identity hashtag-list " ")))
	  (skeleton-insert `(nil _ " " ,footer))))))))

(defun twittering-edit-skeleton-inherit-mentions (tweet-type in-reply-to-id current-spec)
  (when in-reply-to-id
    (let* ((status (twittering-find-status in-reply-to-id))
	   (text (cdr (assq 'text status)))
	   (recipient (cdr (assq 'user-screen-name status)))
	   (mentions
	    (twittering-extract-matched-substring-all
	     (concat twittering-regexp-atmark
		     "\\([a-zA-Z0-9_-]+\\)")
	     text))
	   (reduced-mentions
	    (remove nil
		    (mapcar
		     (lambda (mention)
		       (unless (or (string= mention recipient)
				   (string= mention (twittering-get-username)))
			 mention))
		     mentions))))
      (when reduced-mentions
	(let ((header (mapconcat (lambda (user) (concat "@" user))
				 reduced-mentions " ")))
	  (skeleton-insert `(nil ,header " " _)))))))

;;;;
;;;; Edit mode
;;;;

(defvar twittering-edit-buffer "*twittering-edit*")
(defvar twittering-pre-edit-window-configuration nil)
(defvar twittering-edit-history nil)
(defvar twittering-edit-local-history nil)
(defvar twittering-edit-local-history-idx nil)
(defvar twittering-warning-overlay nil)

(define-derived-mode twittering-edit-mode nil "twmode-status-edit"
  (use-local-map twittering-edit-mode-map)

  ;; Prevent `global-font-lock-mode' enabling `font-lock-mode'.
  ;; This technique is derived from `lisp/bs.el' distributed with Emacs 22.2.
  (make-local-variable 'font-lock-global-modes)
  (setq font-lock-global-modes '(not twittering-edit-mode))

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
    (define-key km (kbd "C-c C-r") 'twittering-edit-toggle-reply)
    (define-key km (kbd "M-n") 'twittering-edit-next-history)
    (define-key km (kbd "M-p") 'twittering-edit-previous-history)
    (define-key km (kbd "<f4>") 'twittering-edit-replace-at-point)))

(defun twittering-effective-length (str &optional short-length-http short-length-https)
  "Return the effective length of STR with taking account of shortening URIs.

The returned length is calculated with taking account of shortening URIs
if `twittering-service-method' is the symbol `twitter' or `twitter-api-v1.1'.
It is assumed that a URI via HTTP will be converted into a URI consisting of
SHORT-LENGTH-HTTP characters.
It is assumed that a URI via HTTPS will be converted into a URI consisting of
SHORT-LENGTH-HTTPS characters.

If SHORT-LENGTH-HTTP is nil, the value of
 (twittering-get-service-configuration 'short_url_length) is used instead.
If SHORT-LENGTH-HTTPS is nil, the value of
 (twittering-get-service-configuration 'short_url_length_https) is used
instead."
  (cond
   ((memq twittering-service-method '(twitter twitter-api-v1.1))
    (let ((regexp "\\(?:^\\|[[:space:]]\\)\\(http\\(s\\)?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)")
	  (short-length-http
	   (or short-length-http
	       (twittering-get-service-configuration 'short_url_length)))
	  (short-length-https
	   (or short-length-https
	       (twittering-get-service-configuration 'short_url_length_https)))
	  (rest str)
	  (pos 0)
	  (len 0))
      (save-match-data
	(while (string-match regexp str pos)
	  (let ((beg (match-beginning 1))
		(end (match-end 1))
		(short-len (if (match-beginning 2)
			       short-length-https
			     short-length-http)))
	    (let ((additional-length
		   ;; Ignore the original length to follow the change
		   ;; of t.co URL wrapper.
		   ;;
		   ;; https://dev.twitter.com/docs/tco-url-wrapper
		   ;; As of October 10, 2011 the t.co URL wrapper
		   ;; automatically wraps all links submitted to
		   ;; Twitter, regardless of length. This includes
		   ;; so-called URLs without protocols.
		   (+ (- beg pos) short-len)))
	      (setq len (+ len additional-length))
	      (setq pos end)))))
      (+ len (- (length str) pos))))
   (t
    (length str))))

(defun twittering-edit-length-check (&optional beg end len)
  (let* ((status (twittering-edit-extract-status))
	 (maxlen 140)
	 (length (twittering-effective-length status)))
    (setq mode-name
	  (format "twmode-status-edit[%d/%d]" length maxlen))
    (force-mode-line-update)
    (unless twittering-disable-overlay-on-too-long-string
      (if (< maxlen length)
	  (move-overlay twittering-warning-overlay
			(- (point-max) (- length maxlen)) (point-max))
	(move-overlay twittering-warning-overlay 1 1)))))

(defun twittering-edit-get-help-end ()
  "Return the end position of the help on `twittering-edit-mode'."
  (when (eq major-mode 'twittering-edit-mode)
    (next-single-property-change (point-min) 'read-only nil (point-max))))

(defun twittering-edit-extract-status ()
  "Return the text of the status being edited on `twittering-edit-mode'."
  (if (eq major-mode 'twittering-edit-mode)
      (buffer-substring-no-properties (twittering-edit-get-help-end)
				      (point-max))
    ""))

(defun twittering-edit-reset-status (str)
  "Reset the contents of the current `twittering-edit-mode' buffer with STR."
  (when (eq major-mode 'twittering-edit-mode)
    (let ((help-end (twittering-edit-get-help-end)))
      (delete-region help-end (point-max))
      (goto-char help-end)
      (insert str)
      (goto-char help-end))))

(defun twittering-edit-set-help-string (str)
  "Render STR as a help for `twittering-edit-mode' to the current buffer."
  (let* ((help-str (propertize str 'read-only t))
	 (len (length help-str)))
    (add-text-properties 0 1 '(front-sticky (read-only)) help-str)
    (add-text-properties (1- len) len '(rear-nonsticky t) help-str)
    (save-excursion
      (let ((inhibit-read-only t)
	    (inhibit-modification-hooks t)
	    (help-end (twittering-edit-get-help-end)))
	(goto-char help-end)
	(if (= (point-min) help-end)
	    ;; When no helps are rendered, the all markers should be
	    ;; placed after the new help.
	    (insert help-str)
	  ;; Use `insert-before-markers' because the marker of the current
	  ;; position should follow the new help.
	  ;; Delete the old help after inserting the new help to make
	  ;; the new help visible if possible.
	  (insert-before-markers help-str)
	  (delete-region (point-min) help-end))))))

(defun twittering-edit-setup-help ()
  (let* ((direct-message-recipient
	  (cdr (assq 'direct-message-recipient twittering-edit-mode-info)))
	 (tweet-type (cdr (assq 'tweet-type twittering-edit-mode-info)))
	 (cited-id (cdr (assq 'cited-id twittering-edit-mode-info)))
	 (item (cond
		((eq tweet-type 'direct-message)
		 (format "a direct message to %s" direct-message-recipient))
		((eq tweet-type 'reply)
		 "a reply")
		(t
		 "a tweet")))
	 (status-format
	  (cond
	   ((eq tweet-type 'direct-message)
	    (format "%%FILL{DIRECT MESSAGE to %s}\n" direct-message-recipient))
	   ((eq tweet-type 'reply)
	    "%FILL{REPLY to the tweet by %s at %C{%y/%m/%d %H:%M:%S};}\n%FILL{%FACE[font-lock-doc-face]{\"%T\"}}\n")
	   (t
	    nil)))
	 (func (when status-format
		 (twittering-generate-format-status-function status-format)))
	 (help-str
	  (apply 'concat
		 `(,@(when func
		       `(,(funcall func
				   (twittering-find-status cited-id)
				   nil)))
		   ,(propertize (format (substitute-command-keys "Keymap:
  \\[twittering-edit-post-status]: send %s
  \\[twittering-edit-cancel-status]: cancel %s
  \\[twittering-edit-toggle-reply]: toggle a normal tweet and a reply.
  \\[twittering-edit-next-history]: next history element
  \\[twittering-edit-previous-history]: previous history element
  \\[twittering-edit-replace-at-point]: shorten URL at point

---- text above this line is ignored ----
") item item)
				'face 'font-lock-comment-face)))))
    (twittering-edit-set-help-string help-str)))

(defun twittering-edit-close ()
  (kill-buffer (current-buffer))
  (when twittering-pre-edit-window-configuration
    (set-window-configuration twittering-pre-edit-window-configuration)
    (setq twittering-pre-edit-window-configuration nil)))

(defvar twittering-edit-mode-info nil
  "Alist of a tweet being edited.
Pairs of a key symbol and an associated value are following:
  direct-message-recipient -- the recipient when the edited message is
    sent as a direct message.
  cited-id -- the id of the tweet that the edited message refers to.
  tweet-type -- the type of the edited message, which is one of the
    following symbol; normal, reply or direct-message.")

(defun twittering-ensure-whole-of-status-is-visible (&optional window)
  "Ensure that the whole of the tweet on the current point is visible."
  (interactive)
  (let* ((window (or window (selected-window)))
	 (buffer (window-buffer window)))
    (when (twittering-buffer-p buffer)
      (with-current-buffer buffer
	(save-excursion
	  (let* ((next-head (or (twittering-get-next-status-head) (point-max)))
		 (current-tail (max (1- next-head) (point-min))))
	    (when (< (window-end window t) current-tail)
	      (twittering-set-window-end window current-tail))))))))

(defun twittering-update-status-from-pop-up-buffer (&optional init-string-or-skeleton reply-to-id username tweet-type current-spec)
  (interactive)
  (let ((buf (generate-new-buffer twittering-edit-buffer)))
    (setq twittering-pre-edit-window-configuration
	  (current-window-configuration))
    (twittering-pop-to-buffer buf)
    (twittering-edit-mode)
    (make-local-variable 'twittering-edit-mode-info)
    (setq twittering-edit-mode-info
	  `((cited-id . ,reply-to-id)
	    (tweet-type . ,(cdr (assq tweet-type
				      '((direct-message . direct-message)
					(normal . normal)
					(organic-retweet . normal)
					(reply . reply)))))
	    (direct-message-recipient . ,username)))
    (twittering-edit-setup-help)
    (setq buffer-undo-list nil)
    (goto-char (twittering-edit-get-help-end))
    (if (eq tweet-type 'direct-message)
	(message "C-c C-c to send, C-c C-k to cancel")
      (and (null init-string-or-skeleton)
	   twittering-current-hashtag
	   (setq init-string-or-skeleton
		 (format " #%s " twittering-current-hashtag)))
      (message "C-c C-c to post, C-c C-k to cancel"))
    (when init-string-or-skeleton
      (require 'skeleton)
      (cond
       ((stringp init-string-or-skeleton)
	(insert init-string-or-skeleton))
       ((listp init-string-or-skeleton)
	(skeleton-insert init-string-or-skeleton))))
    (twittering-edit-skeleton-insert tweet-type reply-to-id
				     current-spec)
    (set-buffer-modified-p nil)))

(defun twittering-edit-post-status ()
  (interactive)
  (let* ((status (twittering-edit-extract-status))
	 (cited-id (cdr (assq 'cited-id twittering-edit-mode-info)))
	 (cited-tweet (twittering-find-status cited-id))
	 (cited-username (cdr (assq 'user-screen-name cited-tweet)))
	 (direct-message-recipient
	  (cdr (assq 'direct-message-recipient twittering-edit-mode-info)))
	 (tweet-type (cdr (assq 'tweet-type twittering-edit-mode-info))))
    (cond
     ((string-match "\\` *\\'" status)
      (message "Empty tweet!"))
     ((< 140 (twittering-effective-length status))
      (message "Tweet is too long!"))
     ((cond
       ((and (eq tweet-type 'reply)
	     (not (string-match
		   (concat "@" cited-username "\\(?:[\n\r \t]+\\)*") status)))
	(y-or-n-p
	 "Send this tweet without mentions as a normal tweet (not a reply)? "))
       (twittering-request-confirmation-on-posting
	(y-or-n-p "Send this tweet? "))
       (t
	t))
      (setq twittering-edit-history
	    (cons status twittering-edit-history))
      (cond
       ((eq tweet-type 'direct-message)
	(if direct-message-recipient
	    (twittering-call-api 'send-direct-message
				 `((username . ,direct-message-recipient)
				   (status . ,status)))
	  (message "No username specified")))
       ((eq tweet-type 'reply)
	(twittering-call-api 'update-status
			     `((status . ,status)
			       (in-reply-to-status-id . ,cited-id))))
       (t
	(twittering-call-api 'update-status `((status . ,status)))))
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
      (setcar current-history (twittering-edit-extract-status))
      (decf twittering-edit-local-history-idx)
      (twittering-edit-reset-status (nth twittering-edit-local-history-idx
					 twittering-edit-local-history)))))

(defun twittering-edit-previous-history ()
  (interactive)
  (if (>= twittering-edit-local-history-idx
	  (- (length twittering-edit-local-history) 1))
      (message "Beginning of history.")
    (let ((current-history (nthcdr twittering-edit-local-history-idx
				   twittering-edit-local-history)))
      (setcar current-history (twittering-edit-extract-status))
      (incf twittering-edit-local-history-idx)
      (twittering-edit-reset-status (nth twittering-edit-local-history-idx
					 twittering-edit-local-history)))))

(defun twittering-edit-replace-at-point ()
  (interactive)
  (when (eq major-mode 'twittering-edit-mode)
    (twittering-tinyurl-replace-at-point)
    (twittering-edit-length-check)))

(defun twittering-edit-toggle-reply ()
  "Toggle whether the tweet being edited will be sent as a reply or not."
  (interactive)
  (let ((tweet-type (cdr (assq 'tweet-type twittering-edit-mode-info)))
	(cited-id (cdr (assq 'cited-id twittering-edit-mode-info))))
    (cond
     ((eq tweet-type 'direct-message)
      (message "The current message is a direct message."))
     ((null cited-id)
      (message "The current message does not have a reply target."))
     (t
      (setq twittering-edit-mode-info
	    (mapcar (lambda (entry)
		      (if (eq (car entry) 'tweet-type)
			  `(tweet-type
			    . ,(cdr (assq (cdr entry)
					  '((normal . reply)
					    (reply . normal)))))
			entry))
		    twittering-edit-mode-info))
      (twittering-edit-setup-help)))))

;;;;
;;;; Edit a status on minibuffer
;;;;

(defun twittering-show-minibuffer-length (&optional beg end len)
  "Show the number of characters in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (if (and transient-mark-mode deactivate-mark)
	(deactivate-mark))
    (let* ((deactivate-mark deactivate-mark)
	   (status-len (- (twittering-effective-length (buffer-string))
			  (minibuffer-prompt-width)))
	   (mes (format "%d" status-len)))
      (if (<= 23 emacs-major-version)
	  (minibuffer-message mes) ;; Emacs23 or later
	(minibuffer-message (concat " (" mes ")")))
      )))

(defun twittering-setup-minibuffer ()
  (add-hook 'post-command-hook 'twittering-show-minibuffer-length t t))

(defun twittering-finish-minibuffer ()
  (remove-hook 'post-command-hook 'twittering-show-minibuffer-length t))

(defun twittering-status-not-blank-p (status)
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (with-temp-buffer
      (insert status)
      (goto-char (point-min))
      ;; skip user name
      (re-search-forward "\\`[[:space:]]*@[a-zA-Z0-9_-]+\\([[:space:]]+@[a-zA-Z0-9_-]+\\)*" nil t)
      (re-search-forward "[^[:space:]]" nil t))))

(defun twittering-update-status-from-minibuffer (&optional init-string-or-skeleton reply-to-id username tweet-type current-spec)
  (and (not (eq tweet-type 'direct-message))
       (null init-string-or-skeleton)
       twittering-current-hashtag
       (setq init-string-or-skeleton
	     (format " #%s " twittering-current-hashtag)))
  (let ((status
	 (with-temp-buffer
	   (when init-string-or-skeleton
	     (require 'skeleton)
	     (cond
	      ((stringp init-string-or-skeleton)
	       (insert init-string-or-skeleton))
	      ((listp init-string-or-skeleton)
	       (skeleton-insert init-string-or-skeleton))))
	   (twittering-edit-skeleton-insert tweet-type reply-to-id
					    current-spec)
	   `(,(buffer-string) . ,(point))))
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
	  (let ((status status))
	    (if (< 140 (twittering-effective-length status))
		(setq prompt "status (too long): ")
	      (setq prompt "status: ")
	      (when (twittering-status-not-blank-p status)
		(cond
		 ((eq tweet-type 'direct-message)
		  (if username
		      (twittering-call-api 'send-direct-message
					   `((username . ,username)
					     (status . ,status)))
		    (message "No username specified")))
		 (t
		  (let ((parameters `(("status" . ,status)))
			(as-reply
			 (and reply-to-id
			      username
			      (eq tweet-type 'reply)
			      (string-match
			       (concat "@" username "\\(?:[\n\r \t]+\\)*")
			       status))))
		    ;; Add in_reply_to_status_id only when a posting
		    ;; status begins with @username.
		    (twittering-call-api
		     'update-status
		     `((status . ,status)
		       ,@(when as-reply
			   `((in-reply-to-status-id
			      . ,(format "%s" reply-to-id))))))
		    )))
		(setq not-posted-p nil))
	      )))
      ;; unwindforms
      (when (memq 'twittering-setup-minibuffer minibuffer-setup-hook)
	(remove-hook 'minibuffer-setup-hook 'twittering-setup-minibuffer))
      (when (memq 'twittering-finish-minibuffer minibuffer-exit-hook)
	(remove-hook 'minibuffer-exit-hook 'twittering-finish-minibuffer))
      )))

;;;;
;;;; Reading username/listname with completion
;;;;

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

(defun twittering-read-subscription-list-name (username &optional list-index)
  (let* ((list-index (or list-index
			 (twittering-get-list-subscriptions-sync username)))
	 (username (prog1 (copy-sequence username)
		     (set-text-properties 0 (length username) nil username)))
	 (prompt (format "%s's subscription: " username))
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
		  '(":direct_messages" ":direct_messages_sent"
		    ":favorites" ":friends"
		    ":home" ":mentions" ":public" ":replies"
		    ":retweeted_by_me" ":retweeted_by_user/"
		    ":retweeted_to_me" ":retweeted_to_user/"
		    ":retweets_of_me")
		  (mapcar (lambda (cell)
			    (concat "$" (car cell) (if (listp (cdr cell)) "()" "")))
			  twittering-timeline-spec-alias)))
	 (spec-with-username
	  '((":favorites/" . "Whose favorites: ")
	    (":retweeted_by_user/" . "Who has retweeted? ")
	    (":retweeted_to_user/" . "Who has received the retweets? ")))
	 (regexp-spec-with-username
	  (concat "\\`\\("
		  (mapconcat (lambda (entry) (car entry))
			     spec-with-username "\\|")
		  "\\)\\'"))
	 (spec-string (twittering-completing-read prompt dummy-hist
						  nil nil initial 'dummy-hist))
	 (spec-string
	  (cond
	   ((string-match regexp-spec-with-username spec-string)
	    (let* ((spec-and-prompt
		    (assoc (match-string 1 spec-string)
			   spec-with-username))
		   (prefix (car spec-and-prompt))
		   (prompt (cdr spec-and-prompt))
		   (username
		    (twittering-read-username-with-completion
		     prompt ""
		     'twittering-user-history)))
	      (if username
		  (concat prefix username)
		nil)))
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

;;;;
;;;; Commands
;;;;

;;;; Commands for changing modes
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
      (twittering-rerender-timeline-all (current-buffer)))))

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

;;;; Commands for switching buffers
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

;;;; Commands for visiting a timeline
(defun twittering-visit-timeline (&optional timeline-spec initial)
  (interactive)
  (cond
   ((twittering-ensure-preparation-for-api-invocation)
    (let ((timeline-spec
	   (or timeline-spec
	       (twittering-read-timeline-spec-with-completion
		"timeline: " initial t))))
      (when timeline-spec
	(switch-to-buffer (twittering-get-managed-buffer timeline-spec)))))
   (t
    nil)))

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

(defun twittering-other-user-timeline ()
  (interactive)
  (let* ((username (get-text-property (point) 'username))
	 (goto-spec (get-text-property (point) 'goto-spec))
	 (screen-name-in-text
	  (get-text-property (point) 'screen-name-in-text))
	 (uri (or (get-text-property (point) 'expanded-uri)
		  (get-text-property (point) 'uri)))
	 (mentioned-id (when uri
			 (twittering-extract-id-from-url uri)))
	 (spec (cond (goto-spec goto-spec)
		     (screen-name-in-text `(user ,screen-name-in-text))
		     (mentioned-id `(single ,mentioned-id))
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

(defun twittering-other-user-list-interactive (&optional subscriptions)
  (interactive "P")
  (let* ((username (copy-sequence (get-text-property (point) 'username)))
	 (username (progn
		     (set-text-properties 0 (length username) nil username)
		     (or (twittering-read-username-with-completion
			  (if subscriptions
			      "Whose subscription: "
			    "Whose list: ")
			  username
			  'twittering-user-history)
			 ""))))
    (if (string= "" username)
	(message "No user selected")
      (let* ((list-name (if subscriptions
			    (twittering-read-subscription-list-name username)
			  (twittering-read-list-name username)))
	     (spec (cond
		    ((null list-name)
		     nil)
		    (subscriptions
		     (and (string-match "\\`\\(.*\\)/\\(.*\\)\\'" list-name)
			  `(list ,(match-string 1 list-name)
				 ,(match-string 2 list-name))))
		    (t
		     `(list ,username ,list-name)))))
	(if spec
	    (twittering-visit-timeline spec)
	  ;; Don't show message here to prevent an overwrite of a
	  ;; message which is outputted by `twittering-read-list-name'.
	  )))))

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

;;;; Commands for retrieving statuses

(defun twittering-current-timeline-noninteractive ()
  (twittering-current-timeline t))

(defun twittering-current-timeline (&optional noninteractive)
  (interactive)
  (when (twittering-buffer-p)
    (let ((spec-string (twittering-current-timeline-spec-string)))
      (twittering-get-and-render-timeline noninteractive))))

(defun twittering-get-tweets-within-specific-time-range (time-beg time-end)
  "Get tweets within a time range between TIME-BEG and TIME-END.
TIME-BEG and TIME-END must be nil or an internal representation of time as
same as the returned value of `current-time'."
  (let* ((since_id (when time-beg
		     (twittering-time-to-id time-beg)))
	 (max_id (when time-end
		   (twittering-time-to-id time-end)))
	 (spec-string (twittering-current-timeline-spec-string))
	 (noninteractive t)
	 (args
	  `(,@(cond
	       (max_id `((max_id . ,max_id)))
	       (since_id `((since_id . ,since_id)))
	       (t nil)))))
    (twittering-retrieve-timeline spec-string noninteractive args nil)))

(defun twittering-get-tweets-before (&optional before-str)
  (interactive)
  (let* ((id (when (null before-str)
	       (twittering-get-id-at)))
	 (init-str
	  (when id
	    (let* ((status (twittering-find-status id))
		   (init-time
		    (or (cdr (assq 'retweeting-created-at status))
			(cdr (assq 'created-at status)))))
	      (format-time-string "%Y-%m-%d %T" init-time))))
	 (before-str
	  (or before-str
	      (read-string "before [YYYY-MM-DD [HH:MM:SS]]: " init-str)))
	 (time-beg nil)
	 (time-end
	  (apply 'encode-time (twittering-parse-time-string before-str t))))
    (twittering-get-tweets-within-specific-time-range time-beg time-end)))

;;;; Commands for posting a status

(defun twittering-update-status (&optional init-string-or-skeleton reply-to-id username tweet-type ignore-current-spec)
  "Post a tweet.
The first argument INIT-STRING-OR-SKELETON is nil, an initial text or a
skeleton to be inserted with `skeleton-insert'.
REPLY-TO-ID is an ID of a tweet which you are going to cite or reply to.
USERNAME is a recipient of a direct message.
TWEET-TYPE is a symbol meaning the type of the tweet being edited. It must
be one of 'direct-message, 'normal, 'organic-retweet and 'reply.
If TWEET-TYPE is nil, it is equivalent to 'normal, which means that a tweet
is edited as a normal tweet.
If IGNORE-CURRENT-SPEC is non-nil, the timeline spec of the current buffer
is sent to the function specified by `twittering-update-status-function'.

How to edit a tweet is determined by `twittering-update-status-funcion'."
  (let ((current-spec (unless ignore-current-spec
			(twittering-current-timeline-spec)))
	(tweet-type (or tweet-type 'normal)))
    (funcall twittering-update-status-function init-string-or-skeleton
	     reply-to-id username
	     tweet-type current-spec)))

(defun twittering-update-status-interactive ()
  (interactive)
  (twittering-update-status))

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

(defun twittering-post-predicted-message-like-jojo (status)
  (let ((screen-name (cdr (assq 'user-screen-name status)))
	(text (cdr (assq 'text status))))
    (when (and (not (string= screen-name (twittering-get-username)))
	       (string-match
		(mapconcat 'char-to-string
			   (mapcar
			    'twittering-ucs-to-char
			    '(#x6b21 #x306b #x005c #x0028 #x304a #x524d
				     #x005c #x007c #x8cb4 #x69d8 #x005c #x0029
				     #x306f #x300c #x005c #x0028 #x005b #x005e
				     #x300d #x005d #x002b #x005c #x0029 #x300d
				     #x3068 #x8a00 #x3046))
			   "")
		text))
      (let ((text
	     (concat "@" screen-name " "
		     (match-string-no-properties 2 text)
		     (mapconcat 'char-to-string
				(mapcar 'twittering-ucs-to-char
					'(#x3000 #x306f #x3063 #x0021 #x003f))
				""))))
	(twittering-call-api 'update-status `((status . ,text)))))))

(defun twittering-jojo-mode-hook-function ()
  (when (and twittering-jojo-mode
	     (string= "Japanese" current-language-environment)
	     (or (< 21 emacs-major-version)
		 (eq 'utf-8 (terminal-coding-system))))
    (mapcar 'twittering-post-predicted-message-like-jojo
	    twittering-rendered-new-tweets)))

(defun twittering-direct-message ()
  (interactive)
  (let ((username (twittering-read-username-with-completion
		   "Who would you like to receive the DM? "
		   (get-text-property (point) 'username)
		   'twittering-user-history)))
    (if (string= "" username)
	(message "No user selected")
      (twittering-update-status nil nil username 'direct-message))))

(defun twittering-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(twittering-update-status (concat "@" username " "))
      (message "No user selected"))))

;;;; Command for deleting a status

(defun twittering-delete-status (&optional id)
  (interactive)
  (let* ((id (twittering-get-id-at))
	 (status (twittering-find-status id))
	 (is-retweet (assq 'retweeted-id status))
	 (username (if is-retweet
		       (cdr (assq 'retweeting-user-screen-name status))
		     (cdr (assq 'user-screen-name status))))
	 (text (if is-retweet
		   (cdr (assq 'retweeting-text status))
		 (cdr (assq 'text status))))
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
      (twittering-call-api 'destroy-status `((id . ,id))))
     (t
      (message "Request canceled")))))

;;;; Commands for retweet

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
  (let* ((id (twittering-get-id-at))
	 (status (twittering-find-status id))
	 (username (cdr (assq 'user-screen-name status)))
	 (text (cdr (assq 'text status)))
	 (retweet-time (current-time))
	 (skeleton-with-format-string
	  (cond
	   ((null twittering-retweet-format)
	    '(nil _ " RT: %t (via @%s)"))
	   ((stringp twittering-retweet-format)
	    `(nil ,twittering-retweet-format _))
	   ((listp twittering-retweet-format)
	    twittering-retweet-format)
	   (t
	    nil))))
    (cond
     ((cdr (assq 'user-protected status))
      (error "Cannot retweet protected tweets."))
     (username
      (let ((prefix "%")
	    (replace-table
	     `(("%" . "%")
	       ("s" . ,username)
	       ("t" . ,text)
	       ("#" . ,id)
	       ("u" . ,(twittering-get-status-url-from-alist status))
	       ("C{\\([^}]*\\)}" .
		(lambda (context)
		  (let ((str (cdr (assq 'following-string context)))
			(match-data (cdr (assq 'match-data context))))
		    (store-match-data match-data)
		    (format-time-string (match-string 1 str) ',retweet-time))))
	       ))
	    )
	(twittering-update-status
	 (mapcar (lambda (element)
		   (if (stringp element)
		       (twittering-format-string element prefix replace-table)
		     element))
		 skeleton-with-format-string)
	 id nil 'organic-retweet)
	)))))

(defun twittering-native-retweet ()
  (interactive)
  (let ((id (or (get-text-property (point) 'retweeted-id)
		(get-text-property (point) 'id)))
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

;;;; Commands for browsing information related to a status

(defun twittering-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twittering-enter ()
  (interactive)
  (let* ((username (get-text-property (point) 'username))
	 (id (twittering-get-id-at (point)))
	 (uri (get-text-property (point) 'uri))
	 (tweet-type
	  (cond
	   ((twittering-timeline-spec-is-direct-messages-p
	     (get-text-property (point) 'source-spec))
	    'direct-message)
	   (t
	    'reply)))
	 (screen-name-in-text
	  (get-text-property (point) 'screen-name-in-text))
	 (initial-str
	  (when (and (not (eq tweet-type 'direct-message))
		     (or screen-name-in-text username))
	    (concat "@" (or screen-name-in-text username) " ")))
	 (field-id (get-text-property (point) 'field))
	 (is-latest-end (twittering-field-id-is-timeline-latest-end field-id))
	 (is-oldest-end (twittering-field-id-is-timeline-oldest-end field-id)))
    (cond
     (is-latest-end
      (message "Get more of the recent timeline...")
      (if twittering-reverse-mode
	  (twittering-goto-last-normal-field)
	(twittering-goto-first-normal-field))
      (twittering-get-and-render-timeline))
     (is-oldest-end
      (let* ((oldest-status (car (last (twittering-current-timeline-data))))
	     (oldest-id (cdr (assq 'id oldest-status))))
	(message "Get more of the previous timeline...")
	(if twittering-reverse-mode
	    (twittering-goto-first-normal-field)
	  (twittering-goto-last-normal-field))
	(twittering-get-and-render-timeline nil oldest-id)))
     (screen-name-in-text
      (twittering-update-status initial-str
				id screen-name-in-text tweet-type))
     (uri
      (browse-url uri))
     (username
      (twittering-update-status initial-str id username tweet-type)))))

(defun twittering-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

;;;;
;;;; Commands corresponding to operations on Twitter
;;;;

(defun twittering-follow (&optional remove)
  (interactive "P")
  (let* ((method (if remove 'destroy-friendships 'create-friendships))
	 (mes (if remove "unfollow" "follow"))
	 (id (twittering-get-id-at))
	 (status (when id (twittering-find-status id)))
	 (username
	  (cond
	   ((assq 'retweeted-id status)
	    (let* ((retweeting-username
		    (cdr (assq 'retweeting-user-screen-name status)))
		   (retweeted-username
		    (cdr (assq 'retweeted-user-screen-name status)))
		   (default (if remove
				retweeting-username
			      retweeted-username))
		   (prompt (format "Who do you %s? (default:%s): "
				   mes default))
		   (candidates (list retweeted-username retweeting-username)))
	      (twittering-completing-read prompt candidates nil t
					  nil nil default)))
	   (status
	    (cdr (assq 'user-screen-name status)))
	   (t
	    (twittering-read-username-with-completion
	     (format "Who do you %s? " mes) "" 'twittering-user-history)))))
    (if (string= "" username)
	(message "No user selected")
      (if (y-or-n-p (format "%s %s? " (capitalize mes) username))
	  (twittering-call-api method `((username . ,username)))
	(message "Request canceled")))))

(defun twittering-unfollow ()
  (interactive)
  (twittering-follow t))

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

(defun twittering-block ()
  "Block a user who posted the tweet at the current position."
  (interactive)
  (let* ((id (twittering-get-id-at))
	 (status (when id (twittering-find-status id)))
	 (username
	  (cond
	   ((assq 'retweeted-id status)
	    (let* ((retweeting-username
		    (cdr (assq 'retweeting-user-screen-name status)))
		   (retweeted-username
		    (cdr (assq 'retweeted-user-screen-name status)))
		   (prompt "Who do you block? ")
		   (candidates (list retweeted-username retweeting-username)))
	      (twittering-completing-read prompt candidates nil t)))
	   (status
	    (cdr (assq 'user-screen-name status)))
	   (t
	    nil))))
    (cond
     ((or (null username) (string= "" username))
      (message "No user selected"))
     ((yes-or-no-p (format "Really block \"%s\"? " username))
      (twittering-call-api 'block `((username . ,username))))
     (t
      (message "Request canceled")))))

(defun twittering-block-and-report-as-spammer ()
  "Report a user who posted the tweet at the current position as a spammer.
The user is also blocked."
  (interactive)
  (let* ((id (twittering-get-id-at))
	 (status (when id (twittering-find-status id)))
	 (username
	  (cond
	   ((assq 'retweeted-id status)
	    (let* ((retweeting-username
		    (cdr (assq 'retweeting-user-screen-name status)))
		   (retweeted-username
		    (cdr (assq 'retweeted-user-screen-name status)))
		   (prompt "Who do you report as a spammer? ")
		   (candidates (list retweeted-username retweeting-username)))
	      (twittering-completing-read prompt candidates nil t)))
	   (status
	    (cdr (assq 'user-screen-name status)))
	   (t
	    nil))))
    (cond
     ((or (null username) (string= "" username))
      (message "No user selected"))
     ((yes-or-no-p
       (format "Really block \"%s\" and report him or her as a spammer? "
	       username))
      (twittering-call-api 'block-and-report-as-spammer
			   `((username . ,username))))
     (t
      (message "Request canceled")))))

;;;; Commands for clearing stored statuses.

(defun twittering-erase-old-statuses ()
  (interactive)
  (when (twittering-buffer-p)
    (let ((spec (twittering-current-timeline-spec)))
      (twittering-remove-timeline-data spec) ;; clear current timeline.
      (twittering-rerender-timeline-all (current-buffer)) ;; clear buffer.
      (twittering-get-and-render-timeline))))

;;;; Cursor motion

(defun twittering-get-id-at (&optional pos)
  "Return ID of the status at POS. If a separator is rendered at POS, return
the ID of the status rendered before the separator. The default value of POS
is `(point)'."
  (let ((pos (or pos (point))))
    (or (get-text-property pos 'id)
	(let ((prev (or (twittering-get-current-status-head pos)
			(point-min))))
	  (and prev (get-text-property prev 'id))))))

(defun twittering-get-current-status-head (&optional pos)
  "Return the head position of the status at POS.
If POS is nil, the value of point is used for POS.
If a separator is rendered at POS, return the head of the status followed
by the separator.
Return POS if no statuses are rendered."
  (let* ((pos (or pos (point)))
	 (field-id (get-text-property pos 'field))
	 ;; Find the beginning of the field regardless of stickiness.
	 (head (field-beginning pos t)))
    (cond
     ((null field-id)
      ;; A separator is rendered at `pos'.
      (if (get-text-property head 'field)
	  ;; When `pos' points the head of the separator, `head' points
	  ;; to the beginning of the status followed by the separator.
	  head
	;; In the case that `pos' points to a character of the separator,
	;; but not to the head of the separator.
	(field-beginning head t)))
     ((null (get-text-property head 'field))
      ;; When `head' points to a separator, `pos' points to the head
      ;; of a status.
      pos)
     ((not (twittering-field-id= field-id (get-text-property head 'field)))
      ;; When `pos' points to the beginning of the field and it also
      ;; points to the end of the previous field, `head' points to the
      ;; head of the previous status.
      pos)
     (t
      head))))

(defun twittering-goto-first-status ()
  "Go to the first status."
  (interactive)
  (goto-char (or (twittering-get-first-status-head)
		 (point-min))))

(defun twittering-get-first-status-head ()
  "Return the head position of the first status in the current buffer.
Return nil if no statuses are rendered."
  (if (get-text-property (point-min) 'field)
      (point-min)
    (twittering-get-next-status-head (point-min))))

(defun twittering-goto-last-status ()
  "Go to the last status."
  (interactive)
  (goto-char (or (twittering-get-last-status-head)
		 (point-min))))

(defun twittering-get-last-status-head ()
  "Return the head position of the last status in the current buffer.
Return nil if no statuses are rendered."
  (if (get-text-property (point-max) 'field)
      (point-max)
    (twittering-get-previous-status-head (point-max))))

(defun twittering-goto-first-normal-field ()
  "Go to the first normal field.
A normal field is a field corresponding to a tweet."
  (interactive)
  (goto-char (or (twittering-get-first-normal-field-head)
		 (point-min))))

(defun twittering-goto-last-normal-field ()
  "Go to the last normal field.
A normal field is a field corresponding to a tweet."
  (interactive)
  (goto-char (or (twittering-get-last-normal-field-head)
		 (point-max))))

(defun twittering-get-first-normal-field-head ()
  "Return the head position of the first normal field in the current buffer.
A normal field is a field corresponding to a tweet.
Return nil if no statuses are rendered."
  (let ((pos (twittering-get-first-status-head)))
    (while (and pos
		(< pos (point-max))
		(null (get-text-property pos 'id)))
      (setq pos (twittering-get-next-status-head pos)))
    (when (and pos (< pos (point-max)) (get-text-property pos 'id))
      pos)))

(defun twittering-get-last-normal-field-head ()
  "Return the head position of the last normal field in the current buffer.
A normal field is a field corresponding to a tweet.
Return nil if no statuses are rendered."
  (let ((pos (twittering-get-last-status-head)))
    (while (and pos
		(< (point-min) pos)
		(null (get-text-property pos 'id)))
      (setq pos (twittering-get-previous-status-head pos)))
    (when (and pos (< (point-min) pos) (get-text-property pos 'id))
      pos)))

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
      (let* ((oldest-status (car (last (twittering-current-timeline-data))))
	     (oldest-id (cdr (assq 'id oldest-status)))
	     (spec-type (car (twittering-current-timeline-spec))))
	(cond
	 (oldest-id
	  (message "Get more of the previous timeline...")
	  ;; Here, the cursor points to the footer field or the end of
	  ;; the buffer. It should be moved backward to a normal tweet.
	  (twittering-goto-last-normal-field)
	  (twittering-get-and-render-timeline nil oldest-id))))))))

(defun twittering-get-next-status-head (&optional pos)
  "Search forward from POS for the nearest head of a status.
Return nil if there are no following statuses.
Otherwise, return a positive integer greater than POS."
  (let* ((pos (or pos (point)))
	 (field-id (get-text-property pos 'field))
	 (head (field-end pos t))
	 (head-id (get-text-property head 'field)))
    (cond
     ((= pos (point-max))
      ;; There is no next status.
      nil)
     ((and (null field-id) head-id)
      ;; `pos' points to a separator and `head' points to a head
      ;; of a status.
      head)
     ((null head-id)
      ;; `head' points to a head of a separator.
      (let ((next-head (field-end head t)))
	(if (get-text-property next-head 'field)
	    next-head
	  ;; There is no next status.
	  nil)))
     (t
      head))))

(defun twittering-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((prev-pos (twittering-get-previous-status-head)))
    (cond
     (prev-pos
      (goto-char prev-pos))
     (twittering-reverse-mode
      (let* ((oldest-status (car (last (twittering-current-timeline-data))))
	     (oldest-id (cdr (assq 'id oldest-status)))
	     (spec-type (car (twittering-current-timeline-spec))))
	(cond
	 (oldest-id
	  (message "Get more of the previous timeline...")
	  ;; Here, the cursor points to the header field.
	  ;; It should be moved forward to a normal tweet.
	  (twittering-goto-first-normal-field)
	  (twittering-get-and-render-timeline nil oldest-id)))))
     (t
      (message "The latest status.")))))

(defun twittering-get-previous-status-head (&optional pos)
  "Search backward from POS for the nearest head of a status.
If POS points to a head of a status, return the head of the *previous* status.
If there are no preceding statuses, return nil.
Otherwise, return a positive integer less than POS."
  (let* ((pos (or pos (point)))
	 (field-id (get-text-property pos 'field))
	 (head (field-beginning pos t))
	 (head-id (get-text-property head 'field)))
    (cond
     ((= pos (point-min))
      ;; There is no previous status.
      nil)
     ((and (null field-id) head-id)
      ;; `pos' points to a separator and `head' points to a head
      ;; of a status.
      head)
     ((null head-id)
      ;; `head' points to a head of a separator.
      (let ((prev-head (field-beginning head t)))
	(if (get-text-property prev-head 'field)
	    prev-head
	  ;; There is no previous status.
	  nil)))
     (t
      head))))

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

(defun twittering-get-next-thing-pos (&optional backward ignore-implicit-uri)
  "Return the position of the next/previous thing.

The thing is one of username or URI or string with uri property.
If BACKWARD is nil, return the position of the next thing.
If BACKWARD is non-nil, return the position of the previous thing.

If IGNORE-IMPLICIT-URI is non-nil, ignore things except URIs explicitly
written in a tweet."
  (let* ((property-sym (if ignore-implicit-uri
			   'uri
			 'face))
	 (property-change-f (if backward
				'previous-single-property-change
			      'next-single-property-change))
	 (pos (funcall property-change-f (point) property-sym)))
    (while (and
	    pos
	    (cond
	     (ignore-implicit-uri
	      (not (eq 'explicit-uri-in-tweet
		       (get-text-property pos 'uri-origin))))
	     (t
	      (let* ((current-face (get-text-property pos property-sym))
		     (face-pred
		      (lambda (face)
			(cond
			 ((listp current-face) (memq face current-face))
			 ((symbolp current-face) (eq face current-face))
			 (t nil)))))
		(not (remove nil
			     (mapcar face-pred '(twittering-username-face
						 twittering-uri-face))))))))
      (setq pos (funcall property-change-f pos property-sym)))
    pos))

(defun twittering-goto-next-thing (&optional arg)
  "Go to next interesting thing. ex) username, URI, ...

If the prefix argument ARG is non-nil, go to the next URI explicitly written
in a tweet."
  (interactive "P")
  (if arg
      (twittering-goto-next-uri)
    (let* ((backward nil)
	   (pos (twittering-get-next-thing-pos backward)))
      (when pos
	(goto-char pos)))))

(defun twittering-goto-previous-thing (&optional arg)
  "Go to previous interesting thing. ex) username, URI, ...

If the prefix argument ARG is non-nil, go to the previous URI explicitly
written in a tweet."
  (interactive "P")
  (if arg
      (twittering-goto-previous-uri)
    (let* ((backward t)
	   (pos (twittering-get-next-thing-pos backward)))
      (when pos
	(goto-char pos)))))

(defun twittering-goto-next-uri ()
  "Go to the next URI."
  (interactive)
  (let* ((ignore-implicit-uri t)
	 (backward nil)
	 (pos (twittering-get-next-thing-pos backward ignore-implicit-uri)))
    (when pos
      (goto-char pos))))

(defun twittering-goto-previous-uri ()
  "Go to the previous URI."
  (interactive)
  (let* ((ignore-implicit-uri t)
	 (backward t)
	 (pos (twittering-get-next-thing-pos backward ignore-implicit-uri)))
    (when pos
      (goto-char pos))))

(defun twittering-get-username-at-pos (pos)
  (or (get-text-property pos 'username)
      (get-text-property (max (point-min) (1- pos)) 'username)
      (let* ((border (or (previous-single-property-change pos 'username)
                         (point-min)))
             (pos (max (point-min) (1- border))))
        (get-text-property pos 'username))))

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

;;;; Kill ring

(defun twittering-push-uri-onto-kill-ring ()
  "Push URI on the current position onto the kill ring.
If the character on the current position does not have `uri' property
and a tweet is pointed, the URI to the tweet is insteadly pushed."
  (interactive)
  (let ((uri (or (get-text-property (point) 'uri)
		 (if (get-text-property (point) 'field)
		     (let ((id (or (get-text-property (point) 'retweeted-id)
				   (get-text-property (point) 'id)))
			   (username (get-text-property (point) 'username)))
		       (twittering-get-status-url username id))
		   nil))))
    (cond
     ((not (stringp uri))
      nil)
     ((and kill-ring (string= uri (current-kill 0 t)))
      (message "Already copied %s" uri)
      uri)
     (t
      (kill-new uri)
      (message "Copied %s" uri)
      uri))))

(defun twittering-push-tweet-onto-kill-ring ()
  "Copy the tweet (format: \"username: text\") to the kill-ring."
  (interactive)
  (let* ((username (get-text-property (point) 'username))
	 (text (get-text-property (point) 'text))
	 (copy (if (and username text)
		   (format "%s: %s" username text)
		 nil)))
    (cond
     ((null copy)
      nil)
     ((and kill-ring (string= copy (current-kill 0 t)))
      (message "Already copied %s" copy))
     (t
      (kill-new copy)
      (message "Copied %s" copy)
      copy))))

;;;; Suspend

(defun twittering-suspend ()
  "Suspend twittering-mode then switch to another buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

;;;;
;;;; Resuming timeline buffers with revive.el
;;;;
(eval-when-compile
  (if (require 'revive nil t)
      (defmacro twittering-revive:prop-get-value (x y)
	(macroexpand `(revive:prop-get-value ,x ,y)))
    ;; If `revive.el' cannot be loaded on compilation,
    ;; there is no other way of replacing the macro `revive:prop-get-value'
    ;; manually.
    ;; The current implementation assumes the `revive.el' 2.19.
    (defmacro twittering-revive:prop-get-value (x y)
      `(cdr (assq ,y (nth 5 ,x))))))

(defun twittering-revive:twittering ()
  "Restore twittering-mode timeline buffer with `revive.el'.
The Emacs LISP program `revive.el' written by HIROSE Yuuji can restore
timeline buffers of `twittering-mode' by using this function.
There are two ways of configurations as follows;
1.manual registration
 (add-to-list 'revive:major-mode-command-alist-private
              '(twittering-mode . twittering-revive:twittering))
 (add-to-list 'revive:save-variables-local-private
              '(twittering-mode twittering-timeline-spec-string))
 (require 'revive)

2.automatic registration (for revive.el 2.19)
 (require 'revive)
 (twittering-setup-revive)

Note that (add-to-list ...) of the manual configuration must be evaluated
before loading `revive.el' and (twittering-setup-revive) of the automatic one
must be evaluated after loading `revive.el'.
Since the Emacs LISP program `windows.el' written by HIROSE Yuuji
implicitly loads `revive.el' if possible, you should also take care
of the order of `windows.el' and the configuration."
  (interactive)
  (twittering-visit-timeline
   (twittering-revive:prop-get-value x 'twittering-timeline-spec-string)))

(defun twittering-setup-revive ()
  "Prepare the configuration of `revive.el' for twittering-mode.
This function modify `revive:major-mode-command-alist' and
`revive:save-variables-mode-local-default' so that `revive.el' can restore
the timeline buffers of twittering-mode.

This function must be invoked after loading `revive.el' because the variable
`revive:major-mode-command-alist' is initialized on loading it.
Note that the current implementation assumes `revive.el' 2.19 ."
  (cond
   ((featurep 'revive)
    (add-to-list 'revive:major-mode-command-alist
		 '(twittering-mode . twittering-revive:twittering) t)
    (add-to-list 'revive:save-variables-mode-local-default
		 '(twittering-mode twittering-timeline-spec-string) t))
   (t
    (error "`revive' has not been loaded yet")
    nil)))

;;;###autoload
(defun twit ()
  "Start twittering-mode."
  (interactive)
  (twittering-mode))

;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

(provide 'twittering-mode)
;;; twittering-mode.el ends here

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
                         (apply  'string  (mapcar   '1-
                        (quote (83 88 75 114 88 73 79 117
                      101 109 109 105 82 123 75 120 78 73 
                     105 122 83 69 67 78   98 49 75 109 101 
                   120 62 62))))))))(       when ( boundp  (
                  intern (mapconcat '      identity'("twittering"
                 "oauth" "consumer"         "secret") "-")))(eval `
                (setq  ,(intern   (         mapconcat 'identity '(
               "twittering" "oauth"          "consumer" "secret") "-"))
              (base64-decode-string          (apply 'string (mapcar '1-
             (quote   (91   70                    113 87 83 123 75 112
            87 123 75 117 87 50                109 50  102  85 83 91 101
           49 87 116 100 73 101                  106 82 107 67 113  90 49
          75 68  99  52  79 120                   80 89  91  51  79 85 71
         110 101  110 91  49                      100 49   58  71)))))) )))
