;;; twittering-mode.el --- Major mode for Twitter

;; Copyright (C) 2007 Yuto Hayamizu.

;; Author: Y. Hayamizu <y.hayamizu@gmail.com>
;; Created: Sep 4, 2007
;; Version: 0.1.1
;; Keywords: twitter web
;; URL: http://hayamin.com/

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

;;; Code:

(require 'cl)
(require 'xml)

(defvar twittering-mode-map (make-sparse-keymap))

(defvar twittering-timer nil)

(defvar twittering-idle-time 20)

(defvar twittering-timer-interval 90)

(defvar twittering-username nil)

(defvar twittering-password nil)

(defvar twittering-buffer "*twittering*")
(defun twittering-buffer ()
  (twittering-get-or-generate-buffer twittering-buffer))

(defvar twittering-http-buffer "*twittering-http-buffer*")
(defun twittering-http-buffer ()
  (twittering-get-or-generate-buffer twittering-http-buffer))

(defvar twittering-friends-timeline-data nil)

(defvar twittering-font-lock-keywords nil)

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

(defun assocref (item alist)
    (cdr (assoc item alist)))

;;;
;;; to show image files

(defvar twittering-wget-buffer "*twittering-wget-buffer*")
(defun twittering-wget-buffer ()
  (twittering-get-or-generate-buffer twittering-wget-buffer))

(defvar twittering-tmp-dir "/tmp/twmode-images")

(defvar twittering-icon-mode nil "You MUST NOT CHANGE this variable directory. You should change through function'twittering-icon-mode'")
(defun twittering-icon-mode (&rest arg)
  (interactive)
  (setq twittering-icon-mode
	(if (or (and arg (car arg)) (not twittering-icon-mode))
	    (if (file-writable-p twittering-tmp-dir)
		(progn 
		  (if (not (file-directory-p twittering-tmp-dir))
		      (make-directory twittering-tmp-dir))
		  t)
	      nil)
	  nil))
  (twittering-render-friends-timeline))

(defvar twittering-image-stack nil)

(defun twittering-image-type (file-name)
  (cond
   ((string-match "\\.jpe?g" file-name) 'jpeg)
   ((string-match "\\.png" file-name) 'png)
   ((string-match "\\.gif" file-name) 'gif)
   (t nil)))

(if twittering-font-lock-keywords
    ()
  (setq
   twittering-font-lock-keywords
   (list
    ;; screen name
    '("\\([-_\.a-zA-Z0-9]+\\):$" 1 twittering-username-face)
    ;; status
    ;; '("\\s-\\s-\\(.*\\)$" 1 font-lock-constant-face)

    ;; uri
    '("https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+" 0 twittering-uri-face)
    ))

  (defun twittering-set-font-lock-keywords ()
    (setq font-lock-defaults
	  (list 'twittering-font-lock-keywords nil nil nil nil)))

  (add-hook 'twittering-mode-hook 'twittering-set-font-lock-keywords)
  t)

(defvar twittering-debug-mode nil)
(defvar twittering-debug-buffer nil)
(defmacro debug-print (obj)
  `(if twittering-debug-mode
       (progn
	 (if (or (null twittering-debug-buffer)
		 (not (buffer-live-p twittering-debug-buffer)))
	     (setq twittering-debug-buffer (generate-new-buffer "*debug*")))
	 (save-excursion
	   (set-buffer twittering-debug-buffer)
	   (insert (twittering-inspect-object ,obj))
	   (newline)
	   ,obj))
    ,obj))

(defun twittering-debug-mode ()
  (interactive)
  (setq twittering-debug-mode
	(not twittering-debug-mode))
  (message (if twittering-debug-mode "debug mode:on" "debug mode:off")))

(if twittering-mode-map
    (let ((km twittering-mode-map))
      (define-key km "\C-c\C-f" 'twittering-friends-timeline)
      (define-key km "\C-c\C-s" 'twittering-update-status-interactive)
      (define-key km "\C-c\C-e" 'twittering-erase-old-statuses)
      (define-key km "\C-m" 'twittering-enter)
      (define-key km [mouse-1] 'twittering-click)
      (define-key km "\C-c\C-v" 'twittering-view-user-page)
      (define-key km "j" 'next-line)
      (define-key km "k" 'previous-line)
      (define-key km "l" 'forward-char)
      (define-key km "h" 'backward-char)
      (define-key km "0" 'beginning-of-line)
      (define-key km "^" 'beginning-of-line-text)
      (define-key km "$" 'end-of-line)
      (define-key km [backspace] 'backward-char)
      (define-key km "G" 'end-of-buffer)
      (define-key km "H" 'beginning-of-buffer)
      nil))

(defvar twittering-mode-syntax-table nil "")

(if twittering-mode-syntax-table
    ()
  (setq twittering-mode-syntax-table (make-syntax-table))
;  (modify-syntax-entry ?  "" twittering-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  twittering-mode-syntax-table)
  )

(defun twittering-mode-init-variables ()
  ;(make-variable-buffer-local 'variable)
  ;(setq variable nil)
  (font-lock-mode t)
  (defface twittering-username-face
    `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-string-face 'twittering-username-face)
  (set-face-attribute 'twittering-username-face nil :underline t)
  (defface twittering-uri-face
    `((t nil)) "" :group 'faces)
  (set-face-attribute 'twittering-uri-face nil :underline t)
  )

(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key)) keylist))
		't)
	     ,@body)))
       clauses)))

;; If you use Emacs21, decode-char 'ucs will fail unless Mule-UCS is loaded.
;; TODO: Show error messages if Emacs 21 without Mule-UCS
(defmacro twittering-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defun twittering-inspect-object (obj)
  (cond
   ((stringp obj) (format "\"%s\"" obj))
   ((symbolp obj) (format "%s" obj))
   ((integerp obj) (format "%d" obj))
   ((floatp obj) (format "%f" obj))
   ((listp obj)
    (let ((ret nil))
      (while obj
	(if (atom obj)
	    (progn (setq ret `(,(twittering-inspect-object obj) "." ,@ret))
		   (setq obj nil))
	  (setq ret (cons (twittering-inspect-object (car obj)) ret))
	  (setq obj (cdr obj))))
      (concat "(" (mapconcat #'identity (reverse ret) " ") ")")))
   ((arrayp obj)
    (concat "[" (mapconcat #'twittering-inspect-object obj " ") "]"))
   (t (error "Unknown type object!"))))

(defun twittering-mode ()
  "Major mode for Twitter"
  (interactive)
  (switch-to-buffer (twittering-buffer))
  (kill-all-local-variables)
  (twittering-mode-init-variables)
  (use-local-map twittering-mode-map)
  (setq major-mode 'twittering-mode)
  (setq mode-name "Twittering mode")
  (set-syntax-table twittering-mode-syntax-table)
  (run-hooks 'twittering-mode-hook)
  (font-lock-mode nil)
  (font-lock-mode t)
  (twittering-start)
  )

;;;
;;; Basic HTTP functions
;;;

(defun twittering-http-get (method-class method &optional sentinel)
  (if (null sentinel) (setq sentinel 'twittering-http-get-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (twittering-http-buffer))
    (erase-buffer))

  (let (proc)
    (condition-case nil
	(progn
	  (setq proc
		(open-network-stream
		 "network-connection-process" (twittering-http-buffer)
		 "twitter.com" 80))
	  (set-process-sentinel proc sentinel)
	  (process-send-string
	   proc
	   (let ((nl "\r\n"))
	     (concat "GET /" method-class "/" method ".xml HTTP/1.1" nl
		     "Host: twitter.com" nl
		     "Authorization: Basic "
		     (base64-encode-string
		      (concat twittering-username ":" twittering-password))
		     nl
		     "Accept: text/xml"
		     ",application/xml"
		     ",application/xhtml+xml"
		     ",application/html;q=0.9"
		     ",text/plain;q=0.8"
		     ",image/png,*/*;q=0.5" nl
		     "Accept-Charset: utf-8;q=0.7,*;q=0.7"
		     nl nl))))
      (error
       (message "Failure: HTTP GET") nil))))

(defun twittering-http-get-default-sentinel (proc stat &optional suc-msg)
  (let ((header (twittering-get-response-header))
	  (body (twittering-get-response-body))
	  (status nil)
	  )
      (if (string-match "HTTP/1\.[01] \\([a-z0-9 ]+\\)\r?\n" header)
	  (progn
	    (setq status (match-string-no-properties 1 header))
	    (case-string
	     status
	     (("200 OK")
	      (mapcar
	       #'twittering-cache-status-datum
	       (reverse (twittering-xmltree-to-status
			 body)))
	      (twittering-render-friends-timeline)
	      (message (if suc-msg suc-msg "Success: Get.")))
	     (t (message status))))
	(message "Failure: Bad http response.")))
  )

(defun twittering-render-friends-timeline ()
  (let ((point (save-excursion (set-buffer (twittering-buffer)) (point))))
    (save-excursion 
      (set-buffer (twittering-buffer))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert
       (mapconcat (lambda (status)
		    (concat
		     (let ((icon-string "\n  ")
			   (filename (assocref 'icon-string status)))
		       (if (and icon-string twittering-icon-mode)
			   (progn
			     (set-text-properties
			      1 2 `(display
				    (image :type ,(twittering-image-type filename)
					   :file ,(concat twittering-tmp-dir
							  "/"
							  filename)))
			      icon-string)
			     icon-string)
			 nil))
		     (assocref 'username status) ":\n  "
		     (assocref 'text status)))
		  twittering-friends-timeline-data
		  "\n"))
      (if twittering-image-stack
	  (clear-image-cache))
      (setq buffer-read-only t))
    (let ((cb (current-buffer))
	  (tb (get-buffer (twittering-buffer))))
      (if (eq cb tb)
	  (goto-char point)))))

(defun twittering-http-post
  (method-class method &optional parameters contents sentinel)
  "Send HTTP POST request to twitter.com

METHOD-CLASS must be one of Twitter API method classes(statuses, users or direct_messages).
METHOD must be one of Twitter API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters. ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (if (null sentinel) (setq sentinel 'twittering-http-post-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (twittering-http-buffer))
    (erase-buffer))

  (let (proc)
    (condition-case nil
	(progn
	  (setq proc
		(open-network-stream
		 "network-connection-process" (twittering-http-buffer)
		 "twitter.com" 80))
	  (set-process-sentinel proc sentinel)
	  (process-send-string
	   proc
	   (let ((nl "\r\n"))
	     (concat "POST /" method-class "/" method ".xml?"
		     (if parameters
			 (mapconcat
			  (lambda (param-pair)
			    (format "%s=%s"
				    (twittering-percent-encode (car param-pair))
				    (twittering-percent-encode (cdr param-pair))))
			  parameters
			  "&"))
		     " HTTP/1.1" nl
		     "Host: twitter.com" nl
		     "Authorization: Basic "
		     (base64-encode-string
		      (concat twittering-username ":" twittering-password))
		     nl
		     "Content-Type: text/plain" nl
		     "Content-Length: 0" nl
		     nl nl))))
      (error
       (message "Failure: HTTP POST") nil))))

(defun twittering-http-post-default-sentinel (proc stat &optional suc-msg)
  
  (condition-case err-signal
      (let ((header (twittering-get-response-header))
	    ; (body (twittering-get-response-body)) not used now.
	    (status nil))
	(string-match "HTTP/1\.1 \\([a-z0-9 ]+\\)\r?\n" header)
	(setq status (match-string-no-properties 1 header))
	(case-string status
		     (("200 OK")
		      (message (if suc-msg suc-msg "Success: Post")))
		     (t (message status)))
	)
    (error (message (twittering-inspect-object err-signal))))
  )

(defun twittering-get-response-header (&optional buffer)
  "Exract HTTP response header from HTTP response.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, the value of `twittering-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (twittering-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (substring content 0 (string-match "\r?\n\r?\n" content)))))

(defun twittering-get-response-body (&optional buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a XML tree as list.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, the value of `twittering-http-buffer' is used as `buffer'."
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (null buffer) (setq buffer (twittering-http-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((content (buffer-string)))
      (let ((content (buffer-string)))
	(xml-parse-region (+ (string-match "\r?\n\r?\n" content)
			     (length (match-string 0 content)))
			  (point-max)))
      )))

(defun twittering-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default twittering-friends-timeline-data)
If STATUS-DATUM is already in DATA-VAR, return nil. If not, return t."
  (if (null data-var)
      (setf data-var 'twittering-friends-timeline-data))
  (let ((id (cdr (assq 'id status-datum)))) 
    (if (or (null (symbol-value data-var))
	    (not (find-if
		  (lambda (item)
		    (eql id (cdr (assq 'id item))))
		  (symbol-value data-var))))
	(progn
	  (set data-var (cons status-datum (symbol-value data-var)))
	  t)
      nil)))

(defun twittering-status-to-status-datum (status)
  (let ((status-data (cddr status))
	id text time user-data username icon-url icon-string regex-index)
    (setq id (string-to-number (car (cddr (assq 'id status-data)))))
    (setq text (car (cddr (assq 'text status-data))))
    (setq text (twittering-decode-html-entities text))
    (setq time (car (cddr (assq 'created_at status-data))))
    (setq user-data (cddr (assq 'user status-data)))
    (setq username (car (cddr (assq 'screen_name user-data))))
    (setq icon-url (car (cddr (assq 'profile_image_url user-data))))

    ;; download icons if does not exist
    (if (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" icon-url)
	(let ((filename (match-string-no-properties 1 icon-url)))
	  (setq icon-string filename)
	  (if (file-exists-p (concat twittering-tmp-dir
				     "/" filename))
	      t
	    (add-to-list 'twittering-image-stack icon-url))))
    
    ;; make username clickable
    (add-text-properties 0 (length username)
			 `(mouse-face highlight
			   uri ,(concat "http://twitter.com/" username)
			   username ,username)
			 username)

    ;; make URI clickable
    (setq regex-index 0)
    (while regex-index
      (setq regex-index
	    (string-match "https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+"
			  text
			  regex-index))
      (if regex-index
	  (progn
	    (incf regex-index)
	    (add-text-properties
	     (match-beginning 0) (match-end 0)
	     `(mouse-face highlight
	       uri ,(match-string 0 text))
	     text))))
    `((id . ,id)
      (username . ,username)
      (text . ,text)
      (time . ,time)
      (icon-string . ,icon-string))))


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

(defun twittering-url-encode (str)
  str)

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
  (let (entity (ret encoded-str))
    (while (string-match "&#\\([0-9]+\\);" ret)
      (setq entity (match-string-no-properties 1 ret))
      (setq
       ret
       (replace-match
	(char-to-string
	 (twittering-ucs-to-char (string-to-number entity)))
	t t ret)))
    ret))

(defun twittering-timer-action (func)
  (let ((buf (get-buffer twittering-buffer)))
    (if (null buf)
	(twittering-stop)
      (funcall func)
      )))

(defun twittering-update-status-if-not-blank (status)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (twittering-http-post "statuses" "update"
			  `(("status" . ,status)
			    ("source" . "twmode")))
    t))

(defun twittering-update-status-from-minibuffer (&optional init-str)
  (if (null init-str) (setq init-str ""))
  (let ((status init-str) (not-posted-p t))
    (while not-posted-p
      (setq status (read-from-minibuffer "status: " status nil nil nil nil t))
      (setq not-posted-p
	    (not (twittering-update-status-if-not-blank status))))))

;;;
;;; Commands
;;;

(defun twittering-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'twittering-friends-timeline))
  (if twittering-timer
      nil
    (setq twittering-timer
	  (run-at-time "0 sec"
		       twittering-timer-interval
		       #'twittering-timer-action action))))

(defun twittering-stop ()
  (interactive)
  (cancel-timer twittering-timer)
  (setq twittering-timer nil))

(defun twittering-friends-timeline ()
  (interactive)
  (let ((buf (get-buffer twittering-buffer)))
    (if (not buf)
	(twittering-stop)
      (twittering-http-get "statuses" "friends_timeline")
      ))

  (if twittering-icon-mode
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

(defun twittering-update-status-interactive ()
  (interactive)
  (twittering-update-status-from-minibuffer))

(defun twittering-erase-old-statuses ()
  (interactive)
  (setq twittering-friends-timeline-data nil)
  (twittering-http-get "statuses" "friends_timeline"))

(defun twittering-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twittering-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
	(uri (get-text-property (point) 'uri)))
    (if username
	(twittering-update-status-from-minibuffer (concat "@" username " "))
      (if uri
	  (browse-url uri)))))

(defun twittering-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
	(browse-url uri))))

(defun twittering-reply-to-user ()
  (interactive)
  (let ((username (get-text-property (point) 'username)))
    (if username
	(twittering-update-status-from-minibuffer (concat "@" username " ")))))

(provide 'twittering-mode)
;;; twittering.el ends here
