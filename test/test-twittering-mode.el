
(when (and (> 22 emacs-major-version)
	   (require 'url-methods nil t))
  ;; `twittering-make-http-request-from-uri', which is called for the format
  ;; specifier "%i", requires `url-generic-parse-url'. But the function for
  ;; Emacs 21, which is distributed with twittering-mode, does not work
  ;; correctly until calling `url-scheme-get-property'.
  (url-scheme-get-property "http" 'name)
  (url-scheme-get-property "https" 'name))

(defcase test-version nil nil
  (test-assert-string-match "^twittering-mode-v\\([0-9]+\\(\\.[0-9]+\\)*\\|HEAD\\)"
    (twittering-mode-version)))

(defcase test-assocref nil nil
  (test-assert-eq 'bar (assocref 'foo '((baz . qux) (foo . bar))))
  (test-assert-eq nil (assocref 'quxx '((baz . qux) (foo . bar)))))

(defcase test-toggle-proxy nil nil
  (setq twittering-proxy-use nil)
  (twittering-toggle-proxy)
  (test-assert-ok twittering-proxy-use)
  (twittering-toggle-proxy)
  (test-assert-ok (not twittering-proxy-use)))

(defcase test-sign-string nil nil
  (setq twittering-sign-simple-string nil)
  (test-assert-string-equal ""
    (twittering-sign-string))

  (setq twittering-sign-simple-string "")
  (test-assert-string-equal " []"
    (twittering-sign-string))

  (setq twittering-sign-simple-string "foo")
  (test-assert-string-equal " [foo]"
    (twittering-sign-string))

  (setq twittering-sign-string-function (lambda () "foo"))
  (test-assert-string-equal "foo"
    (twittering-sign-string))
  )

(defcase test-user-agent nil nil
  (test-assert-string-equal (format "Emacs/%d.%d Twittering-mode/%s"
				    emacs-major-version
				    emacs-minor-version
				    twittering-mode-version)
    (twittering-user-agent))
  (setq twittering-user-agent-function
	(lambda () "foo user agent"))
  (test-assert-string-equal "foo user agent"
    (twittering-user-agent))
  )

(defcase test-icon-mode nil nil
  (setq twittering-icon-mode nil)
  (twittering-icon-mode)
  (test-assert-ok twittering-icon-mode)
  (twittering-icon-mode)
  (test-assert-ok (not twittering-icon-mode))
  (twittering-icon-mode nil)
  (test-assert-ok twittering-icon-mode)
  (twittering-icon-mode t)
  (test-assert-ok twittering-icon-mode)
  (twittering-icon-mode -1)
  (test-assert-ok (not twittering-icon-mode))
  (twittering-icon-mode 1)
  (test-assert-ok twittering-icon-mode)
  )

(defcase test-scroll-mode nil nil
  (setq twittering-scroll-mode nil)
  (twittering-scroll-mode)
  (test-assert-ok twittering-scroll-mode)
  (twittering-scroll-mode)
  (test-assert-ok (not twittering-scroll-mode))
  (twittering-scroll-mode nil)
  (test-assert-ok twittering-scroll-mode)
  (twittering-scroll-mode t)
  (test-assert-ok twittering-scroll-mode)
  (twittering-scroll-mode 1)
  (test-assert-ok twittering-scroll-mode)
  (twittering-scroll-mode -1)
  (test-assert-ok (not twittering-scroll-mode)))
  

(defcase test-percent-encode nil nil
  (test-assert-string-equal "Rinko"
    (twittering-percent-encode "Rinko"))
  (test-assert-string-equal "%25"
    (twittering-percent-encode "%"))
  (test-assert-string-equal "love%20plus"
    (twittering-percent-encode "love plus"))
  (test-assert-string-equal "%0A"
    (twittering-percent-encode "\n")))

(with-network
 (when (require 'url nil t)
   (defcase tinyurl nil nil
     (test-assert-string-equal "http://tinyurl.com/3xsrg5"
       (twittering-tinyurl-get "http://example.com/example"))
     )))

(defcase case-string nil nil
  (test-assert-string-equal "Kobayakawa"
    (case-string "Rinko"
      (("Rinko") "Kobayakawa")
      (t "unknown")))

  (test-assert-string-equal "unknown"
    (case-string "Manaka"
      (("Rinko") "Kobayakawa")
      (t "unknown")))
  
  (test-assert-string-equal "Kobayakawa"
    (case-string "Rinko"
      (("Manaka") "Takane")
      (("Rinko") "Kobayakawa")
      (("Nene") "Anegasaki")
      (t nil)))

  (test-assert-string-equal "Amphibian"
    (case-string "Frog"
      (("Rabbit") "Mammal")
      (("Salamandar" "Frog") "Amphibian")
      (t nil)))
  )

(defcase format-string nil nil
  (test-assert-string-equal ""
    (twittering-format-string "" "" nil))

  (test-assert-string-equal "Hello world"
    (twittering-format-string "Hello world" "" nil))

  (test-assert-string-equal "RT: twittering-mode now (via @twmode)"
    (twittering-format-string "RT: %t (via @%s)" "%"
			      '(("t" . "twittering-mode now")
				("s" . "twmode"))))

  (test-assert-string-equal "RT: %t (via @twmode)"
    (twittering-format-string "RT: %t (via @%s)" "%"
			      '(("t" . "%t")
				("s" . "twmode"))))

  (test-assert-string-equal "new\nline"
    (twittering-format-string "new~%line" "~"
			      '(("%" . "\n"))))
  )

(defun test-restore-timeline-spec(spec-str spec normalized-spec)
  (let ((spec-from-str
	 (twittering-string-to-timeline-spec spec-str)))
    (list
     (equal (twittering-string-to-timeline-spec
	     (twittering-timeline-spec-to-string spec))
	    normalized-spec)
     (equal normalized-spec spec-from-str))))

(defcase timeline-spec nil nil
  (test-assert-equal
   (test-restore-timeline-spec
    "(user+(user/mylist+(@))+:filter/WORD/(USER2+:mentions))"
    '(merge (user "user")
	    (merge (list "user" "mylist")
		   (merge (replies)))
	    (filter "WORD" (merge (user "USER2")
				  (mentions))))
    '(merge (user "user")
	    (list "user" "mylist")
	    (replies)
	    (filter "WORD" (merge (user "USER2")
				  (mentions)))))
   '(t t))
  (test-assert-equal
   (test-restore-timeline-spec
    "(user-A+~+((user-B))+(:filter/R+/user-C+(:filter/R3\\//USER-D+:public)))"
    '(merge (user "user-A")
	    (home)
	    (merge (user "user-B")
		   (filter "R+" (user "user-C")))
	    (filter "R3/" (user "USER-D"))
	    (public))
    '(merge (user "user-A")
	    (home)
	    (user "user-B")
	    (filter "R+" (user "user-C"))
	    (filter "R3/" (user "USER-D"))
	    (public)))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":filter/ABC/user/mylist"
    '(filter "ABC" (list "user" "mylist"))
    '(filter "ABC" (list "user" "mylist")))
   '(t t))
  (test-assert-equal
   (test-restore-timeline-spec
    ":filter/ABC\\/user/mylist"
    '(filter "ABC/user" (user "mylist"))
    '(filter "ABC/user" (user "mylist")))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":filter/ABC\\\\/user/mylist"
    '(filter "ABC\\\\" (list "user" "mylist"))
    '(filter "ABC\\\\" (list "user" "mylist")))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":filter/\\\\/user/mylist"
    '(filter "\\\\" (list "user" "mylist"))
    '(filter "\\\\" (list "user" "mylist")))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":retweeted_by_me" '(retweeted_by_me)  '(retweeted_by_me))
   '(t t))
  (test-assert-equal
   (test-restore-timeline-spec
    ":retweeted_to_me" '(retweeted_to_me) '(retweeted_to_me))
   '(t t))
  (test-assert-equal
   (test-restore-timeline-spec
    ":retweets_of_me" '(retweets_of_me)  '(retweets_of_me))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":favorites" '(favorites) '(favorites))
   '(t t))

  (test-assert-equal
   (test-restore-timeline-spec
    ":favorites/USER" '(favorites "USER") '(favorites "USER"))
   '(t t))
  )

(defun format-status (status format-str)
  (twittering-update-status-format format-str)
  (twittering-format-status status))

(lexical-let ((status (car (get-fixture 'timeline-data))))
  (defcase test-format-status nil nil
    (test-assert-string-equal "hello world"
      (format-status status "hello world"))
    (test-assert-string-equal "%"
      (format-status status "%%"))



    (test-assert-string-equal "something like emacs"
      (format-status status "something like %s"))

    (test-assert-string-equal "We love emacs!"
      (format-status status "We love %S!"))

    (test-assert-string-equal
     ""
     (let ((twittering-icon-mode nil))
       (format-status status "%i")))
    (test-assert-string-equal
     " "
     (let ((twittering-icon-mode t)
	   (window-system t))
       (format-status status "%i")))

    (test-assert-string-equal
	"Emacs is the extensible self-documenting text editor."
      (format-status status "%d"))

    (test-assert-string-equal "GNU project"
      (format-status status "%l"))
    (test-assert-string-equal " [GNU project]"
      (format-status status "%L"))
    (setcdr (assoc 'user-location status) "")
    (test-assert-string-equal ""
      (format-status status "%l"))
    (test-assert-string-equal ""
      (format-status status "%L"))

    (setcdr (assoc 'in-reply-to-screen-name status) "hoge")
    (setcdr (assoc 'in-reply-to-status-id status) "123456")
    (test-assert-string-equal " in reply to hoge"
      (format-status status "%r"))
    (setcdr (assoc 'in-reply-to-screen-name status) "foo")
    (setcdr (assoc 'in-reply-to-status-id status) "")
    (test-assert-string-equal ""
      (format-status status "%r"))
    (setcdr (assoc 'in-reply-to-screen-name status) "")
    (setcdr (assoc 'in-reply-to-status-id status) "654321")
    (test-assert-string-equal ""
      (format-status status "%r"))
    (setcdr (assoc 'in-reply-to-screen-name status) "")
    (setcdr (assoc 'in-reply-to-status-id status) "")
    (test-assert-string-equal ""
      (format-status status "%r"))

    (test-assert-string-equal "http://www.gnu.org/software/emacs/"
      (format-status status "%u"))

    (test-assert-string-equal "9492852"
      (format-status status "%j"))

    (test-assert-string-equal ""
      (format-status status "%p"))
    (setcdr (assoc 'user-protected status) "true")
    (test-assert-string-equal "[x]"
      (format-status status "%p"))
    (setcdr (assoc 'user-protected status) "false")
    (test-assert-string-equal ""
      (format-status status "%p"))

    (test-assert-string-equal "created at Wed Dec 09 00:44:57 +0000 2009"
      (format-status status "created at %c"))

    (test-assert-string-equal "created at 2009/12/09 09:44:57"
      (format-status status "created at %C{%Y/%m/%d %H:%M:%S}"))

    ;; (test-assert-string-equal "09:44 午前 12月 09, 2009"
    ;;   (format-status status "%@"))

    (test-assert-string-equal "Help protect and support Free Software and the GNU Project by joining the Free Software Foundation! http://www.fsf.org/join?referrer=7019"
      (format-status status "%T"))

    (setcdr (assoc 'truncated status) "false")
    (test-assert-string-equal ""
      (format-status status "%'"))
    (setcdr (assoc 'truncated status) "true")
    (test-assert-string-equal "..."
      (format-status status "%'"))
    (setcdr (assoc 'truncated status) "false")

    (test-assert-string-equal "web"
      (format-status status "%f"))

    (test-assert-string-equal "6480639448"
      (format-status status "%#"))

    (test-assert-string-equal
     " emacs,  :
  Help protect and support Free Software and the GNU Project by joining the Free Software Foundation! http://www.fsf.org/join?referrer=7019 // from web"
     (let ((twittering-icon-mode nil))
       (format-status status "%i %s,  :\n  %T // from %f%L%r")))
    (test-assert-string-equal
     "  emacs,  :
  Help protect and support Free Software and the GNU Project by joining the Free Software Foundation! http://www.fsf.org/join?referrer=7019 // from web"
     (let ((twittering-icon-mode t)
	   (window-system t))
       (format-status status "%i %s,  :\n  %T // from %f%L%r")))

    (test-assert-string-equal
     "
  Help protect and support Free Software and the GNU Project by joining the
  Free Software Foundation! http://www.fsf.org/join?referrer=7019 // from web"
     (let ((twittering-fill-column 80))
       (format-status status "\n%FILL[  ]{%T // from %f%L%r}")))

    (test-assert-string-equal
     "
  Help protect and support Free Software and the GNU Project by joining the
  Free Software Foundation! http://www.fsf.org/join?referrer=7019 // from web"
     (let ((twittering-fill-column 80))
       (format-status status "\n%FOLD[  ]{%T // from %f%L%r}")))

    (test-assert-string-equal
     "
  Edit XHTML5 documents in nxml-mode with on-the-fly validation:
  http://bit.ly/lYnEg (by @hober) // from web"
     (let ((twittering-fill-column 80)
	   (oldest-status (car (last (get-fixture 'timeline-data)))))
       (format-status oldest-status "\n%FILL[  ]{%T // from %f%L%r}")))

    (test-assert-string-equal
     "
--Edit XHTML5 documents in nxml-mode with on-the-fly validation:
--http://bit.ly/lYnEg
--			     
--	(by @hober) // from web"
     (let ((twittering-fill-column 80)
	   (oldest-status (car (last (get-fixture 'timeline-data)))))
       (format-status oldest-status "\n%FOLD[--]{%T // from %f%L%r}")))
    ))

(defcase test-find-curl-program nil nil
  (test-assert-string-match "curl" (twittering-find-curl-program))
  (with-temp-buffer
    (when (twittering-find-curl-program)
      (test-assert-eq 0
	(call-process (twittering-find-curl-program)
		      nil (current-buffer) nil
		      "--help")))))

(with-network
 (defcase test-ensure-ca-cert nil nil
   (when (twittering-find-curl-program)
     (test-assert-eq 0
       (with-temp-buffer
	 (call-process (twittering-find-curl-program)
		       nil (current-buffer) nil
		       "--cacert"
		       (twittering-ensure-ca-cert)
		       "https://twitter.com/"))))))

(defcase test-status-not-blank-p nil nil
  (test-assert-ok (not (twittering-status-not-blank-p "")))
  (test-assert-ok (not (twittering-status-not-blank-p "\n")))
  (test-assert-ok (not (twittering-status-not-blank-p "@foo")))
  (test-assert-ok (not (twittering-status-not-blank-p "@bar ")))
  (test-assert-ok (twittering-status-not-blank-p "hello"))
  (test-assert-ok (twittering-status-not-blank-p "@baz hello"))
  (test-assert-ok (twittering-status-not-blank-p "@baz\n\nhello"))
  (test-assert-ok (twittering-status-not-blank-p "\nhello"))
  (test-assert-ok (twittering-status-not-blank-p "hello\n"))
  (test-assert-ok (twittering-status-not-blank-p "@foo hello @bar"))
  (test-assert-ok (twittering-status-not-blank-p "hello @foo"))
  )

(defcase test-hmac-sha1 nil nil
  ;; The following tests are copied from RFC 2202.
  (test-assert-string-equal
   (let* ((v (make-list 20 ?\x0b))
	  (key (cond
		((fboundp 'unibyte-string) (apply 'unibyte-string v))
		(t (concat v))))
	  (data "Hi There"))
     (mapconcat (lambda (c) (format "%02x" c))
		(twittering-hmac-sha1 key data)
		""))
   "b617318655057264e28bc0b6fb378c8ef146be00")

  (test-assert-string-equal
   (let* ((key "Jefe")
	  (data "what do ya want for nothing?"))
     (mapconcat (lambda (c) (format "%02x" c))
		(twittering-hmac-sha1 key data)
		""))
   "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79")

  (test-assert-string-equal
   (let* ((key-v (make-list 20 ?\xaa))
	  (key (cond
		((fboundp 'unibyte-string) (apply 'unibyte-string key-v))
		(t (concat key-v))))
	  (data-v (make-list 50 ?\xdd))
	  (data (cond
		 ((fboundp 'unibyte-string) (apply 'unibyte-string data-v))
		 (t (concat data-v)))))
     (mapconcat (lambda (c) (format "%02x" c))
		(twittering-hmac-sha1 key data)
		""))
   "125d7342b9ac11cd91a39af48aa17b4f63f175d3")
  )

(defcase test-oauth nil nil
  ;; "Authenticating Requests | dev.twitter.com"
  ;; http://dev.twitter.com/pages/auth
  (setq sample-consumer-key "GDdmIQH6jhtmLUypg82g")
  (setq sample-consumer-secret "MCD8BKwGdgPHvAuvgvz4EQpqDAtx89grbuNMRd7Eh98")

  ;; Acquiring a request token
  ;; http://dev.twitter.com/pages/auth#request-token
  (test-assert-string-equal
   (let* ((oauth-params
	   `(("oauth_nonce" . "QP70eNmVz8jvdPevU3oJD2AfF7R7odC2XJcn4XlZJqk")
	     ("oauth_callback" . ,(twittering-oauth-url-encode "http://localhost:3005/the_dance/process_callback?service_provider_id=11"))
	     ("oauth_signature_method" . "HMAC-SHA1")
	     ("oauth_timestamp" . "1272323042")
	     ("oauth_consumer_key" . ,sample-consumer-key)
	     ("oauth_version" . "1.0")))
	  (url "https://api.twitter.com/oauth/request_token"))
     (twittering-oauth-auth-str-request-token
      url nil sample-consumer-key sample-consumer-secret oauth-params))
   "OAuth oauth_nonce=\"QP70eNmVz8jvdPevU3oJD2AfF7R7odC2XJcn4XlZJqk\",oauth_callback=\"http%3A%2F%2Flocalhost%3A3005%2Fthe_dance%2Fprocess_callback%3Fservice_provider_id%3D11\",oauth_signature_method=\"HMAC-SHA1\",oauth_timestamp=\"1272323042\",oauth_consumer_key=\"GDdmIQH6jhtmLUypg82g\",oauth_version=\"1.0\",oauth_signature=\"8wUi7m5HFQy76nowoCThusfgB%2BQ%3D\"")

  ;; response
  (test-assert-equal
   (let ((response-str "oauth_token=8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc&oauth_token_secret=x6qpRnlEmW9JbQn4PQVVeVG8ZLPEx6A0TOebgwcuA&oauth_callback_confirmed=true"))
     (twittering-oauth-make-response-alist response-str))
   '(("oauth_token" . "8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc")
     ("oauth_token_secret"
      . "x6qpRnlEmW9JbQn4PQVVeVG8ZLPEx6A0TOebgwcuA")
     ("oauth_callback_confirmed" . "true")))

  ;; Sending the user to authorization
  ;; http://dev.twitter.com/pages/auth#authorization
  ;; response (when using callback)
  (test-assert-equal
   (let ((response-str "oauth_token=8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc&oauth_verifier=pDNg57prOHapMbhv25RNf75lVRd6JDsni1AJJIDYoTY"))
     (twittering-oauth-make-response-alist response-str))
   '(("oauth_token" . "8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc")
     ("oauth_verifier"
      . "pDNg57prOHapMbhv25RNf75lVRd6JDsni1AJJIDYoTY")))

  ;; Exchanging a request token for an access token
  ;; http://dev.twitter.com/pages/auth#access-token
  (test-assert-string-equal
   (let* ((request-token "8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc")
	  (request-token-secret "x6qpRnlEmW9JbQn4PQVVeVG8ZLPEx6A0TOebgwcuA")
	  (verifier "pDNg57prOHapMbhv25RNf75lVRd6JDsni1AJJIDYoTY")
	  (oauth-params
	   `(("oauth_nonce" . "9zWH6qe0qG7Lc1telCn7FhUbLyVdjEaL3MO5uHxn8")
	     ("oauth_signature_method" . "HMAC-SHA1")
	     ("oauth_timestamp" . "1272323047")
	     ("oauth_consumer_key" . ,sample-consumer-key)
	     ("oauth_token" . ,request-token)
	     ("oauth_verifier" . ,verifier)
	     ("oauth_version" . "1.0")))
	  (url "https://api.twitter.com/oauth/access_token"))
     (twittering-oauth-auth-str-exchange-token
      url nil
      sample-consumer-key sample-consumer-secret
      request-token request-token-secret verifier oauth-params))
   "OAuth oauth_nonce=\"9zWH6qe0qG7Lc1telCn7FhUbLyVdjEaL3MO5uHxn8\",oauth_signature_method=\"HMAC-SHA1\",oauth_timestamp=\"1272323047\",oauth_consumer_key=\"GDdmIQH6jhtmLUypg82g\",oauth_token=\"8ldIZyxQeVrFZXFOZH5tAwj6vzJYuLQpl0WUEYtWc\",oauth_verifier=\"pDNg57prOHapMbhv25RNf75lVRd6JDsni1AJJIDYoTY\",oauth_version=\"1.0\",oauth_signature=\"PUw%2FdHA4fnlJYM6RhXk5IU%2F0fCc%3D\"")

  ;; response
  (test-assert-equal
   (let ((response-str "oauth_token=819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw&oauth_token_secret=J6zix3FfA9LofH0awS24M3HcBYXO5nI1iYe8EfBA&user_id=819797&screen_name=episod"))
     (twittering-oauth-make-response-alist response-str))
   '(("oauth_token"
      . "819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw")
     ("oauth_token_secret"
      . "J6zix3FfA9LofH0awS24M3HcBYXO5nI1iYe8EfBA")
     ("user_id" . "819797")
     ("screen_name" . "episod")))

  ;; Making a resource request on a user's behalf
  ;; http://dev.twitter.com/pages/auth#auth-request
  (test-assert-string-equal
   (let* ((access-token "819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw")
	  (access-token-secret "J6zix3FfA9LofH0awS24M3HcBYXO5nI1iYe8EfBA")
	  (oauth-params
	   `(("oauth_nonce" . "oElnnMTQIZvqvlfXM56aBLAf5noGD0AQR3Fmi7Q6Y")
	     ("oauth_signature_method" . "HMAC-SHA1")
	     ("oauth_timestamp" . "1272325550")
	     ("oauth_consumer_key" . ,sample-consumer-key)
	     ("oauth_token" . ,access-token)
	     ("oauth_version" . "1.0")))
	  (url "http://api.twitter.com/1/statuses/update.json")
	  (encoded-query-parameters
	   `((,(twittering-percent-encode "status")
	      . ,(twittering-percent-encode
		  "setting up my twitter 私のさえずりを設定する")))))
     (twittering-oauth-auth-str-access
      "POST" url encoded-query-parameters
      sample-consumer-key sample-consumer-secret
      access-token access-token-secret
      oauth-params))
   "OAuth oauth_nonce=\"oElnnMTQIZvqvlfXM56aBLAf5noGD0AQR3Fmi7Q6Y\",oauth_signature_method=\"HMAC-SHA1\",oauth_timestamp=\"1272325550\",oauth_consumer_key=\"GDdmIQH6jhtmLUypg82g\",oauth_token=\"819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw\",oauth_version=\"1.0\",oauth_signature=\"yOahq5m0YjDDjfjxHaXEsW9D%2BX0%3D\"")
  )