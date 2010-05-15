
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
  (test-assert-string-equal "love+plus"
    (twittering-percent-encode "love plus"))
  (test-assert-string-equal "%0a"
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
    (test-assert-string-equal "[GNU project]"
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

(defcase test-url-reserved-p nil nil
  (test-assert-ok (twittering-url-reserved-p ?a))
  (test-assert-ok (twittering-url-reserved-p ?A))
  (test-assert-ok (twittering-url-reserved-p ?Z))
  (test-assert-ok (twittering-url-reserved-p ?z))
  (test-assert-ok (not (twittering-url-reserved-p ?\[)))
  (test-assert-ok (not (twittering-url-reserved-p ?\])))
  (test-assert-ok (not (twittering-url-reserved-p ?\\)))
  (test-assert-ok (not (twittering-url-reserved-p ?^)))
  (test-assert-ok (not (twittering-url-reserved-p ?`))))

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
