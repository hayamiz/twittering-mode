
(defcase test-version nil nil
  (test-assert-string-match "^twittering-mode-v[0-9]+\\(\\.[0-9]+\\)*"
    (twittering-mode-version)))

(defcase test-buffer nil nil
  (test-assert-ok (bufferp (twittering-buffer)))
  (test-assert-ok (buffer-live-p (twittering-buffer)))
  (test-assert-string-match (regexp-opt (list twittering-buffer))
    (buffer-name (twittering-buffer))))

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
  (test-assert-ok (file-directory-p twittering-tmp-dir))
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
    (twittering-percent-encode "love plus")))

(when (require 'url nil t)
  (defcase tinyurl nil nil
    (test-assert-string-equal "http://tinyurl.com/3xsrg5"
      (twittering-tinyurl-get "http://example.com/example"))
    ))

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

(lexical-let ((status (car (get-fixture 'timeline-data))))
  (defcase test-format-status nil nil
    (test-assert-string-equal "hello world"
      (twittering-format-status status "hello world"))
    (test-assert-string-equal "%"
      (twittering-format-status status "%%"))



    (test-assert-string-equal "something like emacs"
      (twittering-format-status status "something like %s"))

    (test-assert-string-equal "We love emacs!"
      (twittering-format-status status "We love %S!"))

    (setq twittering-icon-mode nil)
    (test-assert-string-equal ""
      (twittering-format-status status "%i"))
    (setq twittering-icon-mode t)
    (test-assert-ok
	(string-match "\\s-+" (twittering-format-status status "%i")))

    (test-assert-string-equal
	"Emacs is the extensible self-documenting text editor."
      (twittering-format-status status "%d"))

    (test-assert-string-equal "GNU project"
      (twittering-format-status status "%l"))
    (test-assert-string-equal " [GNU project]"
      (twittering-format-status status "%L"))
    (setcdr (assoc 'user-location status) "")
    (test-assert-string-equal ""
      (twittering-format-status status "%l"))
    (test-assert-string-equal ""
      (twittering-format-status status "%L"))

    (setcdr (assoc 'in-reply-to-screen-name status) "hoge")
    (setcdr (assoc 'in-reply-to-status-id status) "123456")
    (test-assert-string-equal " in reply to hoge"
      (twittering-format-status status "%r"))
    (setcdr (assoc 'in-reply-to-screen-name status) "foo")
    (setcdr (assoc 'in-reply-to-status-id status) "")
    (test-assert-string-equal ""
      (twittering-format-status status "%r"))
    (setcdr (assoc 'in-reply-to-screen-name status) "")
    (setcdr (assoc 'in-reply-to-status-id status) "654321")
    (test-assert-string-equal ""
      (twittering-format-status status "%r"))
    (setcdr (assoc 'in-reply-to-screen-name status) "")
    (setcdr (assoc 'in-reply-to-status-id status) "")
    (test-assert-string-equal ""
      (twittering-format-status status "%r"))

    (test-assert-string-equal "http://www.gnu.org/software/emacs/"
      (twittering-format-status status "%u"))

    (test-assert-string-equal "9492852"
      (twittering-format-status status "%j"))

    (test-assert-string-equal ""
      (twittering-format-status status "%p"))
    (setcdr (assoc 'user-protected status) "true")
    (test-assert-string-equal "[x]"
      (twittering-format-status status "%p"))
    (setcdr (assoc 'user-protected status) "false")
    (test-assert-string-equal ""
      (twittering-format-status status "%p"))

    (test-assert-string-equal "created at Wed Dec 09 00:44:57 +0000 2009"
      (twittering-format-status status "created at %c"))

    (test-assert-string-equal "created at 2009/12/09 09:44:57"
      (twittering-format-status status "created at %C{%Y/%m/%d %H:%M:%S}"))

    ;; (test-assert-string-equal "09:44 午前 12月 09, 2009"
    ;;   (twittering-format-status status "%@"))

    (test-assert-string-equal "Help protect and support Free Software and the GNU Project by joining the Free Software Foundation! http://www.fsf.org/join?referrer=7019"
      (twittering-format-status status "%t"))

    (setcdr (assoc 'truncated status) "false")
    (test-assert-string-equal ""
      (twittering-format-status status "%'"))
    (setcdr (assoc 'truncated status) "true")
    (test-assert-string-equal "..."
      (twittering-format-status status "%'"))
    (setcdr (assoc 'truncated status) "false")

    (test-assert-string-equal "web"
      (twittering-format-status status "%f"))

    (test-assert-string-equal "6480639448"
      (twittering-format-status status "%#"))

    (setq twittering-icon-mode nil)
    (test-assert-string-equal " emacs,  :
  Help protect and support Free Software and the GNU Project by joining the Free Software Foundation! http://www.fsf.org/join?referrer=7019 // from web"
      (twittering-format-status status "%i %s,  :\n  %t // from %f%L%r"))
    (setq twittering-icon-mode t)
    (test-assert-string-equal "
   emacs,  :
  Help protect and support Free Software and the GNU Project by joining the Free Software Foundation! http://www.fsf.org/join?referrer=7019 // from web"
      (twittering-format-status status "%i %s,  :\n  %t // from %f%L%r"))
    ))

(defcase test-find-curl-program nil nil
  (test-assert-string-match "curl" (twittering-find-curl-program))
  (with-temp-buffer
    (when (twittering-find-curl-program)
      (test-assert-eq 0
	(shell-command (format "%s --help" (twittering-find-curl-program) t))))))

(defcase test-ensure-ca-cert nil nil
  (when (twittering-find-curl-program)
    (test-assert-eq 0
      (call-process (twittering-find-curl-program)
		    nil "hoge" nil
		    "--cacert"
		    (twittering-ensure-ca-cert)
		    "https://twitter.com/"))))

