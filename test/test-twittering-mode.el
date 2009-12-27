
(defcase percent-encode nil nil
  (test-assert-string-equal "Rinko"
    (twittering-percent-encode "Rinko"))
  
  (test-assert-string-equal "%25"
    (twittering-percent-encode "%"))
  
  (test-assert-string-equal "love+plus"
    (twittering-percent-encode "love plus")))

(defcase tinyurl nil nil
  (test-assert-string-equal "http://tinyurl.com/3xsrg5"
    (twittering-tinyurl-get "http://example.com/example"))
  )

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

(defcase sign-string nil nil
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

