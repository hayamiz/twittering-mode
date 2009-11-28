
;; (deftest case-string twittering-mode
;;   (assert
;;    (case-string "Rinko"
;;      (("Rinko") t)
;;      (t nil)))
;;   (assert
;;    (case-string "Rinko"
;;      (("Manaka") nil)
;;      (("Nene" "Rinko") t)
;;      (t nil)))
;;   (assert
;;    (case-string "Nene"
;;      (("Manaka" "Rinko") nil)
;;      (t t)))
;;   )

(defcase percent-encode nil nil
  (test-assert-string-equal "Rinko"
    (twittering-percent-encode "Rinko"))
  
  (test-assert-string-equal "%25"
    (twittering-percent-encode "%"))
  
  (test-assert-string-equal "love+plus"
    (twittering-percent-encode "love plus")))

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
