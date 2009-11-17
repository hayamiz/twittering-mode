
(defsuite twittering-mode nil)

(deftest case-string twittering-mode
  (assert
   (case-string "Rinko"
     (("Rinko") t)
     (t nil)))
  (assert
   (case-string "Rinko"
     (("Manaka") nil)
     (("Nene" "Rinko") t)
     (t nil)))
  (assert
   (case-string "Nene"
     (("Manaka" "Rinko") nil)
     (t t)))
  )

(deftest percent-encode twittering-mode
  (assert-equal "Rinko"
    (twittering-percent-encode "Rinko"))
  (assert-equal "%25"
    (twittering-percent-encode "%"))
  (assert-equal "love+plus"
    (twittering-percent-encode "love plus")))
