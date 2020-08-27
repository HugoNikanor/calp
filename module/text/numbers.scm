
(eval-when (load)
  (throw 'do-not-load-me
         "Import (text numbers <langugage>) instead")
  )

;; scheme@(guile-user)> (number->string-cardinal 123)
;; $10 = "hundratjugotre"
;; scheme@(guile-user)> (number->string-ordinal 123)
;; $11 = "hundratjugotredje"
;; scheme@(guile-user)> (each-string 10)
;; $12 = "var tionde"
