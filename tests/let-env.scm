(((guile) setenv getenv)
 ((hnh util) let-env))

(setenv "CALP_TEST_ENV" "1")
(test-equal "Ensure we have set value beforehand"
  "1" (getenv "CALP_TEST_ENV"))
(let-env ((CALP_TEST_ENV "2"))
         (test-equal "Test our local override"
           "2" (getenv "CALP_TEST_ENV")))
(test-equal "Test that we have returned"
  "1" (getenv "CALP_TEST_ENV"))

(catch 'test-error
  (lambda ()
   (let-env ((CALP_TEST_ENV "2"))
            (test-equal "Test our local override again"
              "2" (getenv "CALP_TEST_ENV"))
            (throw 'test-error)))
  list)

(test-equal "Test restoration after non-local exit"
  "1" (getenv "CALP_TEST_ENV"))
