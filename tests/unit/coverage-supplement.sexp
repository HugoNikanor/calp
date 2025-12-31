;;; Guile's coverage system sometimes miss some definitions.
;;; Add these here so the output gets green.
;;; However, always start by attempting to add more tests to fill
;;; in the coverage.
;;;
;;; Each entry in this file should be a list consisting of:
;;; - The filename, relative calp's root
;;; - The sha256-sum of that file
;;; - Any number of lines which should be marked as covered.
(
 ("module/hnh/util/type.scm"
  "f670542b9b404125224fd4c702be99e2c1c3fd55d862b18228e8772264ef3189"
  12 34 44 46                            ; Macros
  53)                                    ; false? == not

 ("module/base64.scm"
  "4614855f6cfedc20041e7094989e817f2c2c5ef85fb5f8322d915101c0aab53c"
  23 24 25 26 27 ; internal lookup table, used by everything so tested implicitly
  53 54 55 56 ; internal helper function, tested implictly
 )

 ("module/text/util.scm"
  "271a5f7740aa6e378e7fda2da4725171dc50a2e4a790e9529fceed19a747e775"
  52                                    ; else "keyword" apparently missed
  )

 ("module/vcomponent/duration.scm"
  "c83a2750964c2362af5043f33b435a792f62007d847f543e78a8f2550757e010"
  ;; Unreachable code, but fail faster
  113 114 115
  118 119 120 121
  )

 ("module/crypto.scm"
  "9e157f5b53e923e7925b0e53118a4789b55712120427b73c4c3c9561e2c48718"
  8 ; dynamic link
  16 17 18 19 ; primitive sha256 procedure
  )

 ("module/calp/translation.scm"
  "b7c0a82e1c109c339cf83438f39b6fc63219b51a3ad3ee35d58e70fb6a24c5c9"
  9 ; bindtextdomain
  )



 ;; this file simply exports other things. There's nothing to test
 ("module/vcomponent.scm"
  "57554836a49c98263f326188a0caa90dfdade5a31ef6e5396c4f4feddb50dfe1"
  2)

 ("module/vcomponent/config.scm"
  "a7d2da32181d6c4013a9994620458ace4da786be68b2b0a5a14890d736bc4bd7"
  2)

 ;; get-terminal-size is basically impossible to test
 ("module/vulgar/info.scm"
  "f9f30fd2709a5614b986c697e089c36c7d5d8cd3824e6d4e2bac042d5c2c23e6"
  2)

 ;; File will be replaced with new system.
 ;; Techincally a test for the exported symbol `glob` could be
 ;; written, but it's way to much work, and since the tests already
 ;; depend on glob errors would quickly be found anyways.
 ("module/glob.scm"
  "2dfe76208aed52c36899b219683cc265333a042c9fd3b2a9a582aaeb8a453ba8"
  2)

 ;; This is barely a module. It only re-exports usefull utilities to
 ;; include in the configuration file.
 ;; NOTE possible look over if we really want this module, or if it
 ;; should be solved in some other way.
 ("module/calp/config-base.scm"
  "4da40542d408cd81cf80e37938118ab699ab0e9924d6d07edefe31d32bf29ae8"
  2)

 ;; Module simply exports static strings naming all namespaces used.
 ;; TODO shouldn't we pull namespaces from respective modules?
 ("module/calp/namespaces.scm"
  "a511671454f032760c6eabe53fefb29c2540615297ace1dc6024cafbb222d7c8"
  2)

 ;; The pretty print header might be testable, but the remaining is
 ;; only testable by running the tests, which the tests (obviously)
 ;; already do.
 ("module/hnh/test/testrunner.scm"
  "a0dc01c5bf4d2202d6f024e882abafebfb243d09d26fcf0e70008084bbd9c2f3"
  2)

 ;; with-mutex is pretty much impossible to test
 ("module/hnh/util/atomic.scm"
  "c81f950ccc84e33b4382f9a69f75092da0db268e946835f1950da7d7b8fb41d1"
  2)


 ;; Only defines configuration parametrs, nothing to test

 ("module/calp/html/config.scm"
  "5c4e1d0dac665d0e138a7c16684e782fc253c68d3d432d0c9f6b844a52b7fc5c"
  2)

 ("module/calp/html/filter.scm"
  "b0d2c4d890786867d43dc2904f6808d463d67f0b7bc14b772795137542547474"
  2)

 ("module/calp/util/exceptions.scm"
  "03ddd467a4acc28845aa4a8797210086a739bede315aa2693d10ad807ab2d50d"
  2)



;;; Vendored files, and therefore shouldn't be tested

 ("module/sxml/html.scm"
  "b4ffca46c9c723f6828e32d8798f1bbc89c2bfcb6f1368906b2d4bdef11951db"
  2)

 ("module/web/http.scm"
  "50637403dd63d6d1390903cdea17abf26ad09bcf7f95b9492298da30ebcb03ff"
  2)

 ;; File is not tested at all, since it's mearly a "header" file.
 ;; TODO possibly actually test extension loading, since it should
 ;; fail gracefully when library is not available.
 ("module/graphviz.scm"
  "5ff20a55098973fcc8552cd897e779eaf01dc3d4909f8d23be47d317987d8e95"
  2)
)
