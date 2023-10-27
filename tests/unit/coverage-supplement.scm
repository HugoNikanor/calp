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

 ("module/datetime/zic.scm"
  "2a8ac0fae3c88227b05a5978bff3e114745ea146e07a2751df67d16c1e8218f5"
  66 ; syntax-rules
  )


 ("module/hnh/util.scm"
  "3f0bf90a45d6eecce1248b7509e1b050e5cadbe92b279fe5ef082c18baf3e6ca"
  72 73 74 ; conditional import
  77 ; syntax rules not covered
  333 568 ; syntax rules
  )

 ("module/calp/translation.scm"
  "b7c0a82e1c109c339cf83438f39b6fc63219b51a3ad3ee35d58e70fb6a24c5c9"
  9 ; bindtextdomain
  )



 ;; this file simply exports other things. There's nothing to test
 ("module/vcomponent.scm"
  "b1c58b3beb6f170d3c9f7d603b27231ccf696897736113095b446f437721a9e1"
  2)

 ;; get-terminal-size is basically impossible to test
 ("module/vulgar/info.scm"
  "f9f30fd2709a5614b986c697e089c36c7d5d8cd3824e6d4e2bac042d5c2c23e6"
  2)



;;; Vendored files, and therefore shouldn't be tested

 ("module/srfi/srfi-64/test-error.scm"
  "15a0eb700de629a9e79aec8a1fde113fbc9542d052163ede46b433d630b7b01c"
  2)

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
