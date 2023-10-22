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
 ("module/hnh/util/atomic-queue.scm"
  "40c0e0feb77392e3eb1f6ab6136cc116aeeded6030eea7db6688901df5ed021d"
  1 18)
 ("module/hnh/util/atomic-stack.scm"
  "147b45d2216c378c35d5c3ed0228be393b6c287f2a5515802928040f2087378e"
  1 13 29)
 ("module/hnh/util/type.scm"
  "f670542b9b404125224fd4c702be99e2c1c3fd55d862b18228e8772264ef3189"
  1                                      ; Module declaration
  12 34 44 46                            ; Macros
  53)                                    ; false? == not
 ;; File is not tested at all, since it's mearly a "header" file.
 ;; TODO possibly actually test extension loading, since it should
 ;; fail gracefully when library is not available.
 ("module/graphviz.scm"
  "5ff20a55098973fcc8552cd897e779eaf01dc3d4909f8d23be47d317987d8e95"
  18 82 84)
 ("module/base64.scm"
  "4614855f6cfedc20041e7094989e817f2c2c5ef85fb5f8322d915101c0aab53c"
  1 ; module declaration
  23 24 25 26 27 29 ; internal lookup table, used by everything so tested implicitly
  53 54 55 56 ; internal helper function, tested implictly
 )
 ("module/text/util.scm"
  "271a5f7740aa6e378e7fda2da4725171dc50a2e4a790e9529fceed19a747e775"
  5                                     ; (Module declaration
  52                                    ; else "keyword" apparently missed
  )
 ("module/vcomponent/duration.scm"
  "c83a2750964c2362af5043f33b435a792f62007d847f543e78a8f2550757e010"
  ;; Unreachable code, but fail faster
  113 114 115
  118 119 120 121
  )
 ("module/calp.scm"
  "873a191bc7122e59e3d60fb0f075dfa73fd8cc5ae0f3cb51932b24a0497ceeb1"
  1 ; Modudule declaration
  5 ; Version number (global variables are missed by the coverage)
  )
 ("module/crypto.scm"
  "9e157f5b53e923e7925b0e53118a4789b55712120427b73c4c3c9561e2c48718"
  1 ; Module declaration
  8 ; dynamic link
  10 11 12 13 14 ; constants
  16 17 18 19 ; primitive sha256 procedure
  )
 ("module/datetime.scm"
  "67eb46283a4097e8400322ab6434518a2455ca630e163238b5839c2bf25c9ac7"
  1
  159 160 161 162 163 164 165 ; week day declarations
  ;; function declarations
  189 223 238 389 909
  965 966 974
  987 992 1011 1025
  1231
  1239 1280
  923 924 925 ; read-hash-extend
  ;; aliases
  958 959 960
  1029 1031 1032 1034 1036 1037 1039 1041 1042 1044 1046 1047
  1049 1051 1052 1054 1056 1057 1059 1061 1062 1064 1065 1067 1068
  ;; other
  204
  252 ; Would depend on local timezone
  491 ; somehow not counted
  )

 ("module/datetime/zic.scm"
  "2a8ac0fae3c88227b05a5978bff3e114745ea146e07a2751df67d16c1e8218f5"
  13 ; module declaration
  66 ; syntax-rules
  171 223 ; function declaration
  )

 ("module/datetime/timespec.scm"
  "9feb7a7a09d9942d72c6b14b9f230e7711a73ca518ec2dc209775354203d856b"
  6 ; module declaration
  )

 ("module/hnh/util.scm"
  "3f0bf90a45d6eecce1248b7509e1b050e5cadbe92b279fe5ef082c18baf3e6ca"
  1
  72 73 74 ; conditional import
  77 ; syntax rules not covered:w
  289 ; != definition
  333 488 568 ; syntax rules
  )

 ("module/srfi/srfi-41/util.scm"
  "adb832b17f7ffe7c070fa3845f65283e850b14a07499d22715e16111f59ad88e"
  1)


 ;; this file simply exports other things. There's nothing to test
 ("module/vcomponent.scm"
  "b1c58b3beb6f170d3c9f7d603b27231ccf696897736113095b446f437721a9e1"
  2)


;;; Vendored files, and therefore shouldn't be tested

 ("module/srfi/srfi-64/test-error.scm"
  "15a0eb700de629a9e79aec8a1fde113fbc9542d052163ede46b433d630b7b01c"
  2)

 ("module/sxml/html.scm"
  "b4ffca46c9c723f6828e32d8798f1bbc89c2bfcb6f1368906b2d4bdef11951db"
  2)
)
