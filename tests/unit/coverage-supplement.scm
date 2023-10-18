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
  23 29 ; internal lookup table, used by everything so tested implicitly
  53 ; internal helper function, tested implictly
 )
)
