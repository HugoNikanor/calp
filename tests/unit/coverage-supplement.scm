;;; Guile's coverage system sometimes miss some definitions.
;;; Add these here so the output gets green.
;;; However, always start by attempting to add more tests to fill
;;; in the coverage.
;;;
;;; Each entry in this file should be a list consisting of:
;;; - The filename, relative calp's root
;;; - The sha256-sum of that file
;;; - Any number of lines which should be marked as covered.
(("module/vcomponent/base.scm"
  "f98a3887020c400595bcc32805f968ebebca685bc1c18ef1f1531f55d9f72ec1"
  73 83 108 1)
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
 )
