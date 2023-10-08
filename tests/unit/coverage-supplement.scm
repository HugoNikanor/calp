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
 )
