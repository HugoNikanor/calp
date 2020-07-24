(define-module (c lex)
  :use-module (ice-9 peg)
  :use-module (c operators)
  :export (lex))


;; Like the regular define-peg-pattern. But evaluates the
;; pattern before treating it as a peg rule.
(define-macro (define-define-peg-pattern name capture expr)
  `(define-peg-pattern ,name ,capture
     ;; NOTE how does this work if we are in a different module?
     ;; It currently however isn't a problem since we don't export
     ;; this macro.
     ,(eval expr (current-module))))



(define-peg-pattern base-8-digit body
  (range #\0 #\7))

(define-peg-pattern base-10-digit body
  (range #\0 #\9))

(define-peg-pattern base-16-digit body
  (or (range #\0 #\9)
      (range #\A #\F)
      (range #\a #\f)))

;; https://en.cppreference.com/w/cpp/language/integer_literal
(define-peg-pattern base-10 all (+ base-10-digit))
(define-peg-pattern base-8 all (and (ignore "0") (+ base-8-digit)))
(define-peg-pattern base-16 all (and (ignore (and "0" (or "x" "X")))
                                     (+ base-16-digit)))

;; accept anything now, ensure correctnes later
(define-peg-pattern integer-suffix all
  (* (or "u" "U" "l" "L")))

(define-peg-pattern integer all
  (and (or base-8 base-16 base-10) (? integer-suffix)))

(define-peg-pattern number body
  (or integer))

(define-peg-pattern group all
  (and (ignore "(") expr (ignore ")")))

(define-peg-pattern base-8-char all
  (and base-8-digit
       (? base-8-digit)
       (? base-8-digit)))

(define-peg-pattern base-16-char all
  (and (ignore "x") base-16-digit (? base-16-digit)))

(define-peg-pattern escaped-char all
  (and (ignore "\\") (or base-16-char
                         base-8-char
                         peg-any)))

(define-peg-pattern char all
  (and (ignore "'") (or escaped-char peg-any) (ignore "'")))

(define-define-peg-pattern operator all
  `(or ,@(map symbol->string symbol-binary-operators)
       ,@(map (lambda (op) `(and ,(symbol->string op) ws))
              wordy-binary-operators)))

;; whitespace
(define-peg-pattern ws none
  (or " " "	" "\n"))

;; space (for when whitespace is optional)
(define-peg-pattern sp none (* ws))

(define-peg-pattern safe-letter body
  (or "_"
      (range #\A #\Z)
      (range #\a #\z)))

(define-peg-pattern variable all
  (and safe-letter
       (* (or safe-letter
              base-10-digit))))

(define-peg-pattern prefix-operator all
  (or "!" "~" "*" "&" "++" "--" "+" "-"))

;;; Note that stacked pre or postfix operators without parenthesis
;;; dosen't work. So `*&C' is invalid, while `*(&C)' is valid.

(define-peg-pattern prefix all
  (and prefix-operator sp (or variable group funcall #; postfix
                              )))

(define-peg-pattern postfix-operator all
  (or "++" "--"))

(define-peg-pattern postfix all
  ;; literals can't be in-place incremented and decremented
  ;; Make sure we don't match postfix-operator here, since
  ;; that also gives us an infinite loop.
  (and (or prefix funcall group variable) sp postfix-operator))

(define-peg-pattern infix all
  ;; first case is "same" as expr, but in different order to prevent
  ;; infinite self reference. Pre and postfix not here, solved by having
  ;; them before infix in expr
  (and (or funcall postfix prefix group char number variable)
       sp operator sp expr))

(define-peg-pattern funcall all
  (and variable sp group))

;;; main parser
(define-peg-pattern expr body
  (+ (and sp (or infix postfix prefix funcall group char number variable)
          sp)))


(define (lex string)
  (peg:tree (match-pattern expr string)))
