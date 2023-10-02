(define-module (test timespec)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (datetime timespec))

(test-equal "The empty string parses to the empty timespec"
  (timespec-zero) (parse-time-spec ""))

(test-group "timespec-add"

            (test-equal "Zero operands gives 0"
              (timespec-zero) (timespec-add))

            (let ((ts (make-timespec (time hour: 10 minute: 20 second: 30) '- #\z)))
              (test-equal "Single operand gives that operand"
                ts (timespec-add ts)))

            (test-equal "0 + 0 = 0"
              (timespec-zero) (timespec-add (timespec-zero) (timespec-zero)))

            (test-group
             "+ -"
             (test-equal "Remove a number less than the base"
               (make-timespec (time hour: 10 minute: 00 second: 00) '+ #\w)
               (timespec-add (make-timespec (time hour: 10 minute: 20 second: 30)
                                            '+ #\w)
                             (make-timespec (time minute: 20 second: 30)
                                            '- #\w)))

             (test-equal "Remove a number greater than the base"
               (make-timespec (time hour: 01 minute: 00 second: 00) '- #\w)
               (timespec-add (make-timespec (time hour: 10 minute: 00 second: 00) '+ #\w)
                             (make-timespec (time hour: 11 minute: 00 second: 00) '- #\w)))

             (test-equal "x + -x = 0"
               (timespec-zero) (timespec-add (make-timespec (time hour: 10 minute: 20 second: 30) '+ #\w)
                                             (make-timespec (time hour: 10 minute: 20 second: 30) '- #\w))))

            (test-group "- +"
             (test-equal "Add a number less than the (negative) base"
               (make-timespec (time hour: 10 minute: 00 second: 00) '+ #\w)
               (timespec-add (make-timespec (time hour: 10 minute: 20 second: 30) '- #\w)
                             (make-timespec (time hour: 00 minute: 20 second: 30) '+ #\w)))

             (test-equal "Add a number greater than the (negative) base"
               (make-timespec (time hour: 01 minute: 00 second: 00) '- #\w)
               (timespec-add (make-timespec (time hour: 10 minute: 00 second: 00) '- #\w)
                             (make-timespec (time hour: 11 minute: 00 second: 00) '+ #\w)))

             (test-equal "-x + x = 0"
               (timespec-zero) (timespec-add (make-timespec (time hour: 10 minute: 20 second: 30) '- #\w)
                                             (make-timespec (time hour: 10 minute: 20 second: 30) '+ #\w))))

            (test-group "+ +"
             (test-equal "x + x = 2x"
               (make-timespec (time hour: 20 minute: 41 second: 00) '+ #\w)
               (timespec-add (make-timespec (time hour: 10 minute: 20 second: 30) '+ #\w)
                             (make-timespec (time hour: 10 minute: 20 second: 30) '+ #\w))))

            (test-group "- -"
             (test-equal "-x + -x = -2x"
               (make-timespec (time hour: 20 minute: 41 second: 00) '- #\w)
               (timespec-add (make-timespec (time hour: 10 minute: 20 second: 30) '- #\w)
                             (make-timespec (time hour: 10 minute: 20 second: 30) '- #\w))))

            ;; add more than two timespecs

            ;; add timespecs of differing types
            )

(test-group "parse-time-spec"
            ;; TODO what even is this case?
            (test-equal (make-timespec (time) '+ #\g) (parse-time-spec "-g"))

            (test-equal "Parse direct date, with hour minute and second"
              (make-timespec (time hour: 20 minute: 00 second: 00) '+ #\w)
              (parse-time-spec "20:00:00"))
            (test-equal "Parse direct date, with hour and minute"
              (make-timespec (time hour: 20 minute: 00 second: 00) '+ #\w)
              (parse-time-spec "20:00"))
            (test-equal "Parse direct date, with just hour"
              (make-timespec (time hour: 20 minute: 00 second: 00) '+ #\w)
              (parse-time-spec "20"))

            (test-equal "Parse timespec with letter at end"
              (make-timespec (time hour: 20 minute: 00 second: 00) '+ #\g)
              (parse-time-spec "20:00g"))

            (test-equal "Parse negative timespec"
              (make-timespec (time hour: 20 minute: 00 second: 00) '- #\w)
              (parse-time-spec "-20"))

            (test-equal "Parse negative timespec with letter at end"
              (make-timespec (time hour: 20 minute: 00 second: 00) '- #\z)
              (parse-time-spec "-20z")))

'((datetime timespec))
