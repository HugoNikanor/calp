(define-module (test timespec)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (datetime core)
  :use-module (datetime timespec))

(test-group "timespec+"
  (test-equal "Zero operands gives 0"
    (timespec (time)) (timespec+))

  (let ((ts (timespec (time hour: 10 minute: 20 second: 30) '- 'utc)))
    (test-equal "Single operand gives that operand"
      ts (timespec+ ts)))

  (test-equal "0 + 0 = 0"
    (timespec (time))
    (timespec+ (timespec (time)) (timespec (time))))

  (test-group
      "+ -"
    (test-equal "Remove a number less than the base"
      (timespec (time hour: 10 minute: 00 second: 00) '+ 'wall)
      (timespec+ (timespec (time hour: 10 minute: 20 second: 30)
                              '+ 'wall)
                    (timespec (time minute: 20 second: 30)
                              '- 'wall)))

    (test-equal "Remove a number greater than the base"
      (timespec (time hour: 01 minute: 00 second: 00) '- 'wall)
      (timespec+ (timespec (time hour: 10 minute: 00 second: 00) '+ 'wall)
                    (timespec (time hour: 11 minute: 00 second: 00) '- 'wall)))

    (test-equal "x + -x = 0"
      (timespec-type (timespec (time)) 'wall)
      (timespec+ (timespec (time hour: 10 minute: 20 second: 30) '+ 'wall)
                 (timespec (time hour: 10 minute: 20 second: 30) '- 'wall))))

  (test-group "- +"
    (test-equal "Add a number less than the (negative) base"
      (timespec (time hour: 10 minute: 00 second: 00) '- 'wall)
      (timespec+ (timespec (time hour: 10 minute: 20 second: 30) '- 'wall)
                 (timespec (time hour: 00 minute: 20 second: 30) '+ 'wall)))

    (test-equal "Add a number greater than the (negative) base"
      (timespec (time hour: 01 minute: 00 second: 00) '+ 'wall)
      (timespec+ (timespec (time hour: 10 minute: 00 second: 00) '- 'wall)
                 (timespec (time hour: 11 minute: 00 second: 00) '+ 'wall)))

    (test-equal "-x + x = 0"
      (timespec-type (timespec (time)) 'wall)
      (timespec+ (timespec (time hour: 10 minute: 20 second: 30) '- 'wall)
                 (timespec (time hour: 10 minute: 20 second: 30) '+ 'wall))))

  (test-group "+ +"
    (test-equal "x + x = 2x"
      (timespec (time hour: 20 minute: 41 second: 00) '+ 'wall)
      (timespec+ (timespec (time hour: 10 minute: 20 second: 30) '+ 'wall)
                 (timespec (time hour: 10 minute: 20 second: 30) '+ 'wall))))

  (test-group "- -"
    (test-equal "-x + -x = -2x"
      (timespec (time hour: 20 minute: 41 second: 00) '- 'wall)
      (timespec+ (timespec (time hour: 10 minute: 20 second: 30) '- 'wall)
                 (timespec (time hour: 10 minute: 20 second: 30) '- 'wall))))

  (test-equal "Timespec add over multiple days"
    (timespec (time hour: 60 minute: 20 second: 30) '+ 'wall)
    (timespec+ (timespec (time hour: 10 minute: 20 second: 30) '+ 'wall)
               (timespec (time hour: 50) '+ 'wall)))

  (test-equal "Timespec subtract over multiple days"
    (timespec (time hour: 9 minute: 40) '- 'wall)
    (timespec+ (timespec (time hour: 10 minute: 20 second: 30) '+ 'wall)
               (timespec (time hour: 20 second: 30) '- 'wall)))

  (test-equal "Timespec going first positive, then negative"
    (timespec (time hour: 270) '- 'wall)
    (timespec+ (timespec (time hour: 1) '+ 'wall)
               (timespec (time hour: 29) '+ 'wall)
               (timespec (time hour: 300) '- 'wall)))

  ;; add timespecs of differing types
  )

(test-group "parse-time-spec"
  ;; TODO what even is this case?
  ;; (test-equal (timespec (time) '+ 'utc) (parse-time-spec "-g"))

  (test-equal "Parse direct date, with hour minute and second"
    (timespec (time hour: 20 minute: 00 second: 00) '+ #f)
    (parse-time-spec "20:00:00"))
  (test-equal "Parse direct date, with hour and minute"
    (timespec (time hour: 20 minute: 00 second: 00) '+ #f)
    (parse-time-spec "20:00"))
  (test-equal "Parse direct date, with just hour"
    (timespec (time hour: 20 minute: 00 second: 00) '+ #f)
    (parse-time-spec "20"))

  (test-equal "Parse timespec with letter at end"
    (timespec (time hour: 20 minute: 00 second: 00) '+ 'utc)
    (parse-time-spec "20:00g"))

  (test-equal "Parse negative timespec"
    (timespec (time hour: 20 minute: 00 second: 00) '- #f)
    (parse-time-spec "-20"))

  (test-equal "Parse negative timespec with letter at end"
    (timespec (time hour: 20 minute: 00 second: 00) '- 'utc)
    (parse-time-spec "-20z")))

'((datetime timespec))
