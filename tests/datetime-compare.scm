(((datetime)
  date
  datetime time
  date< date<=
  date> date>=
  date/-time<
  time<
  ))

(test-assert "date< empty"
  (date<))

(test-assert "date< single"
  (date< #2020-01-10))

(test-assert "date< double"
  (date< #2020-01-10 #2020-01-11))

(test-assert "date< tripple"
  (date< #2020-01-10 #2020-01-11 #2020-01-12))

(test-assert "date< tripple negate"
  (not (date< #2020-01-10 #2020-01-12 #2020-01-11)))

(test-assert "date<= empty"
  (date<=))

(test-assert "date<= single"
  (date<= #2020-01-10))

(test-assert "date<= double"
  (date<= #2020-01-10 #2020-01-11))

(test-assert "date<="
  (not (date<= #2020-01-01 #2018-05-15 #2020-01-31)))

(test-assert "date<= equal"
  (date<= #2018-05-15 #2018-05-15))

(test-assert "date<"
  (not (date< #2020-01-01 #2018-05-15 #2020-01-31)))

(test-assert "date>"
  (not (date> #2020-01-31 #2018-05-15 #2020-01-01 )))

(test-assert "date>="
  (not (date>=  #2020-01-31  #2018-05-15 #2020-01-01)))

(test-assert "time< simple"
  (time< #05:00:00 #10:00:00))

(test-assert "time<"
  (time< (time) #10:00:00))

(test-assert "date/-time<"
  (date/-time< #2020-01-01 #2020-01-02))

(test-assert "not date/-time<"
  (not (date/-time< #2020-01-01 #2020-01-01)))

(test-assert "date/-time< only other dt"
  (date/-time< #2020-01-01 #2020-01-02T10:00:00))

(test-assert "date/-time< other dt, same date"
  (date/-time< #2020-01-01 #2020-01-01T10:00:00))

;; In UTC+2 (CEST) the below datetime overflows into midnight the following
;; day. Earlier versions of this program only looked at the time component
(test-assert "date/-time< TZ overflow"
  (date/-time< #2020-04-05
               (datetime date: #2020-04-05 time: #22:00:00 tz: "UTC")))

(test-assert "date/-time< time-only"
  (date/-time< #00:00:00 #10:00:00))

(test-assert (not (date/-time< #2018-11-30T08:10:00 #2014-04-13T16:00:00)))

