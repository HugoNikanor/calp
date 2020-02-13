(((srfi srfi-19 alt)
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

(test-assert (not (date/-time< #2018-11-30T08:10:00 #2014-04-13T16:00:00)))
