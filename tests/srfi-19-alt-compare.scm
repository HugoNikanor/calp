(((srfi srfi-19 alt)
  date
  date< date<=
  date> date>=
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
