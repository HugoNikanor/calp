(((datetime) date time)
 ((datetime util) month-stream in-date-range?)
 ((srfi srfi-41) stream->list stream-take
  ))

(test-assert "jan->dec"
  (stream->list (stream-take 11 (month-stream #2020-01-01))))

(test-assert "dec->jan"
  (stream->list (stream-take 2 (month-stream #2020-12-01))))

(test-assert "dec->feb"
  (stream->list (stream-take 3 (month-stream #2020-12-01))))

(test-assert "20 months"
  (stream->list (stream-take 20 (month-stream #2020-01-01))))

(test-equal "Correct months"
  (list #2020-02-01 #2020-03-01 #2020-04-01 #2020-05-01 #2020-06-01 #2020-07-01 #2020-08-01 #2020-09-01 #2020-10-01 #2020-11-01 #2020-12-01 #2021-01-01)

  (stream->list (stream-take 12 (month-stream #2020-02-01))))

(test-assert "in-date-range?"
  (not ((in-date-range? #2020-01-01 #2020-02-29)
        #2018-02-02)))
