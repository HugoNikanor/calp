(((srfi srfi-19 alt) date time)
 ((srfi srfi-19 alt util) month-stream)
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
