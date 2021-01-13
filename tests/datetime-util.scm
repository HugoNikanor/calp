;;; Commentary:
;; Tests timespan overlaps and month-streams.
;; Separate from tests/datetime.scm since 
;; (datetime util) originally was its own module.
;;; Code:

(((datetime) date time datetime
  month-stream in-date-range? timespan-overlaps?)
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




(test-assert "A"
    (timespan-overlaps? #2020-01-01 #2020-01-10
                        #2020-01-05 #2020-01-15))

(test-assert "A, shared start"
    (timespan-overlaps? #2020-01-01 #2020-01-10
                        #2020-01-01 #2020-01-15))

(test-assert "A, tangential"
  (not (timespan-overlaps? #2020-01-01T00:00:00 #2020-01-10T00:00:00
                           #2020-01-10T00:00:00 #2020-01-30T00:00:00)))



(test-assert "s1 instant"
  (timespan-overlaps? #2020-01-15T10:00:00 #2020-01-15T10:00:00
                      #2020-01-10T00:00:00 #2020-01-30T00:00:00))

(test-assert "s2 instant"
  (timespan-overlaps? #2020-01-10T00:00:00 #2020-01-30T00:00:00
                      #2020-01-15T10:00:00 #2020-01-15T10:00:00))

(test-assert "s1 instant, shared start with s2"
  (timespan-overlaps? #2020-01-15T10:00:00 #2020-01-15T10:00:00
                      #2020-01-15T10:00:00 #2020-01-30T00:00:00))


(test-assert "s1 instant, shared end with s2"
  (not (timespan-overlaps? #2020-01-15T10:00:00 #2020-01-15T10:00:00
                           #2020-01-10T00:00:00 #2020-01-15T10:00:00)))

(test-assert "s2 instant, shared start with s1"
  (timespan-overlaps? #2020-01-15T10:00:00 #2020-01-30T00:00:00
                      #2020-01-15T10:00:00 #2020-01-15T10:00:00))


(test-assert "s2 instant, shared end with s1"
  (not (timespan-overlaps? #2020-01-10T00:00:00 #2020-01-15T10:00:00
                           #2020-01-15T10:00:00 #2020-01-15T10:00:00)))


(test-assert "both instant"
  (not (timespan-overlaps? #2020-01-15T10:00:00 #2020-01-15T10:00:00
                           #2020-01-15T10:00:00 #2020-01-15T10:00:00)))

(test-assert "tangential whole day"
    (not (timespan-overlaps? #2020-01-01 #2020-01-02
                             #2020-01-02 #2020-01-03)))

(test-assert "B"
  (timespan-overlaps? #2020-01-05 #2020-01-15
                      #2020-01-01 #2020-01-10))


(test-assert "E"
  (timespan-overlaps? #2020-01-01 #2020-01-10
                      #2020-01-01 #2020-01-10))
