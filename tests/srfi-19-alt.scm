((srfi srfi-19 alt) date+ date- date)

(test-equal #2020-02-28 (date- #2020-03-05 (date day: 6)))
(test-equal #2020-02-29 (date- #2020-03-05 (date day: 5)))
(test-equal #2020-03-01 (date- #2020-03-05 (date day: 4)))
