(define-module (hnh util coverage)
  :use-module ((hnh util) :select (group-by))
  :use-module (hnh util type)
  :use-module (hnh util lens)
  :use-module (hnh util object)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-88)
  :use-module (ice-9 match)
  :use-module ((ice-9 regex) :select (string-match match:substring))
  :export (coverage-info
           coverage-info?
           filename lines total-lines hit-lines
           output-coverage
           parse-coverage
           merge-coverage))


(define-type (coverage-info)
  (filename type: string? default: "NO_FILE")
  (lines type: (list-of (pair-of integer? integer?))
         default: '())
  (total-lines type: integer? default: 0)
  (hit-lines type: integer? default: 0))

(define* (output-coverage coverage optional: (port (current-output-port)))
  (typecheck coverage coverage-info?)
  (format port "SF:~a~%" (filename coverage))
  (for-each (lambda (line)
              (format port "DA:~a,~a~%" (car line) (cdr line)))
            (lines coverage))
  (format port "LH: ~a~%" (hit-lines coverage))
  (format port "LF: ~a~%" (total-lines coverage))
  (format port "end_of_record~%"))

;;; Parses a single line from a coverage file
(define (parse-coverage-line line)
  (cond ((string-match "^DA:([0-9]+),([0-9]+)$" line)
         => (lambda (m)
              (let ((line (string->number (match:substring m 1)))
                    (hits (string->number (match:substring m 2))))
                (list 'DA line hits))))
        ((string-match "^SF:(.*)$" line)
         => (lambda (m)
              (let ((source-file (match:substring m 1)))
                (list 'SF source-file))))
        ((string-match "^LH: ([0-9]+)$" line)
         => (lambda (m)
              (let ((hit (string->number (match:substring m 1))))
                (list 'LH hit))))
        ((string-match "LF: ([0-9]+)$" line)
         => (lambda (m)
              (let ((lines (string->number (match:substring m 1))))
                (list 'LF lines))))
        ((string-match "^end_of_record$" line)
         => (lambda (m) '(end-of-record)))
        (else (scm-error 'no-match "parse-coverage-line"
                         "Got unknown when parsing coverage data: ~s"
                         (list line)
                         #f))))

(define (parse-coverage string)
  (cdr
   (fold (lambda (line state)
           (match (parse-coverage-line line)
             (('DA line hits)
              (modify state (compose-lens car* lines)
                      (lambda (lines) (cons (cons line hits) lines))))
             (('SF source)
              (set state car* filename source))
             (('LH hit)
              (set state car* hit-lines hit))
             (('LF lines)
              (set state car* total-lines lines))
             (('end-of-record)
              (cons (coverage-info) state))))
         (list (coverage-info))
         ;; First line is simply "TN:", discard it
         (remove string-null? (cdr (string-split string #\newline))))))


(define (merge-coverage a b)
  (typecheck a coverage-info?)
  (typecheck b coverage-info?)
  (unless (string=? (filename a) (filename b))
    (scm-error 'misc-error "merge-coverage"
               "Can only merge coverage data for the same file, got ~s and ~s"
               (list (filename a) (filename b))
               #f))
  (unless (= (total-lines a) (total-lines b))
    (scm-error 'misc-error "merge-coverage"
               "Mismatch between found lines. Is it really the same file? File: ~s, got ~s and ~s"
               (list (filename a) (total-lines a) (total-lines b))
               #f))

  (define merged-lines
    (map (lambda (group)
           (cons (car group)
                 (apply + (map cdr (cdr group)))))
         (group-by car (append (lines a) (lines b)))))

  (coverage-info
   filename: (filename a)
   lines: merged-lines
   total-lines: (length merged-lines)
   hit-lines: (length (remove (compose zero? cdr)
                              merged-lines))))
