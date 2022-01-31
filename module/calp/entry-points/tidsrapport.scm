;;; Commentary:
;;; pdftk <filename.pdf> dump_data_fields to get field names (including checkbox values)
;;; Sample template file:
;; ((Text1 förnamn)
;;  (Text2 efternamn)
;;  (Text3 personnummer)
;;  (Text4 address)
;;  (Text5 postnummer)
;;  (Text6 postort)
;;  (Text7 medborgarskap)
;;  (Text8 telefonnummer)
;;  (Text9 mailaddress)
;;  (Text115 institution-enhet-kurs-program)
;;  (Text176 namnförtydligande)
;;  ("Check Box15" har-swedbank)

;;  (groups
;;    (group
;;      (prefix Text)
;;      (summary 16)
;;      (days (- 17 47))
;;      (sum 48))
;;    (group
;;      (prefix Text)
;;      (summary 49)
;;      (days (- 50 80))
;;      (sum 81))
;;    (group
;;      (prefix Text)
;;      (summary 82)
;;      (days (- 83 113))
;;      (sum 114))))

;;; Sample from data file
;; ((förnamn "Hugo"))

;;; Code:


(define-module (calp entry-points tidsrapport)
  :export (main)
  :use-module (hnh util)
  :use-module (hnh util options)
  :use-module (ice-9 getopt-long)
  :use-module (datetime)
  )

(use-modules (srfi srfi-41)
             (srfi srfi-41 util)
             (srfi srfi-1)
             (vcomponent)
             (datetime)
             (vcomponent util instance)
             (vcomponent util instance methods)
             (hnh util)
             (ice-9 regex)
             (ice-9 popen)
             )


(define event-set
  (get-event-set global-event-object))

(define (get-worked-hours summary-search month year)

  (define instances
   (group-by (compose day as-date (extract 'DTSTART))
             (stream->list
              ((@ (vcomponent util search) execute-query)
               (lambda (e)
                 (define d (as-datetime (prop e 'DTSTART)))
                 (define s (date year: year month: month day: 1))

                 (and (string=? summary-search (prop e 'SUMMARY))
                      (datetime<=? (datetime date: s) d)
                      (datetime<=? d (datetime date: (date+ s (date month: 1))))))
               event-set
               ))))

  (define by-day (make-vector 31 0))

  (define (exactify n)
    (if (= n (round n))
        (inexact->exact n)
        n))

  (for-each (lambda (group)
              (define day (car group))
              (vector-set! by-day day
                           (exactify
                            (apply +
                                   (map (lambda (e)
                                          (time->decimal-hour
                                           (as-time
                                            (datetime-difference (prop e 'DTEND)
                                                                 (prop e 'DTSTART)))))
                                        (cadr group))))))
            instances)



  (vector->list by-day))

(define (build-alist work-hours fields)
  (filter-map
    (lambda (f n)
      (if (= 0 n)
        #f
        (list (string->symbol f)
              n)))
    fields
    work-hours))



(define (fill-from-alist template-list data-list)
  (filter-map (lambda (pair)
                (cond ((assoc-ref data-list (cadr pair))
                       => (lambda (it) (cons (car pair) it)))
                      (else #f)))
              template-list))



;; 
;;        [ 1, 31]
;; Text16 [17, 47] Text48
;; Text49 [50, 80] Text81
;; Text82 [83, 113] Text114
;; 
(define (format-field key value)
  (format #f "~%<<~%/T (~a)~%/V (~a)~%>>"
          key value))

(define prefix-string
  "%FDF-1.2
%âãÏÓ
1 0 obj

<<
/FDF 
<<
/Fields [")

(define post-string
  "]
>>
>>
endobj

trailer

<<
/Root 1 0 R
>>
%%EOF")


(define (generate-fdf report)
  (string-append prefix-string
                 (string-join (map (lambda (pair)
                                     (apply format-field pair))
                                   report))
                 post-string)
)

(define opt-spec
  '((pdf (value #t)
         (description "Input pdf fill"))
    (output (single-char #\o) (value optional)
            (description "Output file"))

    (data (value optional)
          (description "Static data to fill fields with")
          )
    (template (value optional)
              (description "Map between real field names and human readable names." (br)
                           "If data is given, but not trans, then data is assumed to be in a correct format"))
    (search (value #t)
            (description
             "Search term for dynamic filling. Supports basic globbing"))))

(define (parse-search str)
  (cond [(string-match "\\{(.*)\\}" str)
         => (lambda (m)
              (map (lambda (option)
                     (string-replace str option
                                     (match:start m)
                                     (match:end m)))
                   (string-split (match:substring m 1) #\,)))]
        [else (list str)]))

(define (main args)
  (define opts (getopt-long args (getopt-opt opt-spec)))

  (define input-pdf (option-ref opts 'pdf #f))
  (define output-pdf (or (option-ref opts 'output #f)
                         (and input-pdf
                              (string-append (dirname input-pdf)
                                             "/" (basename input-pdf ".pdf")
                                             "-output.pdf"))))

  (define data (option-ref opts 'data #f))
  (define template
    (call-with-input-file
     (or (option-ref opts 'template #f)
         (error "Template required"))
     read))

  (define prepared-data

    (cond ((and template data)
           (fill-from-alist template
                            (call-with-input-file data read)))
          (data (call-with-input-file data read))
          (template '())
          (else '())))

  (define search (parse-search (option-ref opts 'search #f)))

  ;; month year
  (define rem (map string->number (option-ref opts '() '())))
  ;; TODO warn when length(search) > 3 (number of rows in pdf)

  (define auto-filled
    (concatenate
     (map (lambda (group search-term)
            (define prefix (->string (car (or (assoc-ref group 'prefix) (list (symbol))))))
            (define summary
              (string-append prefix (->string (car (assoc-ref group 'summary)))))
            (define sum (string-append prefix (->string (car (assoc-ref group 'sum)))))
            (define days
              (let ((days (assoc-ref group 'days)))
                (cond ((not (list? days))
                       (error "Needs list, not pair"))
                      ((null? days)
                       (error "Need more days"))
                      ((and (list? (car days)) (eqv? '- (caar days)))
                       (map (lambda (s) (string-append prefix (->string s)))
                            (iota (1+ (- (list-ref (car days) 2)
                                         (list-ref (car days) 1)))
                                  (list-ref (car days) 1))))
                      ;; TODO case where cadr is a list, instead of cdr is the list?
                      (else
                       (map (lambda (s) (string-append prefix (->string s)))
                            days)))))

            (define work-hours (apply get-worked-hours search-term rem))
            `((,summary ,(format #f "~a ~a" (locale-month (car rem)) search-term))
              ,@(build-alist work-hours days)
              (,sum ,(apply + work-hours))))
          (or (assoc-ref template 'groups)
              (error "Groups required in template"))
          search)))

  (define report
    (append
     prepared-data
     auto-filled))


  (if input-pdf
      (let ((port (open-pipe* OPEN_WRITE
                         "pdftk" input-pdf "fill_form" "-"
                         "output" output-pdf)))
        (set-port-encoding! port "ISO-8859-1")
        (display (generate-fdf report) port)
        (newline port)
        ;; (put-bytevector port (generate-fdf report))
        (close-pipe port))
      (display (generate-fdf report))))
