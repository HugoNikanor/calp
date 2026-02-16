(define-module (vcomponent data-stores vdir)
  :use-module (oop goops)
  :use-module (vcomponent)
  :use-module (vcomponent data-stores common)
  :use-module (vcomponent media-type)
  :use-module (vcomponent datetime)
  :use-module (vcomponent type recurrence)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util path)
  :use-module (hnh util io)
  :use-module (hnh util bimap)
  :use-module (hnh util color)
  :use-module (hnh util color parse)
  :use-module (calp util config)
  :use-module (ice-9 ftw)
  :use-module (ice-9 rdelim)
  :use-module ((ice-9 regex) :select (string-match))
  :use-module (glob)
  :use-module ((web uri) :select (build-uri uri->string))
  :use-module ((web query) :select (encode-query-parameters))
  :export (create-instance)
  )

;;; TODO inotify on the directory and all files, in case another program modifies it

;;; File in which href-to-filename mappings are stored inside the vdir
(define-config default-href-mapping-file ".calp-href"
  pre: (ensure string?))

(define-class <vdir-data-store> (<calendar-data-store>)
  (path getter: path
        init-keyword: path:
        init-value: #f)
  (data-format getter: data-format
               init-keyword: media:
               init-value: #f)
  (href-uid-map getter: href-uid-map
                init-form: (bimap))

  (event-by-href getter: event-by-href
                 init-form: (make-hash-table))

  (filename-by-href
   getter: filename-by-href
   init-form: (make-hash-table))

  ;; Filename used to store filename-by-href
  (href-mapping-file
   init-keyword: mapping-file:
   getter: href-mapping-file
   init-form: (default-href-mapping-file))

  )



;;; Mapping from href strings, to local filenames.
;;; File consists of
;;; - a single comment line, ending with #\newline
;;; - any number of records, where each record conists of
;;;   + an url encoded href
;;;   + #\us (any non url-safe character could have been used)
;;;   + a local filename
;;;   + #\nul (since it can't appear in filenames)
;;; This format was choosen since it perfectly encodes the data, is
;;; lightweight, and can be trivially appended if need be.
;;;
;;; Anything after the final #\nul is ignored, meaning that a trailing
;;; newline is permitted.
;;;
;;; Preview the file with something like
;;; $ tr '\0' '\n' < "$FILENAME" | tr '\037' '\t'


;;; Read the href->filename mapping file, and populate the provided
;;; hash table.
;;; If any error occurs during the operation, an error willl be
;;; logged, and all hrefs will be invalidated, by clearing the table.
(define (read-filename-by-href! ht port)
  (catch #t
    (lambda ()
      (read-line port)
      (let loop ()
        (let* ((href (read-delimited (string #\us) port))
               (filename (read-delimited (string #\nul) port)))
          ;; this silently discards trailing keys. This is likely to
          ;; happen if a a trailing newline was added somewhere along the line.
          (unless (eof-object? filename)
            (hash-set! ht href filename)
            (loop)))))
    (lambda args
      ;; TODO emit warning that read failed, and that all hrefs are now invalidated
      (hash-clear! ht))))

(define (write-href-mappings! mappings port)
  (with-output-to-port port
    (lambda ()
      ;; TODO specify this headers content as part of filename, and
      ;; distribute a libmagic declaration.
      ;;     $ cat ~/.magic
      ;;     0	string	href->filename	Calp Vdir href->filename mapping
      (display "href->filename mapping for calp (https://git.hornquist.se/calp)")
      (newline)

      (hash-for-each
       (lambda (href path) (display href) (display #\us) (display path) (display #\nul))
       mappings))))

;;; TODO append-href-mappings!
;;; - open file in append mode
;;; - write single record



;;; Temporary callback called for each file upon loading,
;;; intended to show a progress bar to the user
;;; TODO allow this to be user configurable
(define* (loading-callback key: idx total file)
  ;; (format (current-error-port) "~a/~a: ~a~%"
  ;;         idx total file)
  'noop
  )

(define-method (initialize (self <vdir-data-store>) args)
  (next-method)
  (typecheck (path self) string?)
  (typecheck (data-format self) calendar-data-format?)
  (typecheck (href-mapping-file self) string?)

  (catch 'system-error
    (lambda () (mkdir (path self)))
    (lambda (err proc fmt args data)
      ;; NOTE possibly log that a new store was created
      (unless (= EEXIST (car data))
        (throw err proc fmt args data))))

  (catch 'system-error
    (lambda ()
      (call-with-input-file (path-append (path self) (href-mapping-file self))
        (lambda (port) (read-filename-by-href! (filename-by-href self) port))))
    (lambda (err proc fmt args data)
      ;; NOTE possibly log that no href-mapping-file existed
      (unless (= ENOENT (car data))
        (throw err proc fmt args data))))

  (define entry-by-filename (make-hash-table))
  (define filenames (glob (glob-pattern self)))
  (define file-count (length filenames))
  (for (i . filename) in (enumerate filenames)
       ;; TODO if parsing fails, log an error, and continue with the rest of the elements.
       ;; This includes both broken files, but also files containing non-compliant components, such as
       ;; - file with no VEVENT
       ;; - file with multiple VEVENTs with different UIDs
       ;; - Missing UID
       ;; - ...
       (loading-callback idx: i total: file-count file: filename)
       (hash-set! entry-by-filename (basename filename)
                  (call-with-input-file filename
                    (parser (data-format self)))))

  ;; (format (current-error-port)
  ;;         "filename-by-href: ~s~%entry-by-filename: ~s~%"
  ;;         (hash-map->list cons (filename-by-href self))
  ;;         (hash-map->list (lambda (a _) a) entry-by-filename))

  ;; TODO TODO dangling-hrefs appears to be a list of filenames
  (let* ((dangling-hrefs referenced-files
                         (lset-diff+intersection
                          string=?
                          (hash-map->list (lambda (_ a) a) (filename-by-href self))
                          (hash-map->list (lambda (a _) a) entry-by-filename)))
         (unreferenced-files (lset-difference string=?
                                              ;; TODO is this the correct order
                                              (hash-map->list (lambda (a _) a) entry-by-filename)
                                              referenced-files)))

    ;; (format (current-error-port)
    ;;         "dangling: ~s~%unreferenced: ~s~%"
    ;;         dangling-hrefs unreferenced-files)

    (for-each (lambda (dangling) (hash-remove! (filename-by-href self) dangling))
              dangling-hrefs)

    (for-each (lambda (unreferenced)
                ;; All filenames are valid hrefs, so we just keep it as is
                (hash-set! (filename-by-href self)
                           unreferenced unreferenced))
              unreferenced-files))

  ;; Write updated mapping back to disk
  (write-href-mapping-file! self)

  ;; TODO warn on UID conflicts

  (hash-for-each (lambda (href filename)
                   (define entry (hash-ref entry-by-filename filename))
                   (hash-set! (event-by-href self) href entry)
                   (set-left! (href-uid-map self) href (prop1 entry 'UID)))
                 (filename-by-href self))

  ;; Note that we don't keep entry-by-filename for later, since all
  ;; its information is already contained in filename-by-href and
  ;; event-by-href.
  )

(define (glob-pattern store)
  (path-append (path store)
               (string-append
                "*." (or (file-extension (data-format store)) "ics"))))

(define* (create-instance key: path media
                          (href-mapping-file (default-href-mapping-file)))
  (typecheck path string?)
  (typecheck media string?)

  (make <vdir-data-store>
    path: path
    media: (resolve-media-type media)
    href-mapping-file: href-mapping-file))

(define-method (store-uri (store <vdir-data-store>))
  (build-uri 'store
             host: "vdir"
             path: (path store)
             query: (encode-query-parameters
                     `(
                       ;; Note that a media type query parameter is
                       ;; required to create a store, but the
                       ;; media-type field of a data format is
                       ;; optional, and may be a different value than
                       ;; expected. This is just a best effort.
                       ,@(cond ((media-type (data-format store))
                                => (lambda (t) `((media . ,t))))
                               (else '()))
                       ,@(unless (string=? (href-mapping-file store)
                                           (default-href-mapping-file))
                           `((href-mapping-file . ,(href-mapping-file store))))
                       ))))


;;; TODO this is really slow for some reason (~2s for 2000 entries)
;;; Only place it really could be slow is in hash-map->list, since
;;; everything should already be in memory.
(define-method (list-entries (store <vdir-data-store>))
  (hash-map->list cons (event-by-href store)))

(define-method (entry-count (store <vdir-data-store>))
  ;; This works, but is currently worthless, since we load all the
  ;; data in the constructor.
  (length (glob (glob-pattern store))))

;;; TODO get-by-uid
;;; TODO caldav-filter

(define-method (get-by-href (store <vdir-data-store>) href)
  (hash-ref (event-by-href store) href))

(define-method (put-event! (store <vdir-data-store>) href entry)
  (hash-set! (event-by-href store) href entry)

  (define filename
    (cond ((hash-ref (filename-by-href store) href)
           => identity)
          (else
           ;; TODO TODO if `f = <any string>`, and the store contains `$f`,
           ;; then a put on `$f.ics` causes a second href mapping to
           ;; be created for the file. This will lead to the entry
           ;; being effectively duplicated, since we list entries by
           ;; their hrefs.
           (define ext (or (file-extension (data-format store)) "ics"))
           (define filename
             (if (string-match (format #f "[.]~a$" ext)
                               href)
                 href
                 (string-append href "." ext)))
           ;; NOTE this is where append to the filename-by-href file could be useful
           (hash-set! (filename-by-href store) href filename)
           filename)))

  ;; TODO TODO update href-uid-map

  ;; TODO the Vdir standard technically only allows tempfiles with a
  ;; .tmp extension. However, the files created by
  ;; with-atomic-output-to-file follows the pattern
  ;; `.${randstr}-${randstr-without-periods}`, which is ignored by
  ;; vdir¹. Bigger problem is that this filename structure isn't part of
  ;; with-atomic-output-to-file's contract (as of 2025-11-04)
  ;; 
  ;; ¹ Technically, another program might read it as metadata
  (with-atomic-output-to-file (path-append (path store) filename)
    (lambda ()
      ((serializer (data-format store))
       entry
       (current-output-port)))))

(define-method (remove-by-href! (store <vdir-data-store>) href)
  ;; TODO TODO update href-uid-map
  (hash-remove! (event-by-href store) href)
  (delete-file (path-append (path store) (hash-ref (filename-by-href store) href)))
  (hash-remove! (filename-by-href store) href))

(define-method (write-href-mapping-file! (store <vdir-data-store>))
  ;; TODO make this atomic?
  (call-with-output-file (path-append (path store) (href-mapping-file store))
    (lambda (port)
      (write-href-mappings! (filename-by-href store) port))))

(define-method (flush! (store <vdir-data-store>))
  (write-href-mapping-file! store))

(define* (get-metadata path key key: dflt)
  (catch 'system-error
    (lambda () (call-with-input-file (path-append path key) read-line))
    (const dflt)))

(define (set-metadata! path key value)
  (call-with-output-file (path-append path key)
    (display value)))

(define (remove-metadata! path key)
  (delete-file (path-append path key)))

;;; TODO language
(define-method (store-displayname (store <vdir-data-store>))
  (get-metadata (path store) "displayname"))

(define-method (set-store-displayname! (store <vdir-data-store>) name)
  (set-metadata! (path store) "displayname" name))

(define-method (remove-store-displayname! (store <vdir-data-store>))
  (remove-metadata! (path store) "displayname"))

;;; TODO language
(define-method (store-description (store <vdir-data-store>))
  (get-metadata (path store) "description"))

(define-method (set-store-description! (store <vdir-data-store>) desc)
  (set-metadata! (path store) "description" desc))

(define-method (remove-store-description! (store <vdir-data-store>))
  (remove-metadata! (path store) "description"))

;;;
(define-method (store-color (store <vdir-data-store>))
  (and=> (get-metadata (path store) "color") parse-hex-rgb))

(define-method (set-store-color! (store <vdir-data-store>) color)
  (typecheck color color?)
  (set-metadata! (path store) "color"
                 (-> color ->rgb rgb->hex)))

(define-method (remove-strore-color! (store <vdir-data-store>))
  (remove-metadata! (path store) "color"))


(define-method (entries-in-interval (store <vdir-data-store>)
                                    reference-zone start end)
  ;; TODO log level debug
  (typecheck start zoned-datetime?)
  (typecheck end   zoned-datetime?)
  (format (current-error-port) "<DEBUG> entries-in-interval ~s, ~s - ~s~%"
          (uri->string (store-uri store)) start end)
  (define result
    (call-with-values
        (lambda ()
          (partition
           (compose recurring? cdr)
           (hash-map->list cons (event-by-href store))))
      (expand-and-interleave-recurrences reference-zone start end)))
  ;; TODO log level debug
  (format (current-error-port) "<DEBUG> Entries gotten ~s~%"
          (uri->string (store-uri store)))
  result)

