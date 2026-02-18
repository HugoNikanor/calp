;;; Commentary:
;;; This is a read-only data store, which reads the result of a REPORT query for calendar status.
;;; It only exists to import the example database from RFC 4791
;;; Code:

(define-module (vcomponent data-stores webdav-report-xml)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module (calp namespaces)
  :use-module (hnh util)
  :use-module (oop goops)
  :use-module (vcomponent data-stores common)
  :use-module (vcomponent media-type)
  :use-module (web uri)
  :export (create-instance))

(define-class <webdav-report-xml-store> (<calendar-data-store>)
  (stuff init-keyword: stuff: getter: stuff))

(define* (create-instance key: path)
  (define doc
    (call-with-input-file path xml->namespaced-sxml))

  (define root (xml-document-root doc))

  ;; TODO ensure root is {DAV:}multistatus

  (make <webdav-report-xml-store>
    stuff: (for response in
                (filter (lambda (tag) (tag-matches? tag 'response webdav))
                        (xml-element-children root))

                (let* ((href
                        (->> response xml-element-children
                             (find-child ((xml webdav 'href)))
                             xml-text-content
                             string->uri-reference))
                       (prop
                        (->> response xml-element-children
                             (find-child ((xml webdav 'propstat))) xml-element-children
                             (find-child ((xml webdav 'prop))) xml-element-children))
                       (data (->> prop (find-child ((xml caldav 'calendar-data))) xml-text-content))
                       (etag (->> prop (find-child ((xml webdav 'etag))) xml-text-content)))

                  ;; TODO store etag?

                  ;; NOTE we ignore everything in the href except the final component.
                  ;; This since we MUST only have a single URI path component here.
                  ;; The alternative would be a much more complex scheme where we returned
                  ;; a tree of stores, mapping out the URI tree given.
                  ;; E.g. the url "http://cal.example.com/bernard/work/abcd1.ics"
                  ;; would return the tree
                  ;; ("cal.example.com" ("bernard" ("work" . <webdav-report-xml-store>)))
                  ;; with abcd1.ics as the entry of the store
                  (cons (uri-path-last href)
                        (call-with-input-string data
                          (parser (@ (vcomponent media-type text calendar) format))))))))

(define-method (list-entries (store <webdav-report-xml-store>))
  (stuff store))

;;; NOTE: this procedure appears in mulitple places in the code base.
;;; It is NOT moved to a module, since it's a band-aid. All overly-specified hrefs
;;; MUST be validated before used (e.g. that all the "upper" components also point here),
;;; which this procedure plainly ignored
(define (uri-path-last href)
  (last (string-split (uri-path href) #\/)))
