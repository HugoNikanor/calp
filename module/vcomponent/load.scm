(define-module (vcomponent load)
  :export (load-calendars load-calendars*)
  :use-module (util)
  :use-module (util config)
  :use-module ((vcomponent parse) :select (parse-cal-path)))

(define-config calendar-files '() ""
  pre: list?)

(define* (load-calendars calendar-files)
  (map parse-cal-path calendar-files))
