(((parameters) calendar-files)
 ((vcomponent load) load-calendars)
 )

(test-assert (load-calendars calendar-files: (calendar-files)))


