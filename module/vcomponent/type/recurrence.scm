(define-module (vcomponent type recurrence)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (vcomponent type recurrence generate)
  :use-module (vcomponent type recurrence parse)
  :use-module (vcomponent type recurrence internal)
  :re-export (
              generate-recurrence-set
              repeating?

              recur-rule
              recur-rule?
              freq freq*
              until      until*
              count      count*
              interval   interval*
              bysecond   bysecond*
              byminute   byminute*
              byhour     byhour*
              byday      byday*
              bymonthday bymonthday*
              byyearday  byyearday*
              byweekno   byweekno*
              bymonth    bymonth*
              bysetpos   bysetpos*
              wkst       wkst*

              weekdays
              intervals
              weekday->symbol

              byday->string
              ))

