(vcomponent 
  #:type 'VCALENDAR
  #:properties
  (-> (table)
      (table-put 'VERSION (list (vline #:value (vcalendar-version #:max "2.0")))))
  #:children
  (list (vcomponent
          #:type 'VEVENT
          #:properties
          (-> (table)
              (table-put 'SUMMARY
                         (list (vline #:value "See parameter"
                                      #:params (-> (table)
                                                   (table-put 'X-PARAM
                                                              "Quoted; sure, thing!")))))))))
