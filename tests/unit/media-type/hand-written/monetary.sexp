;;; -*- mode: scheme -*-
(vcomponent 
  type: 'VCALENDAR
  properties: 
  (-> (table)
      (table-put 'PRICE (list (vline value: (monetary 
                                              currency: "USD"
                                              amount: 10.20))))))


