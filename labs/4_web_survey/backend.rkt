#lang racket/base
(require 
  web-server/http/xexpr
  web-server/http/request-structs ; for bindings
  web-server/servlet/web ; for send/suspend
)
(provide
  display-question-get-number
  display-item-html
  display-question-get-string
) 

(define (display-question-get-number prompt)
  (let 
    ([req 
      (send/suspend
        (lambda (where-to)
          (redirect/get)
          (response/xexpr
            `(html
              (style ((type "text/css")) "body { white-space: pre-wrap }")
              (body
                (p ,prompt)
                (form ([action ,where-to]
                      [method "POST"])
                  (input ([type "text"]
                          [name "answer"])
                  )
                )
              )
            )
          )
        ) 
      )
    ])
    (let 
      ([num 
        (string->number
          (bytes->string/utf-8
            (binding:form-value
              (bindings-assq
                #"answer"
                (request-bindings/raw req)
              )
            )
          )
        )
      ])
      (if num
        num
        (display-question-get-number prompt)
      )
    )
  )
)

(define (display-question-get-string prompt)
  (let 
    ([req 
      (send/suspend
        (lambda (where-to)
          (redirect/get)
          (response/xexpr
            `(html
              (style ((type "text/css")) "body { white-space: pre-wrap }")
              (body
                (p ,prompt)
                (form ([action ,where-to]
                      [method "POST"])
                  (input ([type "text"]
                          [name "answer"])
                  )
                )
              )
            )
          )
        ) 
      )
    ])
    (let 
      ([str 
        (bytes->string/utf-8
          (binding:form-value
            (bindings-assq
              #"answer"
              (request-bindings/raw req)
            )
          )
        )
      ])
      (if str
        str
        (display-question-get-string prompt)
      )
    )
  )
)

(define (display-item-html prompt)
  (let 
    ([req 
      (send/suspend
        (lambda (where-to)
          (redirect/get)
          (response/xexpr
            `(html
              (style ((type "text/css")) "body { white-space: pre-wrap }")
              (body
                (p ,prompt)
              )
            )
          )
        ) 
      )
    ])
    (redirect/get)
    req
  )
)