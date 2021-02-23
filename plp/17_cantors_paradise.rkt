#lang plait
(print-only-errors #t)

(define-type-alias Stream (Request -> Response))

(define-type Request
  [head-req]
  [tail-req])

(define-type Response
  [head-res (head : Number)]
  [tail-res (tail : Stream)])

; to implement
; - naturals

(define (build-stream n)  
  (lambda (r)
    (type-case Request r
      [(head-req)
        (head-res n)
      ]
      [(tail-req)
        (tail-res (build-stream (add1 n)))
      ]
    )
  )
)


(naturals : Stream)
(define naturals
  (build-stream 0)
)