#lang plait #:lazy
(print-only-errors #t)

; to implement
; - take-while
; - build-infinite-list

(take-while : (('a -> Boolean) (Listof 'a) -> (Listof 'a)))
(define (take-while pred xs)
  (type-case (Listof 'a) xs
    [empty
      empty
    ]
    [(cons f r)
      (if (pred f)
        (cons f (take-while pred r))
        empty
      )
    ]
  )
)

(test (take-while (lambda (n) (< n 5)) (list 1 2 3 4 5 1 2)) (list 1 2 3 4))

(define nats (cons 0 (map add1 nats)))

(build-infinite-list : ((Number -> 'a) -> (Listof 'a)))
(define (build-infinite-list f)
  (map f nats)
)