#lang plait #:lazy
(print-only-errors #t)

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

(nats : (Listof Number))
(define nats (cons 0 (map add1 nats)))

(non-skippable? : (Number Number -> Boolean))
(define (non-skippable? d n)
  (not (> (* 2 d) n))
)

(prime? : (Number -> Boolean))
(define (prime? n)
  (cond
    [(>= 1 n)
      #f
    ]
    [else
      (let ([n2 (take-while (lambda (d) (non-skippable? d n)) (rest (rest nats)))])
        (prime?-rec n n2)
      ) 
    ]
  )
)

(prime?-rec : (Number (Listof Number) -> Boolean))
(define (prime?-rec n ns)
  (type-case (Listof Number) ns
    [empty
      #t
    ]
    [(cons f r)
      (if (equal? (remainder n f) 0)
        #f
        (prime?-rec n r)
      )
    ]
  )
)

(primes : (Listof Number))
(define primes
  (filter prime? nats)
)