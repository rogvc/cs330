#lang plait

; to implement
; - fact
; - fib


(fact : (Number -> Number))
(define (fact n)
    (fact-rec n 1)
)

(fact-rec : (Number Number -> Number))
(define (fact-rec n total)
    (cond
        [(= n 0)
            total
        ]
        [(< n 0)
            total
        ]
        [else
            (fact-rec (- n 1) (* total n))
        ]
    )
)

;;; (test (fact 4) 24)

(fib : (Number -> Number))
(define (fib n)
    (fib-rec n 0 1)
)

(fib-rec : (Number Number Number -> Number))
(define (fib-rec n a b)
    (cond
        [(= n 0)
            a
        ]
        [(= n 1)
            b
        ]
        [else
            (fib-rec (- n 1) b (+ a b))
        ]
    )
)

(test (fib 9) 34)