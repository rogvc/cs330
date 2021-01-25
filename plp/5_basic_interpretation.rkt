#lang plait

(define-type AE
  [numE   (n   : Number)]
  [plusE  (lhs : AE) (rhs : AE)]
  [minusE (lhs : AE) (rhs : AE)])

; to implement:
;   interp

(interp : (AE -> Number))
(define (interp ae)
    (type-case AE ae
        [(numE n)
            n
        ]
        [(plusE lhs rhs)
            (+ (interp lhs) (interp rhs))
        ]
        [(minusE lhs rhs)
            (- (interp lhs) (interp rhs))
        ]
    )
)

(test (interp (numE 3)) 3)
(test (interp (plusE (numE 1) (numE 2))) 3)
(test (interp (minusE (numE 2) (numE 1))) 1)
(test (interp (plusE (plusE (numE 1) (numE 1)) (numE 2))) 4)

; functional programming practice

; to implement (in any order)
;   keep-nums-between-5-and-95
;   keep-nums-in-range
;   keep-accepted-nums
;   keep-accepted
;(check-range (Number Number Number -> Boolean))
(define (check-range n lower upper)
    (cond
        [(> n upper) #f]
        [(> lower n) #f]
        [else #t]
    )
)

;(keep-nums-in-range (Number Number (Listof Number) -> (Listof Number)))
(define (keep-nums-in-range m o ns)
    (type-case (Listof Number) ns
        [empty
            empty
        ]
        [(cons element others)
            (if (check-range element m o)
                (cons element (keep-nums-in-range m o others))
                (keep-nums-in-range m o others)
            )
        ]
    )
)

(test (keep-nums-in-range 1 10 (list 0 1 5 7 10 11 12)) (list 1 5 7 10))

;(keep-nums-between-5-and-95 ((Listof Number) -> (Listof Number)))
(define (keep-nums-between-5-and-95 ns)
    (keep-nums-in-range 5 95 ns)
)

(test (keep-nums-between-5-and-95 (list 0 1 3 5 7 10 35 95 100 101)) (list 5 7 10 35 95))

;(keep-accepted : (('a -> Boolean) -> (Listof 'a)))
(define (keep-accepted f xs)
    (type-case (Listof 'a) xs
        [empty
            empty
        ]
        [(cons element others)
            (if (f element)
                (cons element (keep-accepted f others))
                (keep-accepted f others)
            )
        ]
    )
)

;(keep-accepted-nums : ('a -> Boolean) -> (Listof Number))
(define (keep-accepted-nums f ns)
    (keep-accepted f ns)
)