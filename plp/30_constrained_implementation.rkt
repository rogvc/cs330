#lang plait

(one-m : (Number -> Number))
(define (one-m m)
    10
)

(one-p : ('a -> 'a))
(define (one-p p)
    'b
)

(two-m : (Listof Number))
(define two-m 
    empty
)

(two-p : (Listof 'a))
(define two-p
    empty
)

(three-m : ((Listof Number) -> (Listof Number)))
(define (three-m ms)
    ms
)

(three-p : ((Listof 'a) -> (Listof 'a)))
(define (three-p ps)
    ps
)

(or (equal? (three-p (quote (1))) (quote (1))) (equal? (three-p (quote (1))) (quote ())))