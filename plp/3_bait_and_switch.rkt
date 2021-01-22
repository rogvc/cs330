#lang plait

; 1. Average
; Prepare helper functions
(count-elements : ((Listof Number) -> Number))
(define (count-elements xs)
  (cond
    [(equal? empty xs) 0]
    [else (+ 1 (count-elements (rest xs)))]
  )
)

(sum-elements : ((Listof Number) -> Number))
(define (sum-elements xs)
  (cond
    [(equal? empty xs) 0]
    [else (+ (first xs) (sum-elements (rest xs)))]
  )
)

; Contract
(average : ((Listof Number) -> Number))
; Template + actual
(define (average xs)
  (cond
    [(equal? empty xs) 0]
    [else (/ (sum-elements xs) (count-elements xs))]
  )
)
; Tests
(test (average empty) 0)
(test (average (list 1 2)) 1.5)
(test (average (list 1 2 3)) 2)

; 2. Suffixes
; Contract
(suffixes : ((Listof 'a) -> (Listof (Listof 'a))))
; Template + actual
(define (suffixes xs)
  (cond
    [(equal? xs empty) (list (list))]
    [else (append (list xs) (suffixes (rest xs)))]
  )
)

(test (suffixes (list 1 2 3)) (list (list 1 2 3) (list 2 3) (list 3) (list)))

; 3. Find

; Contract
;(find : ((Listof 'a) predicate -> (Optionof 'a)))

; Template + actual
(define (find xs predicate)
  (cond
    [(equal? xs empty) (none)]
    [else 
      (if (predicate (first xs))
        (some (first xs))
        (find (rest xs) predicate)
      )
    ]
  )
)