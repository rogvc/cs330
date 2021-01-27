#lang plait
(print-only-errors #t)

; desugaring

(define-type SurfaceAE
  [numS   (n   : Number)]
  [plusS  (lhs : SurfaceAE) (rhs : SurfaceAE)]
  [minusS (lhs : SurfaceAE) (rhs : SurfaceAE)]
  [multS (lhs : SurfaceAE) (rhs : SurfaceAE)])

(define-type CoreAE
  [numC   (n   : Number)]
  [plusC  (lhs : CoreAE) (rhs : CoreAE)]
  [multC (lhs : CoreAE) (rhs : CoreAE)])

; to implement:
;   desugar

(desugar : (SurfaceAE -> CoreAE))
(define (desugar sae)
  (type-case SurfaceAE sae
    [(numS n)
      (numC n)
    ]
    [(plusS lhs rhs)
      (plusC (desugar lhs) (desugar rhs))
    ]
    [(minusS lhs rhs)
      (plusC (desugar lhs) (multC (numC -1) (desugar rhs)))
    ]
    [(multS lhs rhs)
      (multC (desugar lhs) (desugar rhs))
    ]
  )
)

(interpret : (CoreAE -> Number))
(define (interpret sae)
  (type-case CoreAE sae
    [(numC n)
      n
    ]
    [(plusC lhs rhs)
      (+ (interpret lhs) (interpret rhs))
    ]
    [(multC lhs rhs)
      (* (interpret lhs) (interpret rhs))
    ]
  )
)

; functional programming practice

; to implement (in any order)
;   list-sum
;   list-product
;   list-calc
;   list-compute

(list-sum : ((Listof Number) -> Number))
(define (list-sum ns)
  (type-case (Listof Number) ns
    [empty
      0
    ]
    [(cons el els)
      (+ el (list-sum els))
    ]
  )
)
(test (list-sum '(1 2)) 3)
(test (list-sum '(3 5 10)) 18)
(test (list-sum '(3 5 10 -18)) 0)
(test (list-sum '(-3 -5 -10)) -18)

(list-product : ((Listof Number) -> Number))
(define (list-product ns)
  (type-case (Listof Number) ns
    [empty
      1
    ]
    [(cons el els)
      (* el (list-product els))
    ]
  )
)
(test (list-product '(1 2)) 2)
(test (list-product '(3 -5)) -15)

(list-compute : (('a 'b -> 'b) 'b (Listof 'a) -> 'b))
(define (list-compute op unit ns)
  (type-case (Listof 'b) ns
    [empty
      unit
    ]
    [(cons el els)
      (op el (list-calc op unit els))
    ]
  )
)

(list-calc : ((Number Number -> Number) Number (Listof Number) -> Number))
(define (list-calc op unit ns)
  (list-compute op unit ns)
)
(test (list-calc + 0 (list 1 2)) (list-sum (list 1 2)))
(test (list-calc * 1 (list 1 2)) (list-product (list 1 2)))