#lang plait
(print-only-errors #t)

; desugaring

(define-type Expr
  [numE (n : Number)]
  [idE (x : Symbol)]
  [plusE (lhs : Expr) (rhs : Expr)]
  [withE (x : Symbol) (bound-exp : Expr) (body-exp : Expr)])

; to implement:
;   subst
(subst : (Expr Symbol Expr -> Expr))
(define (subst what for in)
  (type-case Expr in
    [(numE n)
      (numE n)
    ]
    [(idE x)
      (if (equal? x for)
        what
        (idE x)
      )
    ]
    [(plusE lhs rhs)
      (plusE (subst what for lhs) (subst what for rhs))
    ]
    [(withE x bound-exp body-exp)
      (if (equal? x for)
        (withE x (subst what for bound-exp) body-exp)
        (withE x (subst what for bound-exp) (subst what for body-exp))
      )
    ]
  )
)

(test (subst (numE 42) 'x (idE 'x)) (numE 42))
(test (subst (numE 2) 'x (plusE (idE 'x) (idE 'x))) (plusE (numE 2) (numE 2)))

; functional programming practice

; to implement (in any order)
;   all-greater-than-5?
;   all-nums-pass?
;   all-pass?
;   any-pass?

(any-pass? : (('a -> Boolean) (Listof 'a) -> Boolean))
(define (any-pass? pred xs)
  (type-case (Listof 'a) xs
    [empty
      #f
    ]
    [(cons f r)
      (if (pred f)
        #t
        (any-pass? pred r)
      )
    ]
  )
)

(all-pass? : (('a -> Boolean) (Listof 'a) -> Boolean))
(define (all-pass? pred xs)
  (type-case (Listof 'a) xs
    [empty
      #t
    ]
    [(cons f r)
      (if (pred f)
        (and (all-pass? pred r))
        #f
      )
    ]
  )
)

(all-nums-pass? : ((Number -> Boolean) (Listof Number) -> Boolean))
(define (all-nums-pass? pred ns)
  (all-pass? pred ns)
)

(all-greater-than-5? : ((Listof Number) -> Boolean))
(define (all-greater-than-5? ns)
  (type-case (Listof Number) ns
    [empty
      #t
    ]
    [(cons f r)
      (if (> f 5)
        (and (all-greater-than-5? r))
        #f
      )
    ]
  )
)

(test (all-greater-than-5? (list 6 7 8 9 10)) #t)
(test (all-greater-than-5? (list 0 6 7 8 9 10)) #f)