#lang plait

(define-type Expr
  (numE [n : Number])
  (plusE [lhs : Expr] [rhs : Expr])
  (equalE [lhs : Expr] [rhs : Expr])
  (notE [arg : Expr]))

(define-type Type
  (numT)
  (boolT))

; to implement
; - typecheck
(all-numT? : (Expr Expr -> Boolean))
(define (all-numT? l r)
    (and
        (equal? (numT) (typecheck l))
        (equal? (numT) (typecheck r))
    )
)

(is-boolT? : (Expr -> Boolean))
(define (is-boolT? arg)
    (equal? (boolT) (typecheck arg))
)

(typecheck : (Expr -> Type))
(define (typecheck expr)
    (type-case Expr expr
        [(numE n)
            (numT)
        ]
        [(plusE lhs rhs)
            (if (all-numT? lhs rhs)
                (numT)
                (error 'typecheck "your plus operation is malformed.")
            )
        ]
        [(equalE lhs rhs)
            (if (all-numT? lhs rhs)
                (boolT)
                (error 'typecheck "your equal operation is malformed.")
            )
        ]
        [(notE arg)
            (if (is-boolT? arg)
                (boolT)
                (error 'typecheck "your not operation is malformed.")
            )
        ]
    )
)