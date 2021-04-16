#lang plait

(define (lookup gamma x) (gamma x))

(define (extend gamma x t)
  (λ (y) (if (equal? x y) t (lookup gamma y))))

(define-type Type
  (numT)
  (arrowT [domain : Type] [range : Type]))

(define-type Expr
  ....
  (recE [f : Symbol] [x : Symbol] [t-in : Type] [t-out : Type] [e : Expr])
  ....)

(define (tc gamma e)
  (type-case Expr e
    ....
    [(recE f x t-in t-out e)
      (let ([env (extend (extend gamma x t-in) f (arrowT t-in t-out))])
        (arrowT
          (if (equal? (tc env e) t-out)
            t-in
            (error 'tc "There was an error")
          )
          t-out
        )
      )
    ] ; fill in here (and only here)
    ....))

; don't modify anything above this comment
; except for the one place you fill in
