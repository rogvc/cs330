#lang plait

(define-type Term
  (var [n : Number])
  (bool)
  (num)
  (arrow [domain : Term]
         [range : Term])
  (listof [a : Term]))

(define-type-alias Substitution (Hashof Number Term))

(define (walk t subst)
  (type-case Term t
    [(var n)
     (type-case (Optionof Term) (hash-ref subst n)
       [(some t)
        (walk t subst)]
       [(none)
        t])]
    [else
     t]))

(unify : (Term Term Substitution -> (Optionof Substitution)))
(define (unify t0 t1 subst)
  (let ([t0 (walk t0 subst)]
        [t1 (walk t1 subst)])
    (type-case Term t0
      [(var n0)
       (type-case Term t1
         [(var n1)
          (if (= n0 n1)
            (some subst)
            (some (hash-set subst n0 (var n1))))]
         [else
          (some (hash-set subst n0 t1))])]
      [(arrow dom0 ran0)
       (type-case Term t1
         [(var n1)
          (some (hash-set subst n1 t0))]
         [(arrow dom1 ran1)
          (type-case (Optionof Substitution) (unify dom0 dom1 subst)
            [(some subst)
             (unify ran0 ran1 subst)]
            [(none)
             (none)])]
         [else
          (none)])]
      [(num)
       (type-case Term t1
         [(var n1)
          (some (hash-set subst n1 t0))]
         [(num)
          (some subst)]
         [else
          (none)])]
      [(bool)
       (type-case Term t1
         [(var n1)
          (some (hash-set subst n1 t0))]
         [(bool)
          (some subst)]
         [else
          (none)])]
      [(listof a0)
       ; add support for T ::= ... | (Listof T)
       (type-case Term t1
        [(var n1)
         (some (hash-set subst n1 t0))
        ]
        [(listof a1)
            (type-case (Optionof Substitution) (unify a0 a1 subst)
                [(some t)
                    (some subst)
                ]
                [(none)
                    (none)
                ]
            )
        ]
        [else
            (none)
        ]
       )
       ])))