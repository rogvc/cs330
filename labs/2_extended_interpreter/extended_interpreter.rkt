#lang plait
(print-only-errors #t)

; Definitions

(define-type Env
  (mtEnv)
  (anEnv [name : Symbol] [value : CFWAE-Value] [env : Env])
)

(define-type Binding
  (binding [x : Symbol] [bound-expr : CFWAE])
)

(define-type Op
  (plusOp)
  (minusOp)
  (multOp)
  (divOp)
)

(define-type CFWAE
  (numC [n : Number])
  (binopC [op : Op] [lhs : CFWAE] [rhs : CFWAE])
  (withC [lob : (Listof Binding)] [body : CFWAE])
  (idC [x : Symbol])
  (if0C [c : CFWAE] [t : CFWAE] [e : CFWAE])
  (funC [args : (Listof Symbol)] [body : CFWAE])
  (appC [f : CFWAE] [args : (Listof CFWAE)])
)

(define-type CFWAE-Value
  (numV [n : Number])
  (closureV [params : (Listof Symbol)]
            [body : CFWAE]
            [env : Env])
)

; Parse heloer functions and Parse itself

(reserved-words : (Listof Symbol))
(define reserved-words (list '+ '- '* '/ 'with 'ifo 'fun))

(valid-id? : (S-Exp -> Boolean))
(define (valid-id? s-exp)
  (not (member (s-exp->symbol s-exp) reserved-words))
)

(is-binopC : (S-Exp -> Boolean))
(define (is-binopC s-exp)
  (cond
    [(s-exp-match? `(+ ANY ANY) s-exp)
      #t
    ]
    [(s-exp-match? `(- ANY ANY) s-exp)
      #t
    ]
    [(s-exp-match? `(* ANY ANY) s-exp)
      #t
    ]
    [(s-exp-match? `(/ ANY ANY) s-exp)
      #t
    ]
    [else
      #f
    ]
  )
)

(match-operator : (Symbol -> Op))
(define (match-operator operator)
  (cond
    [(equal? operator '+)
      (plusOp)
    ]
    [(equal? operator '-)
      (minusOp)
    ]
    [(equal? operator '*)
      (multOp)
    ]
    [(equal? operator '/)
      (divOp)
    ]
    [else
      (error 'match-operator "Couldn't match operator.")
    ]
  )
)

(parse : (S-Exp -> CFWAE))
(define (parse s-exp)
  (cond
    [(s-exp-match? `NUMBER s-exp)
      (numC (s-exp->number s-exp))
    ]
    [(s-exp-match? `SYMBOL s-exp)
      (if (valid-id? s-exp)
        (idC (s-exp->symbol s-exp))
        (error 'parse "Unexpected reserved word.")
      )
    ]
    [(is-binopC s-exp)
      (let ([se-list (s-exp->list s-exp)])
        (if (= (length se-list) 3)
          (type-case (Listof S-Exp) se-list
            [empty
              (error 'parse "Unexpected empty expression.")
            ]
            [(cons f r)
              (binopC 
                (match-operator (s-exp->symbol f))
                (parse (second se-list))
                (parse (third se-list))
              )
            ]
          )
          (error 'parse "Expression needs to be in this format: (operator operand operand)")
        )
      ) 
    ]
    [else
      (error 'parse "Operation not in our syntax.")
    ]
  )
)

; parse tests
; (Also tests all helper functions)

(test (parse `42) (numC 42))
(test (parse `lol) (idC 'lol))
(test (parse `(+ 22 22)) (binopC (plusOp) (numC 22) (numC 22)))
(test (parse `(- 22 22)) (binopC (minusOp) (numC 22) (numC 22)))
(test (parse `(* 22 22)) (binopC (multOp) (numC 22) (numC 22)))
(test (parse `(/ 22 22)) (binopC (divOp) (numC 22) (numC 22)))
(test (parse `(+ (- 22 10) (* 2 (/ 1 2)))) 
  (binopC 
    (plusOp) 
    (binopC 
      (minusOp) 
      (numC 22) 
      (numC 10)
    ) 
    (binopC 
      (multOp)
      (numC 2)
      (binopC
        (divOp)
        (numC 1)
        (numC 2)
      )
    )
  )
)