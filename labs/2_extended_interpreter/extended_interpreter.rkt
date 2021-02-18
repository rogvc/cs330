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

(is-number : (S-Exp -> Boolean))
(define (is-number s-exp)
  (s-exp-match? `NUMBER s-exp)
)

(is-symbol : (S-Exp -> Boolean))
(define (is-symbol s-exp)
  (s-exp-match? `SYMBOL s-exp)
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

(is-named-expression : (S-Exp Symbol -> Boolean))
(define (is-named-expression s-exp symbol)
  (let ([se-list (s-exp->list s-exp)])
    (type-case (Listof S-Exp) se-list
      [empty
        (error 'is-withC "Unexpected empty expression.")
      ]
      [(cons f r)
        (if (equal? (s-exp->symbol f) symbol)
          #t
          #f
        )
      ]
    )
  )
)

(is-withC : (S-Exp -> Boolean))
(define (is-withC s-exp)
  (is-named-expression s-exp 'with)
)

(is-funC : (S-Exp -> Boolean))
(define (is-funC s-exp)
  (is-named-expression s-exp 'fun)
)

(is-if0C : (S-Exp -> Boolean))
(define (is-if0C s-exp)
  (is-named-expression s-exp 'if0)
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

(is-s-exp-symbol? : (S-Exp Symbol -> Boolean))
(define (is-s-exp-symbol? s-exp symbol)
  (if (and (s-exp-match? `SYMBOL s-exp) (equal? (s-exp->symbol s-exp) symbol))
    #t
    #f
  )
)

(is-list-of-bindings? : (S-Exp -> Boolean))
(define (is-list-of-bindings? s-exp)
  (if (s-exp-match? `([ANY ANY] ...) s-exp)
    #t
    #f
  )
)

(proper-with? : ((Listof S-Exp) -> Boolean))
(define (proper-with? exp-list)
  (type-case (Listof S-Exp) exp-list
    [empty
      #f
    ]
    [(cons f r)
      (if (and (is-s-exp-symbol? f 'with) (is-list-of-bindings? (second exp-list)))
        #t
        #f
      )
    ]
  )
)

(s-exp->binding : (S-Exp -> Binding))
(define (s-exp->binding s-exp)
  (let ([se-list (s-exp->list s-exp)])
    (type-case (Listof S-Exp) se-list
      [empty
        (error 's-exp->binding "Unexpected empty expression.")  
      ]
      [(cons f r)
        (if (s-exp-match? `SYMBOL f)
          (binding (s-exp->symbol f) (parse (second se-list)))
          (error 's-exp->binding "Binding should be of the format (binding [x : Symbol] [bound-expr : CFWAE]).")
        )
      ]
    )
  )
)

(any-duplicate-bindings? : ((Listof Binding) -> Boolean))
(define (any-duplicate-bindings? bindings)
  (type-case (Listof Binding) bindings
    [empty
      #f
    ]
    [(cons f r)
      (if (member f r)
        #t
        (any-duplicate-bindings? r)
      )
    ]
  )
)

(parse-bindings-list : ((Listof S-Exp) -> (Listof Binding)))
(define (parse-bindings-list se-list)
  (type-case (Listof S-Exp) se-list
    [empty
      empty
    ]
    [(cons f r)
      (cons (s-exp->binding f) (parse-bindings-list r))
    ]
  ) 
)

(parse : (S-Exp -> CFWAE))
(define (parse s-exp)
  (cond
    [(is-number s-exp)
      (numC (s-exp->number s-exp))
    ]
    [(is-symbol s-exp)
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
          (error 'parse "Expression needs to be in this format: (Op CFWAE CFWAE)")
        )
      ) 
    ]
    [(is-withC s-exp)
      (let ([se-list (s-exp->list s-exp)])
        (if (= (length se-list) 3)
          (type-case (Listof S-Exp) se-list
            [empty
              (error 'parse "Unexpected empty expression.")
            ]
            [(cons f r)
              (if (not (proper-with? se-list))
                (error 'parse "Expression 'with' formatted incorrectly.")
                (if (any-duplicate-bindings? (parse-bindings-list (s-exp->list (second se-list))))
                  (error 'parse "Expression 'with' cannot have duplicate bindings")
                  (withC
                    (parse-bindings-list (s-exp->list (second se-list)))
                    (parse (third se-list))
                  )
                )      
              )
            ]
          )
          (error 'parse "Expression needs to be in this format: (with ([id CFWAE] ...) CFWAE)")
        )
      )
    ]
    [(is-funC s-exp)
      (....)
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
(test (parse `(+ (- 22 10) (* 2 (/ 1 2)))) (binopC (plusOp) (binopC (minusOp) (numC 22) (numC 10)) (binopC (multOp) (numC 2) (binopC (divOp) (numC 1) (numC 2)))))
(test (parse `(with ([x 10] [y 20] [z 10]) (+ x (- y z)))) (withC (list [binding 'x (numC 10)] [binding 'y (numC 20)] [binding 'z (numC 10)]) (binopC (plusOp) (idC 'x) (binopC (minusOp) (idC 'y) (idC 'z)))))
(test/exn (parse `(with ([x 10] [x 10]) (x))) "parse: Expression 'with' cannot have duplicate bindings")
(test (parse `(fun (x y) (+ x y))) (funC (list 'x 'y) (binopC (plusOp) (idC 'x) (idC 'Y))))