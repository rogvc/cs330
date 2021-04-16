#lang plait
(print-only-errors #t)

(define (main expr) 
  (interp 
    (parse expr)
    (mtEnv)
  )
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

(define-type Op
  (plusOp)
  (minusOp)
  (multOp)
  (divOp)
)

(define-type Binding
  (binding [x : Symbol] [bound-expr : CFWAE])
)

(define-type Env
  (mtEnv)
  (anEnv [name : Symbol] [value : CFWAE-Value] [env : Env])
)

(define-type CFWAE-Value
  (numV [n : Number])
  (closureV [params : (Listof Symbol)]
            [body : CFWAE]
            [env : Env])
)

(lookup : (Symbol Env -> CFWAE-Value))
(define (lookup x env)
  (type-case Env env
    [(mtEnv)
      (error 'lookup (string-append "Unbound identifier " (symbol->string x)))
    ]
    [(anEnv name value env2)
      (if (equal? name x)
        value
        (lookup x env2)
      )
    ]
  )
)

(plus : (CFWAE-Value CFWAE-Value -> CFWAE-Value))
(define (plus n1 n2)
  (type-case CFWAE-Value n1
    [(numV x)
      (type-case CFWAE-Value n2
        [(numV y)
          (numV (+ x y))
        ]
        [else
          (error 'plus "Unexpected value.")
        ]
      )    
    ]
    [else
      (error 'plus "Unexpected value.")
    ]
  )
)

(define (is-reserved-word? s-exp)
  (if (s-exp-symbol? s-exp)
    (let ([word (s-exp->symbol s-exp)])
      (cond
        [(equal? word 'if0) #t]
        [(equal? word 'with) #t]
        [(equal? word 'fun) #t]
        [(equal? word '+) #t]
        [(equal? word '-) #t]
        [(equal? word '*) #t]
        [(equal? word '/) #t]
        [else #f]
      )
    )
    #f
  )
)

(define (is-in-list? word lister)
  (type-case (Listof S-Exp) lister
    [empty #f]
    [(cons f r)
      (if (s-exp-match? word f)
        #t
        (is-in-list? word r)
      )
    ]
  )
)

(define (no-duplicates? s-exp visited)
  (type-case (Listof S-Exp) s-exp
    [empty #t]
    [(cons f r)
      (type-case (Listof S-Exp) (s-exp->list f)
        [empty (error 'parse "oops.")]
        [(cons f1 r1)
          (if (is-in-list? f1 visited)
            #f
            (no-duplicates? r (append visited (list f)))
          ) 
        ]
      ) 
    ]
  )
)

(define (is-valid-binding? s-exp)
  (type-case (Listof S-Exp) s-exp
    [empty #t]
    [(cons f1 r1)
      (type-case (Listof S-Exp) (s-exp->list f1)
        [empty (error 'parse "Found empty binding.")]
        [(cons f2 r2)
          (if 
            (and
              (and (= 2 (length (s-exp->list f1))) (s-exp-symbol? f2)) ; symbol <-> value
              (no-duplicates? s-exp (list)) 
            )
            (is-valid-binding? r1)
            #f
          )
        ]
      )
    ]
  )
  
)

(define (make-new-binding-list s-exp return-list)
  (type-case (Listof S-Exp) s-exp
    [empty return-list]
    [(cons f1 r1)
      (type-case (Listof S-Exp) (s-exp->list f1)
        [empty (error 'parse "Invalid 'binding' syntax.")]
        [(cons f2 r2)
          (make-new-binding-list r1 (append return-list (list (binding (s-exp->symbol f2) (parse (first r2))))))
        ]
      )
    ]
  )
)

(define (parse-binding s-exp)
  (cond
    [(not (s-exp-list? s-exp)) ; Is it a list?
      (error 'parse "Invalid 'bindings' syntax - not a list.")
    ]
    [(not (is-valid-binding? (s-exp->list s-exp))) ; Is it valid?
      (error 'parse "Invalid 'bindings' syntax.")
    ]
    [else
      (make-new-binding-list (s-exp->list s-exp) (list))
    ]
  )
)

(define (all-symbols? s-exp)
  (type-case (Listof S-Exp) s-exp
    [empty #t]
    [(cons f r)
      (if (s-exp-symbol? f)
        (all-symbols? r)
        #f
      )
    ]
  )
)

(define (to-symbol-list s-exp return-list)
  (type-case (Listof S-Exp) s-exp
    [empty return-list]
    [(cons f r)
      (if (s-exp-symbol? f)
        (to-symbol-list r (append return-list (list (s-exp->symbol f))))
        (error 'parse "Invalid 'fun' syntax.")
      )
    ]
  )
)

(define (no-id-duplicates? s-exp visited)
  (type-case (Listof S-Exp) s-exp
    [empty #t]
    [(cons f r)
      (if (is-in-list? f visited)
        #f
        (no-id-duplicates? r (append visited (list f)))
      )
    ]
  )
)

(define (assert-args s-exp)
  (cond
    [(not (s-exp-list? s-exp)) ; Is it a list?
      (error 'parse "Invalid 'args' syntax.")
    ]
    [(not (all-symbols? (s-exp->list s-exp)))
      (error 'parse "Invalid 'args' syntax.")
    ]
    [(no-id-duplicates? (s-exp->list s-exp) (list))
      (to-symbol-list (s-exp->list s-exp) (list))
    ]
    [else
      (error 'parse "Invalid 'args' syntax.")
    ]
  )
)

(define (match-binop opr)
  (cond
    [(equal? '+ opr)
      (plusOp)
    ]
    [(equal? '- opr)
      (minusOp)
    ]
    [(equal? '* opr)
      (multOp)
    ]
    [(equal? '/ opr)
      (divOp)
    ]
  )
)

(define (parse s-exp)
  (cond
    [(s-exp-number? s-exp) 
      (numC (s-exp->number s-exp))
    ]
    [(s-exp-symbol? s-exp) 
      (if (is-reserved-word? s-exp)
        (error 'parse "Use of reserved word.")
        (idC (s-exp->symbol s-exp))
      )
    ]
    [(s-exp-list? s-exp)
      (if (empty? (s-exp->list s-exp))
        (error 'parse "Input was an empty list.")
        (let ([word (first (s-exp->list s-exp))])
          (if (is-reserved-word? word)
            (cond
              [(equal? (s-exp->symbol word) 'if0) 
                (parse-if0 (s-exp->list s-exp))
              ]
              [(equal? (s-exp->symbol word) 'with) 
                (parse-with (s-exp->list s-exp))
              ]
              [(equal? (s-exp->symbol word) 'fun) 
                (parse-fun (s-exp->list s-exp))
              ]
              [else
                (parse-binop (s-exp->list s-exp))
              ]
            )
            (parse-app (s-exp->list s-exp))
          )
        )
      )
    ]
    [else
      (error 'parse "Invalid syntax.")
    ]
  )
)

(define (parse-if0 s-exp)
  (cond
    [(= 4 (length s-exp))
      (if0C 
        (parse (second s-exp))
        (parse (third s-exp))
        (parse (fourth s-exp))
      )
    ]
    [else
      (error 'parse "Invalid 'if' syntax.")
    ]
  )
)

(define (parse-with s-exp)
  (if (= 3 (length s-exp))
    (withC
      (parse-binding (second s-exp))
      (parse (third s-exp))
    )
    (error 'parse "Invalid 'with' syntax.")
  )
)

(define (parse-fun s-exp)
  (if (= 3 (length s-exp))
    (funC 
      (assert-args (second s-exp))
      (parse (third s-exp))
    )
    (error 'parse "Invalid 'fun' syntax.")
  )
)

(define (parse-binop s-exp)
  (cond
    [(= 3 (length s-exp))
      (binopC
        (match-binop (s-exp->symbol (first s-exp)))
        (parse (second s-exp))
        (parse (third s-exp))
      )
    ]
    [else
      (error 'parse "Invalid 'binop' syntax.")
    ]
  )
)

(define (parse-app s-exp)
  (appC
    (parse (first s-exp))
    (map parse (rest s-exp))
  )
)


(define (is-numV? cfwaev)
  (type-case CFWAE-Value cfwaev
    [(numV n) #t]
    [else #f]
  )
)

(define (is-closureV? cfwaev)
  (type-case CFWAE-Value cfwaev
    [(closureV x y z) #t]
    [else #f]
  )
)

(define (match-binop-value opr l r)
  (type-case Op opr
    [(plusOp)
      (numV (+ (numV-n l) (numV-n r)))
    ]
    [(minusOp)
      (numV (- (numV-n l) (numV-n r)))
    ]
    [(multOp)
      (numV (* (numV-n l) (numV-n r)))
    ]
    [(divOp)
      (if (= (numV-n r) 0)
        (error 'interp "Cannot divide by zero.")
        (numV (/ (numV-n l) (numV-n r)))
      )
    ]
  )
)

(define (interp expr env)
  (type-case CFWAE expr
    [(numC n) (numV n)]
    [(idC x) (lookup x env)]
    [(binopC opr lhs rhs)
      (match-binop-value opr (interp lhs env) (interp rhs env))
    ]
    [(withC lob body)
      (let 
        ([nenv 
          (foldl 
            (lambda (b e) 
              (anEnv (binding-x b) (interp (binding-bound-expr b) env) e)
            )
            env
            lob
          )
        ])
        (interp body nenv)
      )
    ]
    [(if0C c t e)
      (let ([cenv (interp c env)])
        (cond
          [(not (is-numV? cenv))
            (error 'interp "Invalid 'if' syntax.")
          ]
          [else
            (if (equal? (numV 0) cenv)
              (interp t env)
              (interp e env)
            )
          ]
        )
      )
    ]
    [(funC args body)
      (closureV args body env)
    ]
    [(appC f args)
      (type-case CFWAE-Value (interp f env)
        [(closureV params body cenv)
          (interp body (make-new-env params args cenv))
        ]
        [else
          (error 'interp "App received something that isn't a function.")
        ]
      )
    ]
  )
)

(define (make-new-env params args env)
  (type-case (Listof CFWAE) args
    [empty
      (type-case (Listof Symbol) params
        [empty env]
        [(cons f-param r-param)
          (error 'interp "Invalid function syntax.")
        ]
      )
    ]
    [(cons f r)
      (type-case (Listof Symbol) params
        [empty (error 'interp "Invalid function syntax.")]
        [(cons f-param r-param)
          (make-new-env r-param r (anEnv f-param (interp f env) env))          
        ]
      )   
    ]
  )
)

(test (parse `1) (numC 1))
(test/exn (parse `"1") "parse: Invalid syntax.")
(test (parse `(+ 2 2)) (binopC (plusOp) (numC 2) (numC 2)))
(test (parse `(- 3 2)) (binopC (minusOp) (numC 3) (numC 2)))
(test (parse `(* 4 5)) (binopC (multOp) (numC 4) (numC 5)))
(test (parse `(/ 6 7)) (binopC (divOp) (numC 6) (numC 7)))
(test/exn (parse `(+ 2)) "parse: Invalid 'binop' syntax.")
(test/exn (parse `(- 10 5 2)) "parse: Invalid 'binop' syntax.")

(test (parse `hey) (idC 'hey))
(test/exn (parse `+) "parse: Use of reserved word.")
(test/exn (parse `-) "parse: Use of reserved word.")
(test/exn (parse `*) "parse: Use of reserved word.")
(test/exn (parse `/) "parse: Use of reserved word.")
(test/exn (parse `with) "parse: Use of reserved word.")
(test/exn (parse `if0) "parse: Use of reserved word.")
(test/exn (parse `fun) "parse: Use of reserved word.")

(test (parse `(if0 0 1 2)) (if0C (numC 0) (numC 1) (numC 2)))
(test (parse `(if0 1 2 3)) (if0C (numC 1) (numC 2) (numC 3)))
(test/exn (parse `(if0 1 2)) "parse: Invalid 'if' syntax.")
(test/exn (parse `(if0 1 2 3 4)) "parse: Invalid 'if' syntax.")

(test/exn (parse `()) "parse: Input was an empty list.") 

(test (parse `(with ((x 3) (y 4)) (if0 1 x y))) (withC (list (binding 'x (numC 3)) (binding 'y (numC 4))) (if0C (numC 1) (idC 'x) (idC 'y))))
(test (parse `(with ((x 5) (y 4) (z 3)) (if0 1 x y))) (withC (list (binding 'x (numC 5)) (binding 'y (numC 4)) (binding 'z (numC 3))) (if0C (numC 1) (idC 'x) (idC 'y))))
(test/exn (parse `(with ((x 7) (y 4)))) "parse: Invalid 'with' syntax.")
(test/exn (parse `(with ((x 2) (y 8)) (if0 1 x y) (if0 1 x y))) "parse: Invalid 'with' syntax.")
(test/exn (parse `(with ((2) (y 10)) (if0 1 x y))) "parse: Invalid 'bindings' syntax.")
(test/exn (parse `(with ((x 4 20) (y 4)) (if0 1 x y))) "parse: Invalid 'bindings' syntax.")

(test (parse `(fun (x y) (* x y))) (funC '(x y) (binopC (multOp) (idC 'x) (idC 'y))))
(test/exn (parse `(fun (* x y))) "parse: Invalid 'fun' syntax.")
(test/exn (parse `(fun (x y) (x) (* x y))) "parse: Invalid 'fun' syntax.")

(test (parse `((fun (x y) (+ x y)) 2 3)) (appC (funC '(x y) (binopC (plusOp) (idC 'x) (idC 'y))) (list (numC 2) (numC 3))))
(test (parse `((fun (x y z) (with ((x 15) (y 34)) (* z (+ x y)))) 42 55 17)) (appC (funC '(x y z) (withC (list (binding 'x (numC 15)) (binding 'y (numC 34))) (binopC (multOp) (idC 'z) (binopC (plusOp) (idC 'x) (idC 'y))))) (list (numC 42) (numC 55) (numC 17))))
(test (parse `(app (1 2))) (appC (idC 'app) (list (appC (numC 1) (list (numC 2))))))
(test (parse `(app (1 2 4 5))) (appC (idC 'app) (list (appC (numC 1) (list (numC 2) (numC 4) (numC 5))))))

(test (main `2) (numV 2))
(test (main `(+ 6 4)) (numV 10))
(test (main `(+ 2 20)) (numV 22))
(test (main `(- 4 5)) (numV -1))
(test (main `(- 14 4)) (numV 10))
(test (main `(* 4 5)) (numV 20))
(test (main `(* 1 6)) (numV 6))
(test (main `(/ 4 4)) (numV 1))
(test/exn (main `(/ 10 0)) "interp: Cannot divide by zero.")
(test/exn (main `yo) "lookup: Unbound identifier yo")
(test (main `(if0 0 1 2)) (numV 1))
(test (main `(if0 2 1 45)) (numV 45))
(test/exn (main `(if0 1 2 whoa)) "lookup: Unbound identifier whoa")
(test (main `(with ((x 2) (y 4)) (if0 1 x y))) (numV 4))
(test (main `(with ((x 2) (y 4)) (if0 0 x y))) (numV 2))
(test/exn (main `(with ((x 2) (y false)) (if0 0 x y))) "lookup: Unbound identifier false")
(test (main `(fun (x) x)) (closureV '(x) (idC 'x) (mtEnv)))
(test (main `(fun (x) (+ 2 (* 2 x)))) (closureV '(x) (binopC (plusOp) (numC 2) (binopC (multOp) (numC 2) (idC 'x))) (mtEnv)))
(test (main `((fun (x y z) (with ((x 15) (y 34)) (* z (+ x y)))) 42 55 17)) (numV 833))
(test (main `((fun (x y) (+ x y)) 2 3)) (numV 5))
(test (main `((fun (x y z) (* (/ x y) z)) 2 10 4)) (numV 4/5))
