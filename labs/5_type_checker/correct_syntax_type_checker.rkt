#lang plait
(print-only-errors #t)

(define-type Expr
  (num [n : Number])
  (id [x : Symbol])
  (bool [b : Boolean])
  (bin-num-op [op : (Number Number -> Number)] [lhs : Expr] [rhs : Expr])
  (iszero [e : Expr])
  (bif [test : Expr] [then : Expr] [else : Expr])
  (with [bound-x : Symbol] [bound-body : Expr] [body : Expr])
  (fun 
    [arg-x : Symbol]
    [arg-type : Type]
    [body : Expr]
    [body-type : Type]
  )
  (app [fun : Expr] [arg : Expr])
  (nempty)
  (ncons [first : Expr] [rest : Expr])
  (nfirst [e : Expr])
  (nrest [e : Expr])
  (isnempty [e : Expr])
)

(define-type Type
  (t-num)
  (t-bool)
  (t-nlist)
  (t-fun [arg : Type] [result : Type])
)

(define-type Environment
  [n-env]
  [env (name : Symbol) (type : Type) (env : Environment)]
)

(is-zero? : (Number -> Boolean))
(define (is-zero? n)
  (equal? n 0)
)

(is-nempty? : (S-Exp -> Boolean))
(define (is-nempty? s-exp)
  (equal? `empty s-exp)
)

(is-bin-num-op? : (S-Exp -> Boolean))
(define (is-bin-num-op? s-exp)  
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

(s-exp->op : (S-Exp -> (Number Number -> Number)))
(define (s-exp->op s-exp)
  (cond
    [(s-exp-match? `+ s-exp)
      (lambda (n1 n2) (+ n1 n2))
    ]
    [(s-exp-match? `- s-exp)
      (lambda (n1 n2) (- n1 n2))
    ]
    [(s-exp-match? `* s-exp)
      (lambda (n1 n2) (* n1 n2))
    ]
    [(s-exp-match? `/ s-exp)
      (lambda (n1 n2) (/ n1 n2))
    ]
    [else
      (error 's-exp->op "This S-Exp isn't an binary number operation.")
    ]
  )
)

(extract-expr-id : (S-Exp -> S-Exp))
(define (extract-expr-id s-exp)
  (let ([se-list (s-exp->list s-exp)])
    (type-case (Listof S-Exp) se-list
      [empty
        (error 'parse "Invalid syntax.")
      ]
      [(cons f r)
        f
      ]
    )
  )
)

(extract-expr-value : (S-Exp -> S-Exp))
(define (extract-expr-value s-exp)
  (let ([se-list (s-exp->list s-exp)])
    (type-case (Listof S-Exp) se-list
      [empty
        (error 'parse "Invalid With syntax.")
      ]
      [(cons f r)
        (first r)
      ]
    )
  )
)

(extract-fun : ((Listof S-Exp) -> Expr))
(define (extract-fun se-list)
  (type-case (Listof S-Exp) se-list
    [empty (error 'parse "Invalid Syntax.")]
    [(cons f r)
      (fun
        (s-exp->symbol (first (s-exp->list f)))
        (type-of (id (s-exp->symbol (third (s-exp->list f)))))  
        (parse (third r))
        (type-of (id (s-exp->symbol (second r))))
      )
    ]
  )
)

(is-cons? : (S-Exp -> Boolean))
(define (is-cons? s-exp)
  (if (s-exp-list? s-exp)
    (is-cons?-rec (s-exp->list s-exp))
    (equal? `empty s-exp)
  )
)

(is-cons?-rec : ((Listof S-Exp) -> Boolean))
(define (is-cons?-rec s-exp)
  (type-case (Listof S-Exp) s-exp
    [empty
      (error 'parse "Should not reach the end of cons list.")
    ]
    [(cons f r)
      (if (equal? `cons f)
        (if (or 
              (s-exp-number? (second s-exp)) 
              (equal? (t-num) (type-of (parse (second s-exp))))
            )
          (is-cons? (third s-exp))
          (error 'is-cons?-rec "Invalid Syntax.")
        )
        (equal? `empty (third s-exp))
      )
    ]
  )
)

(parse : (S-Exp -> Expr))
(define (parse s-exp)
  (cond
    [(is-nempty? s-exp)
      (nempty)
    ]
    [(s-exp-number? s-exp)
      (num (s-exp->number s-exp))
    ]
    [(s-exp-symbol? s-exp)
      (id (s-exp->symbol s-exp))
    ]
    [(s-exp-boolean? s-exp)
      (bool (s-exp->boolean s-exp))
    ]
    [(s-exp-list? s-exp)
      (let ([se-list (s-exp->list s-exp)])
        (type-case (Listof S-Exp) se-list
          [empty
            (error 'parse "S-Exp was empty list.")
          ]
          [(cons f r)
            (cond
              [(is-bin-num-op? s-exp)
                (bin-num-op (s-exp->op f) (parse (second se-list)) (parse (third se-list)))
              ]
              [(equal? `zero? f)
                (iszero (parse (second se-list)))
              ]
              [(equal? `if f)
                (bif 
                  (parse (second se-list))
                  (parse (third se-list))
                  (parse (fourth se-list))
                )
              ]
              [(equal? `with f)
                (with 
                  (s-exp->symbol (extract-expr-id (second se-list)))
                  (parse (extract-expr-value (second se-list)))
                  (parse (third se-list))
                )
              ]
              [(equal? `fun f)
                (extract-fun r)
              ]
              [(s-exp-list? f)
                (app (parse f) (parse (second se-list)))
              ]
              [(equal? `cons f)
                (if (or 
                      (s-exp-number? (first r)) 
                      (equal? (t-num) (type-of (parse (first r))))
                    )
                  (cond
                    [(is-cons? (second r))
                      (ncons (parse (first r)) (parse (second r)))
                    ]
                    [(equal? `empty (second r))
                      (ncons (parse (first r)) (parse (second r)))
                    ]
                    [else
                      (error 'parse "Invalid Syntax.")
                    ]
                  )
                  (error 'parse "Invalid Syntax.")
                )
              ]
              [(equal? `first f)
                (if (is-cons? (second se-list))
                  (nfirst (parse (second se-list)))
                  (error 'parse "Invalid Syntax.")
                )
              ]
              [(equal? `rest f)
                (if (is-cons? (second se-list))
                  (nrest (parse (second se-list)))
                  (error 'parse "Invalid Syntax.")  
                )
              ]
              [(equal? `empty? f)
                (isnempty (parse (second se-list)))
              ]
              [else
                (error 'parse "Invalid Syntax.")
              ]
            )
          ]
        )
      )
    ]
    [else
      (error 'type-of "Invalid Syntax.")
    ]
  )
)

(type-of : (Expr -> Type))
(define (type-of e)
  (type-of-rec e (n-env))
)

(lookup : (Symbol Environment -> Type))
(define (lookup s e)
  (type-case Environment e
    [(n-env)
      (error 'lookup (string-append "Unbound identifier " (symbol->string s)))
    ]
    [(env n t r)
      (if (equal? s n)
        t
        (lookup s r)
      )
    ]
  )
)

(both-nums? : (Expr Expr Environment -> Boolean))
(define (both-nums? l r en)
  (and 
    (equal? (t-num) (type-of-rec l en))
    (equal? (t-num) (type-of-rec r en))
  )
)

(type-of-fun-arg : (Expr -> Type))
(define (type-of-fun-arg f)
  (type-case Expr f
    [(fun a t b bt)
      t
    ]
    [else
      (error 'type-of-fun-arg "Expression was not a function.")
    ]
  )
)

(type-of-fun-ret : (Expr -> Type))
(define (type-of-fun-ret f)
  (type-case Expr f
    [(fun a t b bt)
      bt
    ]
    [else
      (error 'type-of-fun-ret "Expression was not a function.")
    ]
  )
)

(type-of-rec : (Expr Environment -> Type))
(define (type-of-rec ex en)
  (type-case Expr ex
    [(num n)
      (t-num)
    ]
    [(id i)
      (cond
        [(equal? i 'true)
          (t-bool)
        ]
        [(equal? i 'false)
          (t-bool)
        ]
        [(equal? i 'number)
          (t-num)
        ]
        [(equal? i 'Boolean)
          (t-bool)
        ]
        [(equal? i 'empty)
          (t-nlist)
        ]
        [else
          (lookup i en)
        ]
      )
    ]
    [(bool b)
      (t-bool)
    ]
    [(bin-num-op o l r)
      (if (both-nums? l r en)
        (t-num)
        (error 'type-of "Right and Left sides should be numbers.")
      )
    ]
    [(iszero n)
      (if (equal? (t-num) (type-of-rec n en))
        (t-bool)
        (error 'type-of "Argument must be a number.")
      )
    ]
    [(bif x t e)
      (if (equal? (t-bool) (type-of-rec x en))
        (if (equal? (type-of-rec t en) (type-of-rec e en))
          (type-of-rec t en)
          (error 'type-of "Then and Else have different types.")
        )
        (error 'type-of "Expression should yield Boolean value.")
      )
    ]
    [(with i x b)
      (let [(nu-env (env i (type-of-rec x en) en))]
        (begin (type-of-rec x nu-env) (type-of-rec b nu-env))
      )
    ]
    [(fun a t b bt)
      (if (equal? bt (type-of-rec b (env a t en)))
        (t-fun t bt)
        (error 'type-of "Body and Return Type have different types.")
      )
    ]
    [(app f a)
      (if (equal? (t-fun (type-of-fun-arg f) (type-of-fun-ret f)) (type-of-rec f en))
        (if (equal? (type-of-fun-arg f) (type-of-rec a en))
          (type-of-fun-ret f)
          (error 'type-of "Function and Expression types are different.")
        )
        (error 'type-of "Invalid function expression syntax.")
      )
    ]
    [(nempty)
      (t-nlist)
    ]
    [(ncons f r)
      (if (equal? (t-num) (type-of-rec f en))
        (if (equal? (t-nlist) (type-of-rec r en))
          (t-nlist)
          (if (equal? (t-num) (type-of-rec r en))
            (t-nlist)
            (error 'type-of "Cons must receive a valid list expression.") 
          )
        )
        (error 'type-of "Invalid function expression syntax.")
      )
    ]
    [(nfirst e)
      (type-case Expr e
        [(ncons f r)
          (type-of-rec f en)
        ]
        [(nempty)
          (t-num)
        ]
        [else
          (error 'type-of "Invalid Syntax.")
        ]
      )
    ]
    [(nrest e)
      (if (equal? (t-nlist) (type-of-rec e en))
        (t-nlist)
        (error 'type-of "Rest argument must be a list expression.")
      )
    ]
    [(isnempty e)
      (if (equal? (t-nlist) (type-of-rec e en))
        (t-bool)
        (error 'type-of "Isnempty argument must be a list expression.")
      )
    ]
  )
)

(test (type-of (parse `0)) (t-num))
(test (type-of (parse `-1)) (t-num))
(test (type-of (parse `9001)) (t-num))
(test (type-of (parse `-534)) (t-num))
(test (type-of (parse `123)) (t-num))

(test/exn (type-of (parse `howdy)) "lookup: Unbound identifier howdy")
(test/exn (type-of (parse `$yMb0L)) "lookup: Unbound identifier $yMb0L")

(test (type-of (parse `true)) (t-bool))
(test (type-of (parse `false)) (t-bool))

(test (type-of (parse `{+ 1 2})) (t-num))
(test (type-of (parse `{- 1 2})) (t-num))
(test (type-of (parse `{* 1 2})) (t-num))
(test (type-of (parse `{/ 1 2})) (t-num))
(test (type-of (parse `{+ {- 1 2} {* 2 {/ 3 5}}})) (t-num))
(test (type-of (parse `{- {+ 1 2} {/ 2 {* 3 5}}})) (t-num))
(test/exn (type-of (parse `{3 4})) "parse: Invalid Syntax.")
(test/exn (type-of (parse `{+ false 400})) "type-of: Right and Left sides should be numbers.")
(test/exn (type-of (parse `{+ 2 wrong})) "lookup: Unbound identifier wrong")

(test (type-of (parse `(zero? 0))) (t-bool))
(test (type-of (parse `(zero? 23))) (t-bool))
(test/exn (type-of (parse `(zero? not-zero))) "lookup: Unbound identifier not-zero")
(test/exn (type-of (parse `(zero? false))) "type-of: Argument must be a number.")

(test (type-of (parse `(if true (+ 1 2) (* 3 4)))) (t-num))
(test (type-of (parse `(if false false true))) (t-bool))
(test/exn (type-of (parse `(if 1000 (+ 1 2) (- 3 4)))) "type-of: Expression should yield Boolean value.")
(test/exn (type-of (parse `(if false true (/ 12 432)))) "type-of: Then and Else have different types.")

(test (type-of (parse `(with (x 55) (+ x 43)))) (t-num))
(test (type-of (parse `(with (x (with (x 6) (+ 2 x))) (+ x 4)))) (t-num))
(test (type-of (parse `(with (x false) x))) (t-bool))
(test (type-of (parse `(with (x true) 5))) (t-num))
(test (type-of (parse `(with (x (if true (* 5 2) (+ 1 2))) (+ 23 x)))) (t-num))
(test/exn (type-of (parse `(with (x (zero? 43)) (+ 4 x)))) "type-of: Right and Left sides should be numbers.")
(test/exn (type-of (parse `(with (x (if true (zero? 5) (zero? 0))) (+ 4 x)))) "type-of: Right and Left sides should be numbers.")

(test (type-of (parse `(fun (x : number) : number x))) (t-fun (t-num) (t-num)))
(test (type-of (parse `(fun (x : number) : number (+ x 45)))) (t-fun (t-num) (t-num)))
(test (type-of (parse `(fun (x : Boolean) : Boolean (if x true false)))) (t-fun (t-bool) (t-bool)))
(test/exn (type-of (parse `(fun (x : number) : Boolean (+ x 45)))) "type-of: Body and Return Type have different types.")
(test/exn (type-of (parse `(fun (x : Boolean) : Boolean (+ x 45)))) "type-of: Right and Left sides should be numbers.")
(test/exn (type-of (parse `(fun (x : number) : number (zero? x)))) "type-of: Body and Return Type have different types.")

(test (type-of (parse `(cons 1 empty))) (t-nlist))
(test (type-of (parse `(cons 1 (cons 2 empty)))) (t-nlist))
(test (type-of (parse `(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 empty)))))))) (t-nlist))
(test (type-of (parse `(cons (+ 1 1) empty))) (t-nlist))
(test (type-of (parse `(cons 1 (cons 2 (cons 3 (cons (+ 4 5) (cons (- 6 1) empty))))))) (t-nlist))
(test/exn (type-of (parse `(cons 3 2 1))) "parse: Invalid Syntax.")
(test/exn (type-of (parse `(cons 3 false))) "parse: Invalid Syntax.")
(test/exn (type-of (parse `(cons false (cons 3 empty)))) "parse: Invalid Syntax.")

(test (type-of (parse `empty)) (t-nlist))
(test (type-of (parse `(empty? empty))) (t-bool))
(test (type-of (parse `(empty? (cons 1 empty)))) (t-bool))

(test (type-of (parse `(first (cons (+ 5 1) empty)))) (t-num))
(test (type-of (parse `(first (cons 12 (cons 10 (cons 11 empty)))))) (t-num))
(test (type-of (parse `(first empty))) (t-num))
(test/exn (type-of (parse `(first whoa))) "parse: Invalid Syntax.")

(test (type-of (parse `(rest (cons (+ 5 1) empty)))) (t-nlist))
(test (type-of (parse `(rest (cons 12 (cons 10 (cons 11 empty)))))) (t-nlist))
(test (type-of (parse `(rest empty))) (t-nlist))
(test/exn (type-of (parse `(rest whoa))) "parse: Invalid Syntax.")