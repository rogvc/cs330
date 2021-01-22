#lang plait

; parsing

(define-type AE
  [numE   (n   : Number)]
  [plusE  (lhs : AE) (rhs : AE)]
  [minusE (lhs : AE) (rhs : AE)])

; to implement:
;   parse

(parse : (S-Exp -> AE))
(define (parse s-exp)
  (cond
    [(s-exp-match? `NUMBER s-exp)
     (numE (s-exp->number s-exp))]
    [(s-exp-match? `(+ ANY ANY) s-exp)
     (let ([se (s-exp->list s-exp)])
       (plusE (parse (list-ref se 1))
             (parse (list-ref se 2))))]
    [(s-exp-match? `(- ANY ANY) s-exp)
     (let ([se (s-exp->list s-exp)])
       (minusE (parse (list-ref se 1))
             (parse (list-ref se 2))))]
   [else
    (error 'parse "not in our language")]
  )
)

; functional programming practice

; to implement (in any order)
;   add-5
;   add-m
;   transform-num-list
;   transform-list

(define-type (Listof 'a)
  [empty-list]
  [complete (first-element : 'a) (other-elements : (Listof 'a))]
 )

(define (add-5 ns)
  (add-m 5 ns)
)

(define (add-m m ns)
;  (type-case (Listof 'a) ns
;   [(empty-list) empty]
;   [(complete first-element other-elements) (cons (+ m first-element) (add-m m other-elements))]
;  )
  
; How to turn this into something without first and rest?
  (cond
   [(empty? ns) empty]
   [else
    (cons (+ m (first ns)) (add-m m (rest ns)))
   ])
)

(test (add-5 (list 0 0 0)) (list 5 5 5))
(test (add-m 25 (list 0 0 0)) (list 25 25 25))


(define (transform-num-list f ns)
  (cond
   [(empty? ns) empty]
   [else
    (cons (f (first ns)) (transform-num-list f (rest ns)))]
   )
  )
(define (transform-list f ns)
  (cond
   [(empty? ns) empty]
   [else
    (cons (f (first ns)) (transform-list f (rest ns)))]
   )
  )