#lang plait
(print-only-errors #t)

; to implement:
; - make-adder1
; - make-titler1
; - make-maker
; - make-adder2
; - make-titler2

(make-adder1 : (Number -> (Number -> Number)))
(define (make-adder1 n)
  (lambda (param) (+ n param))
)

(make-titler1 : (String -> (String -> String)))
(define (make-titler1 title)
  (lambda (param) (string-append title param))
)

(make-maker : (('a 'b -> 'c) -> ('a -> ('b -> 'c))))
(define (make-maker f)
  (lambda (a) 
    (lambda (b) 
      (f a b)
    )
  )
)

; The following functions are incorrect.
; I still need to check with a TA on how to use make-maker to implement the next two functions

(make-adder2 : (Number -> (Number -> Number)))
(define (make-adder2 n)
  (lambda (param) (+ n param))
)

(make-titler2 : (String -> (String -> String)))
(define (make-titler2 title)
  (lambda (param) (string-append title param))
)