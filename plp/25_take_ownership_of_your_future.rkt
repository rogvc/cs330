#lang plait #:untyped
; notice that this module is untyped
; it's just like regular plait but it will not
; detect type errors statically

; the following syntax rule defines a generator expression
; `yield` is the identifier you use to yield values within the body
; `body` is a sequence of 0 or more expressions
(define-syntax-rule
  (generator yield body ...)
  (let ([save-point (none)])
    (lambda ()
      (call/cc
       (lambda (return-point)
         (type-case (Optionof 'a) save-point
           [(some k)
            (k return-point)]
           [(none)
            (let ([yield (lambda (y)
                           (set! return-point
                                 (call/cc
                                  (lambda (yield-point)
                                    (begin
                                      (set! save-point (some yield-point))
                                      (return-point y))))))])
              (begin body ...
                     (error 'generator "no more values")))]))))))

; reference
; delete before submitting
;;; (let ([g (generator yield
;;;            (yield 1)
;;;            (yield 2)
;;;            (yield 3)
;;;            (yield 4))])
;;;   (list (g) (g) (g) (g)))

; to implement
; - facts
; - fibs

;;; (facts : (-> Number))
(define facts
    (generator yield
        (letrec 
            ([factz (lambda (x f) 
                (begin (yield f) 
                    (if (= x 0)
                        (factz (add1 x) f)
                        (if (= x 1)
                            (factz (add1 x) f)
                            (factz (add1 x) (* x f))
                        )
                    )
                )
            )])
            (factz 1 1)
        )
    )
)

(define fibs
    (generator yield
        (letrec 
            ([fibz (lambda (x f) 
                    (cond
                        [(= x 0)
                            (begin (yield 0) (fibz (add1 x) 0))
                        ]
                        [(= x 1)
                            (begin (yield 1) (fibz (add1 x) 1))
                        ]
                        [else
                            (begin (yield f) (fibz (+ x f) x))
                        ]
                    )
            )])
            (fibz 0 0)
        )
    )
)

;;; (list (facts) (facts) (facts) (facts) (facts))
(list (fibs) (fibs) (fibs) (fibs) (fibs) (fibs) (fibs))