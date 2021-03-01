#lang plait
(require (typed-in racket/base [exact-integer? : (Number -> Boolean)]))
(print-only-errors #t)

(define (8-bit? n)
  (and (exact-integer? n)
       (>= n -128)
       (< n 128)))

(test (8-bit? 0) #true)
(test (8-bit? 10) #true)
(test (8-bit? 42) #true)
(test (8-bit? 127) #true)
(test (8-bit? 128) #false)
(test (8-bit? -128) #true)
(test (8-bit? -129) #false)

; to implement
; - 8008+
; - 8008-
(define (8008+ x y)
  (if (and (8-bit? x) (8-bit? y))
    (let ([sum (+ x y)])
      (if (8-bit? sum)
        sum
        (cond
          [(equal? sum 128)
            -128
          ]
          [(equal? sum -128)
            128
          ]
          [(> sum 128)
            (+ (- sum 128) -128)
          ]
          [else
            (+ (+ sum 128) 128)
          ]
        )
      )  
    )
    (error '8008+ "Input wasn't an 8-bit number.")
  )
)

(define (8008- x y) 
  (if (and (8-bit? x) (8-bit? y))
    (let ([sub (- x y)])
      (if (8-bit? sub)
        sub
        (cond
          [(equal? sub 128)
            -128
          ]
          [(equal? sub -128)
            128
          ]
          [(> sub 128)
            (+ (- sub 128) -128)
          ]
          [else
            (+ (+ sub 128) 128)
          ]
        )
      )  
    )
    (error '8008- "Input wasn't an 8-bit number.")
  )
)