#lang plait #:untyped

; notice that this module is untyped
; you write regular plait code but the types are not checked

(define message
    (lambda (x y) 
        (+ x y)
    )
)

(string-append "hey " (message 10 12))