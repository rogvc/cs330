#lang plait
(print-only-errors #t)

; to implement:

; - list-reverse
(define helper-list
  (lambda (f s)
    (type-case (Listof 'a) f
      [empty
        s
      ]
      [(cons f-1 r-1)
        (helper-list r-1 (cons f-1 s))
      ]
    )
  )
)

(list-reverse : ((Listof 'a) -> (Listof 'a)))
(define (list-reverse xs)
  (helper-list xs '())
)

(test (list-reverse (list 1 2 3 4 5)) (list 5 4 3 2 1))
(test (list-reverse (list 'q 'w 'e 'r 't 'y)) (list 'y 't 'r 'e 'w 'q))

(define-type (Treeof 'a)
  [leaf]
  [branch (value : 'a)
          (left-child : (Treeof 'a))
          (right-child : (Treeof 'a))])

; - tree-reverse

(define (tree-reverse t)
  (type-case (Treeof 'a) t
    [(leaf)
      (leaf)
    ]
    [(branch value lc rc)
      (branch value (tree-reverse rc) (tree-reverse lc))
    ]
  )
)