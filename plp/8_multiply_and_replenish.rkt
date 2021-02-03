#lang plait

(define-type (Treeof 'a)
  [leaf]
  [branch (value : 'a)
          (left-child : (Treeof 'a))
          (right-child : (Treeof 'a))])

; to implement:
; - count-nodes
; - sum-nodes
; - tree-map

(count-nodes : ((Treeof 'a) -> Number))
(define (count-nodes t)
  (type-case (Treeof 'a) t
    [(leaf)
      0
    ]
    [(branch value left-child right-child)
      (+ 1 (+ (count-nodes left-child) (count-nodes right-child)))
    ]
  )
)

(sum-nodes : ((Treeof 'a) -> Number))
(define (sum-nodes t)
  (type-case (Treeof 'a) t
    [(leaf)
      0
    ]
    [(branch value left-child right-child)
      (+ value (+ (sum-nodes left-child) (sum-nodes right-child)))
    ]
  )
)

(tree-map : (('a -> 'b) (Treeof 'a) -> (Treeof 'b)))
(define (tree-map f t)
  (type-case (Treeof 'a) t
    [(leaf)
      (leaf)
    ]
    [(branch value left-child right-child)
      (branch (f value) (tree-map f left-child) (tree-map f right-child))
    ]
  )
)
; and optionally...

; - tree-foldl