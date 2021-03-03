#lang plait

(define-type-alias Address Number)

(define-type HeapObject
  [pair (left : Address) (right : Address)]
  [atom])

(define-type-alias Heap (Address -> HeapObject))

; to implement
; - reachable?
(print-only-errors #t)

(define (reachable? a roots heap)
  (type-case (Listof Address) roots
    [empty 
      #f
    ]
    [(cons f r)
      (if (reachable?-rec a f heap (list))
        #t
        (reachable? a r heap)
      )
    ]
  )
)

(define (reachable?-rec a root heap visited)
  (cond
    [(is-in-list a visited)
      #f
    ]
    [(equal? a root) 
      #t
    ]
    [else
      (type-case HeapObject (heap root)
        [(atom) 
          #f
        ]
        [(pair l ri)
          (cond
            [(or (equal? a l) (equal? a ri))
              #t
            ]
            [else
              (or
                (if (is-in-list root visited)
                  #f
                  (reachable?-rec a ri heap (append (list root) visited)) 
                )
                (if (is-in-list root visited)
                  #f
                  (reachable?-rec a l heap (append (list root) visited)) 
                )                
              )
            ]
          )
        ]
      )
    ]
  )
)

(define (is-in-list a lister)
  (type-case (Listof Address) lister
    [empty
      #f
    ]
    [(cons f r)
      (if (equal? a f)
        #t
        (is-in-list a r)
      )
    ]
  )
)

(test (reachable? 0 (list) (λ (x) (atom))) #f)
(test (reachable? 1 (list 0) (λ (x) (if (= x 0) (pair 1 2) (atom)))) #t)
(test (reachable? 3 (list 0) (λ (x) (cond ((= x 0) (pair 1 2)) ((= x 2) (pair 3 4)) (else (atom))))) #t)
(test (reachable? 0 (list 0) (λ (x) (atom))) #t)
(test (reachable? 3 (list 0) (λ (x) (cond ((= x 0) (pair 1 2)) ((= x 2) (pair 0 1)) (else (atom))))) #f)