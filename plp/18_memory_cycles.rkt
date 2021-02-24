#lang plait
(print-only-errors #t)

(define-type-alias Recognizer ((Listof Event) -> Boolean))

(define-type Event
  [allocation]
  [first-use]
  [last-use]
  [deallocation]
  [other])

; to implement
; - recognize

(define-type State
  [unallocated]
  [allocated]
  [in-use]
  ;;; [freed-early]
  [unused]
  ;;; [double-freed]
  [unknown]
)

(which-state : (Event -> State))
(define (which-state e)
  (type-case Event e
    ([allocation]
      [allocated]
    )
    ([first-use]
      [in-use]
    )
    ([last-use]
      [unused]
    )
    ([deallocation]
      [unallocated]
    )
    ([other]
      [unknown]
    )
  )
)

(recognize : Recognizer)
(define (recognize events)
  (recognize-rec (unallocated) events)
)

(define (unallocated->allocated state event)
  (and (equal? state (unallocated)) (equal? (which-state event) (allocated)))
)

(define (allocated->in-use state event)
  (and (equal? state (allocated)) (equal? (which-state event) (in-use)))
)

(define (in-use->unused state event)
  (and (equal? state (in-use)) (equal? (which-state event) (unused)))
)

(define (unused->unallocated state event)
  (and (equal? state (unused)) (equal? (which-state event) (unallocated)))
)

(recognize-rec : (State (Listof Event) -> Boolean))
(define (recognize-rec state events)
  (type-case (Listof Event) events
    [empty
      #t
    ]
    [(cons f r)
      (cond
        [(unallocated->allocated state f)
          (recognize-rec (which-state f) r)
        ]
        [(allocated->in-use state f)
          (recognize-rec (which-state f) r)
        ]
        [(in-use->unused state f)
          (recognize-rec (which-state f) r)
        ]
        [(unused->unallocated state f)
          (recognize-rec (which-state f) r)
        ]
        [else
          #f
        ]
      )
    ]
  )
)

(test (recognize empty) #t)
(test (recognize (list (allocation) (first-use) (last-use) (deallocation))) #t)
(test (recognize (list (allocation) (deallocation) (last-use))) #f)