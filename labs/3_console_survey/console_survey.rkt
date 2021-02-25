#lang plait
(print-only-errors #t)

(define-type Item
  (item [question : String] [response : String]))

(for-each : (('a -> Void) (Listof 'a) -> Void))
(define (for-each f xs)
  (type-case (Listof 'a) xs
    [empty (void)]
    [(cons x xs)
     (begin (f x) (for-each f xs))]))

(define q1
  (item 
    "\nWhich Star Wars trilogy is the best?\n" 
    "1. The Original\n2. The Prequel\n3. The Sequel\n"
  )
)

(define q2
  (item 
    "\nI’m amazed (and delighted!) at your choice. What about it bothers you the least?\n" 
    "1. the words the actors say\n2. how they say them\n3. something else\n"
  )
)

(define q3
  (item
    "\nWhose arc do you find more satisfying?\n"
    "1. Anakin Skywalker\n2. Luke Skywalker\n"
  )
)

(define q4
  (item 
    "\nWhat was the root cause of Anakin’s downfall?\n" 
    "1. his abilities\n2. his pride in his abilities\n3. his relationship with Padme\n4. his attachment to temporary things\n5. other...\n"
  )
)

(define q5
  (item 
    "\nIs Luke defined more by his hopefulness or his loyalty?\n" 
    "1. his hopefulness\n2. his loyalty\n"
  )
)

(define q6
  (item 
    "\nWhat in-universe explanation for Luke’s loss of hope in the sequel trilogy is most plausible?\n" 
    "1. the pressure of building a new Jedi Order\n2. unresolved fears about the Dark Side\n3. disillusionment with the history of the Jedi Order\n4. new knowledge about midichlorians\n5. other...\n"
  )
)

(define q7
  (item 
    "\nTo whom did Luke owe the most loyalty?\n" 
    "1. his father\n2. Obi Wan\n3. Han and Leia\n4. Yoda\n5. other...\n"
  )
)

(define end
  (item
    ""
    ""
  )
)

(display-item : ('a -> Void))
(define (display-item i)
  (type-case Item i
    [(item question response)
      (display (string-append question response))
    ]
  )
)

(display-all : ((Listof Item) -> Void))
(define (display-all is)
  (for-each display-item is)
)

(get-custom-answer : ( -> String))
(define (get-custom-answer)
  (string-append "  -> " (string-append (begin (display "What's your answer?\n") (s-exp->string (read))) "\n"))
)

(console-survey :  (-> Void))
(define (console-survey)
  (console-survey-rec q1 empty)
)

(console-survey-rec : (Item (Listof Item) -> Void))
(define (console-survey-rec i ans)
  (type-case Item i
    [(item question response)
      (cond
        [(equal? i q1)
          (let ([answer (begin (display-item i) (s-exp->number (read)))])
            (cond              
              [(equal? answer 1)
                (console-survey-rec q3 (append ans (list (item question "  -> The Original\n"))))
              ]
              [(equal? answer 2)
                (console-survey-rec q2 (append ans (list (item question "  -> The Prequel\n"))))
              ]

              [(equal? answer 3)
                (console-survey-rec q3 (append ans (list (item question "  -> The Sequel\n"))))
              ]
              [else
                (error 'console-survey "Invalid answer entered. Try again.")
              ]
            )         
          )
        ]
        [(equal? i q2)
          (let ([answer (begin (display-item i) (s-exp->number (read)))])
            (cond
              [(equal? answer 1)
                (console-survey-rec q3 (append ans (list (item question "  -> the words the actors say\n"))))
              ]
              [(equal? answer 2)
                (console-survey-rec q3 (append ans (list (item question "  -> how they say them\n"))))
              ]
              [(equal? answer 3)
                (console-survey-rec q3 (append ans (list (item question "  -> something else\n"))))
              ]
              [else
                (error 'console-survey "Invalid answer entered. Try again.")
              ]
            )         
          )
        ]
        [(equal? i q3)
          (let ([answer (begin (display-item i) (s-exp->number (read)))])
            (cond
              [(equal? answer 1)
                (console-survey-rec q4 (append ans (list (item question "  -> Anakin Skywalker\n"))))
              ]
              [(equal? answer 2)
                (console-survey-rec q5 (append ans (list (item question "  -> Luke Skywalker\n"))))
              ]
              [else
                (error 'console-survey "Invalid answer entered. Try again.")
              ]
            )         
          )
        ]
        [(equal? i q4)
          (let ([answer (begin (display-item i) (s-exp->number (read)))])
            (cond
              [(equal? answer 1)
                (console-survey-rec end (append ans (list (item question "  -> his abilities\n"))))
              ]
              [(equal? answer 2)
                (console-survey-rec end (append ans (list (item question "  -> his pride in his abilities\n"))))
              ]
              [(equal? answer 3)
                (console-survey-rec end (append ans (list (item question "  -> his relationship with Padme\n"))))
              ]
              [(equal? answer 4)
                (console-survey-rec end (append ans (list (item question "  -> his attachment to temporary things\n"))))
              ]
              [(equal? answer 5)
                (console-survey-rec end (append ans (list (item question (get-custom-answer)))))
              ]
              [else
                (error 'console-survey "Invalid answer entered. Try again.")
              ]
            )         
          )
        ]
        [(equal? i q5)
          (let ([answer (begin (display-item i) (s-exp->number (read)))])
            (cond
              [(equal? answer 1)
                (console-survey-rec q6 (append ans (list (item question "  -> his hopefulness\n"))))
              ]
              [(equal? answer 2)
                (console-survey-rec q7 (append ans (list (item question "  -> his loyalty\n"))))
              ]
              [else
                (error 'console-survey "Invalid answer entered. Try again.")
              ]
            )         
          )
        ]
        [(equal? i q6)
          (let ([answer (begin (display-item i) (s-exp->number (read)))])
            (cond
              [(equal? answer 1)
                (console-survey-rec end (append ans (list (item question "  -> the pressure of building a new Jedi Order\n"))))
              ]
              [(equal? answer 2)
                (console-survey-rec end (append ans (list (item question "  -> unresolved fears about the Dark Side\n"))))
              ]
              [(equal? answer 3)
                (console-survey-rec end (append ans (list (item question "  -> disillusionment with the history of the Jedi Order\n"))))
              ]
              [(equal? answer 4)
                (console-survey-rec end (append ans (list (item question "  -> new knowledge about midichlorians\n"))))
              ]
              [(equal? answer 5)
                (console-survey-rec end (append ans (list (item question (get-custom-answer)))))
              ]
              [else
                (error 'console-survey "Invalid answer entered. Try again.")
              ]
            )         
          )
        ]
        [(equal? i q7)
          (let ([answer (begin (display-item i) (s-exp->number (read)))])
            (cond
              [(equal? answer 1)
                (console-survey-rec end (append ans (list (item question "  -> his father\n"))))
              ]
              [(equal? answer 2)
                (console-survey-rec end (append ans (list (item question "  -> Obi Wan\n"))))
              ]
              [(equal? answer 3)
                (console-survey-rec end (append ans (list (item question "  -> Han and Leia\n"))))
              ]
              [(equal? answer 4)
                (console-survey-rec end (append ans (list (item question "  -> Yoda\n"))))
              ]
              [(equal? answer 5)
                (console-survey-rec end (append ans (list (item question (get-custom-answer)))))
              ]
              [else
                (error 'console-survey "Invalid answer entered. Try again.")
              ]
            )         
          )
        ]
        [else
          (begin (display "\nHere are your answers:\n") (display-all ans))
        ]
      )
    ]
  )
)

(console-survey)