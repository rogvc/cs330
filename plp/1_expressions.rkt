#lang plait

; 1. expressions using +, -, *, and /
(+ 1 2)
(- 3 4)
(* 4 5)
(/ 6 7)

; 2. expressions using string-append and substring
(string-append "Hello " "World")
(substring "Wow" 1 2)

; 3. expression using let
(let ([my-name "Rogerio"])
  (string-append my-name " Cruz"))

; 4. expression using list
(list "laptop" "headphones" "cellphone" "switch")

; 5. expression using <, >, or =
(< 5 4)

; 6. expressions using and, or, and not
(and #t (> 2 1))
(or #f #t)
(not #f)

; 7. expressions using if and cond
(if (> 10 0) "positive" "negative")
(cond 
 [(> -5 0) "positive"]
 [(< -5 0) "negative"])