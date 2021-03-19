#lang web-server/insta
(require
  web-server/http/xexpr
  web-server/servlet/web
  "survey.rkt"
  "backend.rkt"
)
 
(define (start req)
  (run-survey)
)
