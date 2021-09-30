#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "ADD LOGICAL IMMEDIATE"
    (test-case-RIL-a "algfi" #xC2 #xA)
    (test-case-RIL-a "alfi" #xC2 #xB)))

(module+ test
  (time (run-tests tests)))
