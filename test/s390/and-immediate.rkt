#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "AND IMMEDIATE"
    (test-case-RIL-a "nilf" #xC0 #xB)))

(module+ test
  (time (run-tests tests)))
