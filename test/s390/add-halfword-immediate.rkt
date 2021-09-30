#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "ADD HALFWORD IMMEDIATE"
    (test-case-RI-a "aghi" #xA7 #xB)))

(module+ test
  (time (run-tests tests)))
