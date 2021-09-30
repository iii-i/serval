#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "BRANCH RELATIVE ON CONDITION"
    (test-case-RI-c "brc" #xA7 #x4)))

(module+ test
  (time (run-tests tests)))
