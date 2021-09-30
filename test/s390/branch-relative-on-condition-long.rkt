#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "BRANCH RELATIVE ON CONDITION LONG"
    (test-case-RIL-c "brc" #xC0 #x4)))

(module+ test
  (time (run-tests tests)))
