#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD ADDRESS RELATIVE LONG"
    (test-case-RIL-b "larl" #xC0 #x0)))

(module+ test
  (time (run-tests tests)))
