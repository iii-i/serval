#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD ADDRESS"
    (test-case-RX-a "la" #x41)))

(module+ test
  (time (run-tests tests)))
