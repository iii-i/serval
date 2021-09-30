#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "SHIFT RIGHT DOUBLE LOGICAL"
    (test-case-RS-a "srdl" #x8C)))

(module+ test
  (time (run-tests tests)))
