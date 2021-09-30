#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "SHIFT LEFT DOUBLE"
    (test-case-RS-a "slda" #x8F)))

(module+ test
  (time (run-tests tests)))
