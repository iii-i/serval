#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "SHIFT RIGHT DOUBLE"
    (test-case-RS-a "srda" #x8E)))

(module+ test
  (time (run-tests tests)))
