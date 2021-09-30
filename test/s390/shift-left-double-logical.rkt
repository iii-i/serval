#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "SHIFT LEFT DOUBLE LOGICAL"
    (test-case-RS-a "sldl" #x8D)))

(module+ test
  (time (run-tests tests)))
