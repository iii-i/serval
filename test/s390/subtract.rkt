#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "SUBTRACT"
    (test-case-RR "sr" #x1B)
    (test-case-RRE "sgr" #xB9 #x09)))

(module+ test
  (time (run-tests tests)))
