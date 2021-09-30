#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "SHIFT RIGHT SINGLE LOGICAL"
    (test-case-RS-a "srl" #x88)
    (test-case-RSY-a "srlk" #xEB #xDE)
    (test-case-RSY-a "srlg" #xEB #x0C)))

(module+ test
  (time (run-tests tests)))
