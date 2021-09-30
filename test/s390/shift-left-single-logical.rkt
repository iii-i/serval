#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "SHIFT LEFT SINGLE LOGICAL"
    (test-case-RS-a "sll" #x89)
    (test-case-RSY-a "sllk" #xEB #xDF)
    (test-case-RSY-a "sllg" #xEB #x0D)))

(module+ test
  (time (run-tests tests)))
