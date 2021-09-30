#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "ROTATE LEFT SINGLE LOGICAL"
    (test-case-RSY-a "rll" #xEB #x1D)
    (test-case-RSY-a "rllg" #xEB #x1C)))

(module+ test
  (time (run-tests tests)))
