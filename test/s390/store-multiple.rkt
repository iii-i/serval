#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "STORE MULTIPLE"
    (test-case-RSY-a "stmg" #xEB #x24 #:data-size (data-size-multiple 8))))

(module+ test
  (time (run-tests tests)))
