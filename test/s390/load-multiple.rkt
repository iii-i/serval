#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD MULTIPLE"
    (test-case-RSY-a "lmg" #xEB #x04 #:data-size (data-size-multiple 8))))

(module+ test
  (time (run-tests tests)))
