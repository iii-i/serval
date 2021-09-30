#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "SHIFT RIGHT SINGLE"
    (test-case-RS-a "sra" #x8A)
    (test-case-RSY-a "srak" #xEB #xDC)
    (test-case-RSY-a "srag" #xEB #x0A)))

(module+ test
  (time (run-tests tests)))
