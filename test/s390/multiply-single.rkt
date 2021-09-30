#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "MULTIPLY SINGLE"
    (test-case-RRE "msr" #xB2 #x52)
    (test-case-RRE "msgr" #xB9 #x0C)))

(module+ test
  (time (run-tests tests)))
