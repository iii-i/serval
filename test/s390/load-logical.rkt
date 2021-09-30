#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD LOGICAL"
    (test-case-RRE "llgfr" #xB9 #x16)
    (test-case-RXY-a "llgf" #xE3 #x16 #:data-size (data-size 4))))

(module+ test
  (time (run-tests tests)))
