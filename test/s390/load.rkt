#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD"
    (test-case-RR "lr" #x18)
    (test-case-RRE "lgr" #xB9 #x04)
    (test-case-RXY-a "lg" #xE3 #x04 #:data-size (data-size 8))))

(module+ test
  (time (run-tests tests)))
