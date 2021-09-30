#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "AND"
    (test-case-RR "nr" #x14)
    (test-case-RRE "ngr" #xB9 #x80)
    (test-case-RXY-a "ng" #xE3 #x80 #:data-size (data-size 8))))

(module+ test
  (time (run-tests tests)))
