#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "EXCLUSIVE OR"
    (test-case-RR "xr" #x17)
    (test-case-RRE "xgr" #xB9 #x82)
    (test-case-SS-a "xc" #xD7)
    (test-case-RXY-a "xg" #xE3 #x82 #:data-size (data-size 8))))

(module+ test
  (time (run-tests tests)))
