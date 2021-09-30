#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD REVERSED"
    (test-case-RRE "lrvr" #xB9 #x1F)
    (test-case-RRE "lrvgr" #xB9 #x0F)))

(module+ test
  (time (run-tests tests)))
