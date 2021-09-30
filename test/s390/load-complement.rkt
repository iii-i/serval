#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD COMPLEMENT"
    (test-case-RR "lcr" #x13)
    (test-case-RRE "lcgr" #xB9 #x03)))

(module+ test
  (time (run-tests tests)))
