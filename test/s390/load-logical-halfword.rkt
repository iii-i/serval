#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD LOGICAL HALFWORD"
    (test-case-RRE "llghr" #xB9 #x85)
    (test-case-RXY-a "llgh" #xE3 #x91 #:data-size (data-size 2))))

(module+ test
  (time (run-tests tests)))
