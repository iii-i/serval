#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "STORE"
    (test-case-RX-a "st" #x50 #:data-size (data-size 4))
    (test-case-RXY-a "stg" #xE3 #x24 #:data-size (data-size 8))
    (test-case-RXY-a "sty" #xE3 #x50 #:data-size (data-size 4))))

(module+ test
  (time (run-tests tests)))
