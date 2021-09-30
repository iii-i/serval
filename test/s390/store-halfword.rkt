#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "STORE HALFWORD"
    (test-case-RX-a "sth" #x40 #:data-size (data-size 2))
    (test-case-RXY-a "sthy" #xE3 #x70 #:data-size (data-size 2))))

(module+ test
  (time (run-tests tests)))
