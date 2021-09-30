#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "SHIFT LEFT SINGLE"
    (test-case-RS-a "sla" #x8B)
    (test-case-RSY-a "slag" #xEB #x0B)
    (test-case-RSY-a "slak" #xEB #xDD)))

(module+ test
  (time (run-tests tests)))
