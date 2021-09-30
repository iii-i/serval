#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "ADD IMMEDIATE"
    (test-case-RIL-a "agfi" #xC2 #x8)))

(module+ test
  (time (run-tests tests)))
