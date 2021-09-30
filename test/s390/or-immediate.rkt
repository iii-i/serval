#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "OR IMMEDIATE"
    (test-case-RIL-a "oilf" #xC0 #xD)))

(module+ test
  (time (run-tests tests)))
