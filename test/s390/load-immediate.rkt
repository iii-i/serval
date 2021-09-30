#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD IMMEDIATE"
    (test-case-RIL-a "lgfi" #xC0 #x1)))

(module+ test
  (time (run-tests tests)))
