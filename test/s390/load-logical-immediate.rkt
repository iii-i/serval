#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD LOGICAL IMMEDIATE"
    (test-case-RIL-a "llilf" #xC0 #xF)))

(module+ test
  (time (run-tests tests)))
