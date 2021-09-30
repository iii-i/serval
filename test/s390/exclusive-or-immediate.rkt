#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "EXCLUSIVE OR IMMEDIATE"
    (test-case-RIL-a "xilf" #xC0 #x7)))

(module+ test
  (time (run-tests tests)))
