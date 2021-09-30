#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "MULTIPLY SINGLE IMMEDIATE"
    (test-case-RIL-a "msgfi" #xC2 #x0)
    (test-case-RIL-a "msfi" #xC2 #x1)))

(module+ test
  (time (run-tests tests)))
