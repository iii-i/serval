#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD HALFWORD IMMEDIATE"
    (test-case-RI-a "lhi" #xA7 #x8)
    (test-case-RI-a "lghi" #xA7 #x9)))

(module+ test
  (time (run-tests tests)))
