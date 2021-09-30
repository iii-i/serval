#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "BRANCH ON CONDITION"
    (test-case-RR "bcr" #x07)))

(module+ test
  (time (run-tests tests)))
