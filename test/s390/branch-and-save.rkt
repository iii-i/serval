#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "BRANCH AND SAVE"
    (test-case-RR "basr" #x0D)))

(module+ test
  (time (run-tests tests)))
