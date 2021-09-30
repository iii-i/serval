#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "BRANCH RELATIVE AND SAVE LONG"
    (test-case-RIL-b "brasl" #xC0 #x5)))

(module+ test
  (time (run-tests tests)))
