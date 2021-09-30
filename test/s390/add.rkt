#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "ADD"
    (test-case-RR "ar" #x1A)
    (test-case-RRE "agr" #xB9 #x08)))

(module+ test
  (time (run-tests tests)))
