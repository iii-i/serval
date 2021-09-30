#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD RELATIVE LONG"
    (test-case-RIL-b "lgrl" #xC4 #x8 #:data-size (data-size 8))
    (test-case-RIL-b "lgfrl" #xC4 #xC #:data-size (data-size 4))))

(module+ test
  (time (run-tests tests)))
