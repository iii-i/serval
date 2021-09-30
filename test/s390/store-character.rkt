#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "STORE CHARACTER"
    (test-case-RX-a "stc" #x42 #:data-size (data-size 1))
    (test-case-RXY-a "stcy" #xE3 #x72 #:data-size (data-size 1))))

(module+ test
  (time (run-tests tests)))
