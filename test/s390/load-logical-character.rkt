#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "LOAD LOGICAL CHARACTER"
    (test-case-RXY-a "llgc" #xE3 #x90 #:data-size (data-size 1))))

(module+ test
  (time (run-tests tests)))
