#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "OR"
    (test-case-RR "or" #x16)
    (test-case-RRE "ogr" #xB9 #x81)
    (test-case-RXY-a "og" #xE3 #x81 #:data-size (data-size 8))))

(module+ test
  (time (run-tests tests)))
