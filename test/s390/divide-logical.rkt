#lang errortrace rosette

(require
  "lib.rkt")

(define tests
  (test-suite+ "DIVIDE LOGICAL"
    (test-case-RRE "dlgr" #xB9 #x87)
    (test-case-RRE "dlr" #xB9 #x97)
    (test-case-RXY-a "dlg" #xE3 #x87 #:data-size (data-size 8))
    (test-case-RXY-a "dl" #xE3 #x97 #:data-size (data-size 4))))

(module+ test
  (time (run-tests tests)))
