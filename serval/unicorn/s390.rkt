#lang racket/base

(require
  ffi/unsafe
  "const/s390.rkt"
  "engine.rkt")

(provide
  s390-engine)

(struct s390-engine (ptr mode)
  #:methods gen:engine
  [(define (engine-ptr engine)
     (s390-engine-ptr engine))
   (define (engine-reg-enum engine)
     _uc_s390x_reg)
   (define (engine-reg-type engine reg)
     _uint64)])
