#lang rosette

(require (prefix-in core: serval/lib/core))

(core:target-pointer-bitwidth 64)

(define (check-read-write-bv64)

  (define memmgr (core:make-flat-memmgr))
  (define-symbolic* addr (bitvector 64))
  (define-symbolic* off (bitvector 64))
  (define-symbolic* data (bitvector 64))

  (core:memmgr-store! memmgr addr off data (bv 8 64))

  (define readdata (core:memmgr-load memmgr addr off (bv 8 64)))

  (core:check-unsat? (verify (assert (bveq data readdata)))))

(define (check-big-endian)

  (define memmgr (core:make-flat-memmgr #:big-endian #t))
  (define-symbolic* addr (bitvector 64))
  (define-symbolic* off (bitvector 64))
  (define-symbolic* b0 b1 (bitvector 8))

  (core:memmgr-store! memmgr addr off (concat b0 b1) (bv 2 64))

  (define b0r (core:memmgr-load memmgr addr off (bv 1 64)))
  (define b1r (core:memmgr-load memmgr addr (bvadd1 off) (bv 1 64)))

  (core:check-unsat? (verify (assert (and (bveq b0 b0r) (bveq b1 b1r))))))

(define flat-memory-tests
  (core:test-suite+
   "Tests for flat memory model"
    (core:test-case+ "Check r/w bv64" (check-read-write-bv64))
    (core:test-case+ "Check big-endian support" (check-big-endian))
  ))

(module+ test
  (time (core:run-tests flat-memory-tests)))