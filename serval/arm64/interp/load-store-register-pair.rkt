#lang rosette

(require
  "common.rkt")

(provide
  stp-preindex)

(define (decode-stp opc imm7 Rt2 Rn Rt)
  (define n Rn)
  (define t Rt)
  (define t2 Rt2)
  (define scale
    (cond
      [(equal? opc (bv #b00 2)) (bv 2 64)]
      [(equal? opc (bv #b10 2)) (bv 3 64)]
      [else (undefined)]))
  (define datasize (bvshl (bv 8 64) scale))
  (define offset (bvshl (sign-extend imm7 (bitvector 64)) scale))
  (values n t t2 scale datasize offset))

(define ((interpret-stp wback postindex) cpu opc imm7 Rt2 Rn Rt)
  (define-values (n t t2 scale datasize offset) (decode-stp opc imm7 Rt2 Rn Rt))
  (define mm (cpu-memmgr cpu))

  (define dbytes (bvudiv datasize (bv 8 64)))

  (when (and wback (or (equal? t n) (equal? t2 n)) (not (equal? n (integer->gpr 31))))
    (undefined))

  (define address #f)
  (cond
    [(equal? n (integer->gpr 31))
      (check-sp-alignment cpu)
      (set! address (cpu-sp-ref cpu))]
    [else
      (cpu-gpr-ref cpu n)])

  (when (! postindex)
    (set! address (bvadd address offset)))

  (define data1 (cpu-gpr-ref cpu t))
  (define data2 (cpu-gpr-ref cpu t2))

  (core:memmgr-store! mm address (bv 0 64) data1 dbytes #:dbg #f)
  (core:memmgr-store! mm (bvadd address dbytes) (bv 0 64) data2 dbytes #:dbg #f)

  (when wback
    (when postindex
      (set! address (bvadd address offset)))
    (if (equal? n (integer->gpr 31))
        (cpu-sp-set! cpu address)
        (cpu-gpr-set! cpu n address)))

  (void))

(define-insn (opc imm7 Rt2 Rn Rt)
  #:encode (lambda (V opc2 L) (list opc (bv #b101 3) (bv V 1) (bv opc2 3) (bv L 1) imm7 Rt2 Rn Rt))
  [(#b0 #b011 #b0) stp-preindex (interpret-stp #t #f)])