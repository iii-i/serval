#lang rosette

(require
  "../lib/bvarith.rkt"
  "../lib/memory/manager.rkt"
  "base.rkt"
  serval/lib/debug)

(provide decode-one)

(define (cpu-gpr-bv cpu r) (vector-ref-bv (cpu-gprs cpu) r))
(define (set-cpu-gpr!-bv cpu r v) (vector-set!-bv (cpu-gprs cpu) r v))

(define (cpu-gpr-pair-bv cpu r)
  (bug-assert (bvzero? (lsb r)))
  (concat (cpu-gpr-bv cpu r) (cpu-gpr-bv cpu (bvadd1 r))))

(define (cpu-gpr32-bv cpu r) (extract 31 0 (cpu-gpr-bv cpu r)))
(define (cpu-gpr32high-bv cpu r) (extract 63 32 (cpu-gpr-bv cpu r)))
(define (set-cpu-gpr32!-bv cpu r v)
  (set-cpu-gpr!-bv cpu r (concat (cpu-gpr32high-bv cpu r) v)))

(define (cpu-gpr32-pair-bv cpu r)
  (bug-assert (bvzero? (lsb r)))
  (concat (cpu-gpr32-bv cpu r) (cpu-gpr32-bv cpu (bvadd1 r))))
(define (set-cpu-gpr32-pair!-bv cpu r v)
  (bug-assert (bvzero? (lsb r)))
  (set-cpu-gpr32!-bv cpu r (extract 63 32 v))
  (set-cpu-gpr32!-bv cpu (bvadd1 r) (extract 31 0 v)))

(define (mul-single x y)
  (define lo ((bvmul-proc) x y))
  (define hi ((bvmulh-proc) x y))
  (values
    lo
    (cond
      [(bvsmul-overflow? x y) (bv 3 2)]
      [(bvzero? hi) (if (bvzero? lo) (bv 0 2) (bv 2 2))]
      [else (bv 1 2)])))

(define (divide-logical-32 cpu r1 _divisor)
  (bug-assert (bvzero? (lsb r1)))
  (define dividend (cpu-gpr32-pair-bv cpu r1))
  (define divisor (zero-extend _divisor (bitvector 64)))
  (bug-assert (not (bvzero? divisor)))
  (define quotient ((bvudiv-proc) dividend divisor))
  (bug-assert (bvzero? (extract 63 32 quotient)))
  (set-cpu-gpr32!-bv cpu r1 (extract 31 0 ((bvurem-proc) dividend divisor)))
  (set-cpu-gpr32!-bv cpu (bvadd1 r1) (extract 31 0 quotient)))

(define (divide-logical-64 cpu r1 _divisor)
  (bug-assert (bvzero? (lsb r1)))
  (define dividend (cpu-gpr-pair-bv cpu r1))
  (define divisor (zero-extend _divisor (bitvector 128)))
  (bug-assert (not (bvzero? divisor)))
  (define quotient ((bvudiv-proc) dividend divisor))
  (bug-assert (bvzero? (extract 127 64 quotient)))
  (set-cpu-gpr!-bv cpu r1 (extract 63 0 ((bvurem-proc) dividend divisor)))
  (set-cpu-gpr!-bv cpu (bvadd1 r1) (extract 63 0 quotient)))

(define (displacement-base cpu d b extend)
  (define d64 (extend d (bitvector 64)))
  (if (bvzero? b) d64 (bvadd (cpu-gpr-bv cpu b) d64)))

(define (displacement-index-base cpu d x b extend)
  (define db64 (displacement-base cpu d b extend))
  (if (bvzero? x) db64 (bvadd (cpu-gpr-bv cpu x) db64)))

(define (relative cpu ri)
  (bvadd (cpu-pswa cpu) (sign-extend (concat ri (bv 0 1)) (bitvector 64))))

;
; Condition code handling.
; Names correspond to gcc's gcc/config/s390/s390-modes.def.
;

(define (set-cpu-CCL1! cpu sum)
  (set-cpu-cc!
    cpu
    (concat
      (msb sum)
      (if (bvzero? (extract (- (bitvector-size (type-of sum)) 2) 0 sum)) (bv 0 1) (bv 1 1)))))

(define (set-cpu-CCO! cpu x)
  (define size (bv-size x))
  (define sign (extract (- size 1) (- size 2) x))
  (set-cpu-cc!
    cpu
    (cond
      [(bvzero? x) (bv 0 2)]
      [(bveq sign (bv #b11 2)) (bv 1 2)]
      [(bveq sign (bv #b00 2)) (bv 2 2)]
      [else (bv 3 2)])))

(define (set-cpu-CCS! cpu result)
  (set-cpu-cc!
    cpu
    (cond
      [(bvzero? result) (bv 0 2)]
      [(bvzero? (msb result)) (bv 2 2)]
      [else (bv 1 2)])))

(define (set-cpu-CCT! cpu result)
  (set-cpu-cc! cpu (if (bvzero? result) (bv 0 2) (bv 1 2))))

;
; Instruction encodings.
;

(struct RI-a (r1 i2) #:transparent)

(define (is-RI-a? fetch opcode1 opcode2)
  (and
    (bveq (fetch 0) (bv opcode1 8))
    (bveq (extract 3 0 (fetch 1)) (bv opcode2 4))))

(define (parse-RI-a fetch ctor)
  (ctor
    (extract 7 4 (fetch 1))
    (concat (fetch 2) (fetch 3))))

(define (encode-RI-a opcode1 opcode2 insn)
  (match-define (RI-a r1 i2) insn)
  (vector
    (bv opcode1 8)
    (concat r1 (bv opcode2 4))
    (extract 15 8 i2)
    (extract 7 0 i2)))

(struct RI-c RI-a () #:transparent)
(define is-RI-c? is-RI-a?)
(define parse-RI-c parse-RI-a)
(define encode-RI-c encode-RI-a)

(struct RIL-a (r1 i2) #:transparent)

(define (is-RIL-a? fetch opcode1 opcode2)
  (and
    (bveq (fetch 0) (bv opcode1 8))
    (bveq (extract 3 0 (fetch 1)) (bv opcode2 4))))

(define (parse-RIL-a fetch ctor)
  (ctor
    (extract 7 4 (fetch 1))
    (concat (fetch 2) (fetch 3) (fetch 4) (fetch 5))))

(define (encode-RIL-a opcode1 opcode2 insn)
  (match-define (RIL-a r1 i2) insn)
  (vector
    (bv opcode1 8)
    (concat r1 (bv opcode2 4))
    (extract 31 24 i2)
    (extract 23 16 i2)
    (extract 15 8 i2)
    (extract 7 0 i2)))

(struct RIL-b RIL-a () #:transparent)
(define is-RIL-b? is-RIL-a?)
(define parse-RIL-b parse-RIL-a)
(define encode-RIL-b encode-RIL-a)

(struct RIL-c RIL-a () #:transparent)
(define is-RIL-c? is-RIL-a?)
(define parse-RIL-c parse-RIL-a)
(define encode-RIL-c encode-RIL-a)

(struct RR (r1 r2) #:transparent)

(define (is-RR? fetch opcode)
  (bveq (fetch 0) (bv opcode 8)))

(define (parse-RR fetch ctor)
  (define r1r2 (fetch 1))
  (ctor (extract 7 4 r1r2) (extract 3 0 r1r2)))

(define (encode-RR opcode insn)
  (match-define (RR r1 r2) insn)
  (vector (bv opcode 8) (concat r1 r2)))

(struct RRE (r1 r2) #:transparent)

(define (is-RRE? fetch opcode1 opcode2)
  (and
    (bveq (fetch 0) (bv opcode1 8))
    (bveq (fetch 1) (bv opcode2 8))
    (bveq (fetch 2) (bv #x00 8))))

(define (parse-RRE fetch ctor)
  (define r1r2 (fetch 3))
  (ctor (extract 7 4 r1r2) (extract 3 0 r1r2)))

(define (encode-RRE opcode1 opcode2 insn)
  (match-define (RRE r1 r2) insn)
  (vector (bv opcode1 8) (bv opcode2 8) (bv #x00 8) (concat r1 r2)))

(struct RS-a (r1 r3 d2 b2) #:transparent)

(define (is-RS-a? fetch opcode)
  (bveq (fetch 0) (bv opcode 8)))

(define (parse-RS-a fetch ctor)
  (define r1r3 (fetch 1))
  (define b2dh (fetch 2))
  (ctor
    (extract 7 4 r1r3)
    (extract 3 0 r1r3)
    (bvadd
      (bvshl (zero-extend (extract 3 0 b2dh) (bitvector 12)) (bv 8 12))
      (zero-extend (fetch 3) (bitvector 12)))
    (extract 7 4 b2dh)))

(define (encode-RS-a opcode insn)
  (match-define (RS-a r1 r3 d2 b2) insn)
  (vector
    (bv opcode 8)
    (concat r1 r3)
    (concat b2 (extract 11 8 d2))
    (extract 7 0 d2)))

(struct RSY-a (r1 r3 d2 b2) #:transparent)

(define (is-RSY-a? fetch opcode1 opcode2)
  (and
    (bveq (fetch 0) (bv opcode1 8))
    (bveq (fetch 5) (bv opcode2 8))))

(define (parse-RSY-a fetch ctor)
  (define r1r3 (fetch 1))
  (define b2dlh (fetch 2))
  (ctor
    (extract 7 4 r1r3)
    (extract 3 0 r1r3)
    (concat (fetch 4) (extract 3 0 b2dlh) (fetch 3))
    (extract 7 4 b2dlh)))

(define (encode-RSY-a opcode1 opcode2 insn)
  (match-define (RSY-a r1 r3 d2 b2) insn)
  (vector
    (bv opcode1 8)
    (concat r1 r3)
    (concat b2 (extract 11 8 d2))
    (extract 7 0 d2)
    (extract 19 12 d2)
    (bv opcode2 8)))

(struct RX-a RS-a () #:transparent)
(define is-RX-a? is-RS-a?)
(define parse-RX-a parse-RS-a)
(define encode-RX-a encode-RS-a)

(struct RXY-a RSY-a () #:transparent)
(define is-RXY-a? is-RSY-a?)
(define parse-RXY-a parse-RSY-a)
(define encode-RXY-a encode-RSY-a)

(struct SS-a (d1 l1 b1 d2 b2) #:transparent)

(define (is-SS-a? fetch opcode)
  (bveq (fetch 0) (bv opcode 8)))

(define (parse-SS-a fetch ctor)
  (define b1dh (fetch 2))
  (define b2dh (fetch 4))
  (ctor
    (concat (extract 3 0 b1dh) (fetch 3))
    (fetch 1)
    (extract 7 4 b1dh)
    (concat (extract 3 0 b2dh) (fetch 5))
    (extract 7 4 b2dh)))

(define (encode-SS-a opcode insn)
  (match-define (SS-a d1 l1 b1 d2 b2) insn)
  (vector
    (bv opcode 8)
    l1
    (concat b1 (extract 11 8 d1))
    (extract 7 0 d1)
    (concat b2 (extract 11 8 d2))
    (extract 7 0 d2)))

;
; Instructions.
;

(struct bcr RR () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RR #x07))
   (define (instruction-run insn cpu)
     (match-define (bcr m1 r2) insn)
     (cond
       [(or
          (bvzero? r2)
          (bvzero? (bvand m1 (bvlshr (bv 8 4) (zero-extend (cpu-cc cpu) (bitvector 4))))))
        #t]
       [else
        (set-cpu-pswa! cpu (cpu-gpr-bv cpu r2))
        #f]))])

(struct basr RR () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RR #x0D))
   (define (instruction-run insn cpu)
     (match-define (basr r1 r2) insn)
     (cond
       [(bvzero? r2)
        (set-cpu-gpr!-bv cpu r1 (bvadd (cpu-pswa cpu) (bv 2 64)))
        #t]
       [else
        (define branch-address (cpu-gpr-bv cpu r2))
        (set-cpu-gpr!-bv cpu r1 (bvadd (cpu-pswa cpu) (bv 2 64)))
        (set-cpu-pswa! cpu branch-address)
        #f]))])

(struct lcr RR () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RR #x13))
   (define (instruction-run insn cpu)
     (match-define (lcr r1 r2) insn)
     (define result
       (bvsub
         (bv 0 (bitvector 33))
         (sign-extend (cpu-gpr32-bv cpu r2) (bitvector 33))))
     (set-cpu-gpr32!-bv cpu r1 (extract 31 0 result))
     (set-cpu-CCO! cpu result)
     #t)])

(struct nr RR () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RR #x14))
   (define (instruction-run insn cpu)
     (match-define (nr r1 r2) insn)
     (define result (bvand (cpu-gpr32-bv cpu r1) (cpu-gpr32-bv cpu r2)))
     (set-cpu-gpr32!-bv cpu r1 result)
     (set-cpu-CCT! cpu result)
     #t)])

(struct _or RR () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RR #x16))
   (define (instruction-run insn cpu)
     (match-define (_or r1 r2) insn)
     (define result (bvor (cpu-gpr32-bv cpu r1) (cpu-gpr32-bv cpu r2)))
     (set-cpu-gpr32!-bv cpu r1 result)
     (set-cpu-CCT! cpu result)
     #t)])

(struct xr RR () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RR #x17))
   (define (instruction-run insn cpu)
     (match-define (xr r1 r2) insn)
     (define result (bvxor (cpu-gpr32-bv cpu r1) (cpu-gpr32-bv cpu r2)))
     (set-cpu-gpr32!-bv cpu r1 result)
     (set-cpu-CCT! cpu result)
     #t)])

(struct lr RR () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RR #x18))
   (define (instruction-run insn cpu)
     (match-define (lr r1 r2) insn)
     (set-cpu-gpr32!-bv cpu r1 (cpu-gpr32-bv cpu r2))
     #t)])

(struct ar RR () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RR #x1A))
   (define (instruction-run insn cpu)
     (match-define (ar r1 r2) insn)
     (define sum
       (bvadd
         (sign-extend (cpu-gpr32-bv cpu r1) (bitvector 33))
         (sign-extend (cpu-gpr32-bv cpu r2) (bitvector 33))))
     (set-cpu-gpr32!-bv cpu r1 (extract 31 0 sum))
     (set-cpu-CCO! cpu sum)
     #t)])

(struct sr RR () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RR #x1B))
   (define (instruction-run insn cpu)
     (match-define (sr r1 r2) insn)
     (define diff
       (bvsub
         (sign-extend (cpu-gpr32-bv cpu r1) (bitvector 33))
         (sign-extend (cpu-gpr32-bv cpu r2) (bitvector 33))))
     (set-cpu-gpr32!-bv cpu r1 (extract 31 0 diff))
     (set-cpu-CCO! cpu diff)
     #t)])

(struct sth RX-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RX-a #x40))
   (define (instruction-run insn cpu)
     (match-define (sth r1 x2 d2 b2) insn)
     (memmgr-store!
       (cpu-memmgr cpu)
       (displacement-index-base cpu d2 x2 b2 zero-extend)
       (bv 0 64)
       (extract 15 0 (cpu-gpr-bv cpu r1))
       (bv 2 64))
     #t)])

(struct la RX-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RX-a #x41))
   (define (instruction-run insn cpu)
     (match-define (la r1 x2 d2 b2) insn)
     (set-cpu-gpr!-bv cpu r1 (displacement-index-base cpu d2 x2 b2 zero-extend))
     #t)])

(struct stc RX-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RX-a #x42))
   (define (instruction-run insn cpu)
     (match-define (stc r1 x2 d2 b2) insn)
     (memmgr-store!
       (cpu-memmgr cpu)
       (displacement-index-base cpu d2 x2 b2 zero-extend)
       (bv 0 64)
       (extract 7 0 (cpu-gpr-bv cpu r1))
       (bv 1 64))
     #t)])

(struct st RX-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RX-a #x50))
   (define (instruction-run insn cpu)
     (match-define (st r1 x2 d2 b2) insn)
     (memmgr-store!
       (cpu-memmgr cpu)
       (displacement-index-base cpu d2 x2 b2 zero-extend)
       (bv 0 64)
       (cpu-gpr32-bv cpu r1)
       (bv 4 64))
     #t)])

(struct srl RS-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RS-a #x88))
   (define (instruction-run insn cpu)
     (match-define (srl r1 r3 d2 b2) insn)
     (define result
       (bvlshr
         (cpu-gpr32-bv cpu r1)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 32))))
     (set-cpu-gpr32!-bv cpu r1 result)
     #t)])

(struct sll RS-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RS-a #x89))
   (define (instruction-run insn cpu)
     (match-define (sll r1 r3 d2 b2) insn)
     (define result
       (bvshl
         (cpu-gpr32-bv cpu r1)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 32))))
     (set-cpu-gpr32!-bv cpu r1 result)
     #t)])

(struct sra RS-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RS-a #x8A))
   (define (instruction-run insn cpu)
     (match-define (sra r1 r3 d2 b2) insn)
     (define result
       (bvashr
         (cpu-gpr32-bv cpu r1)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 32))))
     (set-cpu-gpr32!-bv cpu r1 result)
     (set-cpu-CCS! cpu result)
     #t)])

(struct sla RS-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RS-a #x8B))
   (define (instruction-run insn cpu)
     (match-define (sla r1 r3 d2 b2) insn)
     (define op1 (sign-extend (cpu-gpr32-bv cpu r1) (bitvector 95)))
     (define numeric
       (bvshl op1 (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 95))))
     (define result (concat (msb op1) (extract 30 0 numeric)))
     (set-cpu-gpr32!-bv cpu r1 result)
     (set-cpu-cc!
       cpu
       (cond
         [(not (bveq (extract 94 31 op1) (extract 94 31 numeric))) (bv 3 2)]
         [(bvzero? result) (bv 0 2)]
         [(bvzero? (msb result)) (bv 2 2)]
         [else (bv 1 2)]))
     #t)])

(struct srdl RS-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RS-a #x8C))
   (define (instruction-run insn cpu)
     (match-define (srdl r1 r3 d2 b2) insn)
     (define result
       (bvlshr
         (cpu-gpr32-pair-bv cpu r1)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 64))))
     (set-cpu-gpr32-pair!-bv cpu r1 result)
     #t)])

(struct sldl RS-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RS-a #x8D))
   (define (instruction-run insn cpu)
     (match-define (sldl r1 r3 d2 b2) insn)
     (define result
       (bvshl
         (cpu-gpr32-pair-bv cpu r1)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 64))))
     (set-cpu-gpr32-pair!-bv cpu r1 result)
     #t)])

(struct srda RS-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RS-a #x8E))
   (define (instruction-run insn cpu)
     (match-define (srda r1 r3 d2 b2) insn)
     (define result
       (bvashr
         (cpu-gpr32-pair-bv cpu r1)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 64))))
     (set-cpu-gpr32-pair!-bv cpu r1 result)
     (set-cpu-cc!
       cpu
       (cond
         [(bvzero? result) (bv 0 2)]
         [(bvzero? (msb result)) (bv 2 2)]
         [else (bv 1 2)]))
     #t)])

(struct slda RS-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RS-a #x8F))
   (define (instruction-run insn cpu)
     (match-define (slda r1 r3 d2 b2) insn)
     (define op1 (sign-extend (cpu-gpr32-pair-bv cpu r1) (bitvector 127)))
     (define numeric
       (bvshl op1 (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 127))))
     (define result (concat (msb op1) (extract 62 0 numeric)))
     (set-cpu-gpr32-pair!-bv cpu r1 result)
     (set-cpu-cc!
       cpu
       (cond
         [(not (bveq (extract 126 63 op1) (extract 126 63 numeric))) (bv 3 2)]
         [(bvzero? result) (bv 0 2)]
         [(bvzero? (msb result)) (bv 2 2)]
         [else (bv 1 2)]))
     #t)])

(struct brc RI-c () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RI-c #xA7 #x4))
   (define (instruction-run insn cpu)
     (match-define (brc m1 ri2) insn)
     (cond
       [(bvzero? (bvand m1 (bvlshr (bv 8 4) (zero-extend (cpu-cc cpu) (bitvector 4)))))
        #t]
       [else
        (set-cpu-pswa! cpu (relative cpu ri2))
        #f]))])

(struct lhi RI-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RI-a #xA7 #x8))
   (define (instruction-run insn cpu)
     (match-define (lhi r1 i2) insn)
     (set-cpu-gpr32!-bv cpu r1 (sign-extend i2 (bitvector 32)))
     #t)])

(struct lghi RI-a () #:transparent
  #:methods gen:instruction
  [(define (instruction-encode insn) (curry encode-RI-a #xA7 #x9 insn))
   (define (instruction-run insn cpu)
     (match-define (lghi r1 i2) insn)
     (set-cpu-gpr!-bv cpu r1 (sign-extend i2 (bitvector 64)))
     #t)])

(struct aghi RI-a () #:transparent
  #:methods gen:instruction
  [(define (instruction-encode insn) (curry encode-RI-a #xA7 #xB insn))
   (define (instruction-run insn cpu)
     (match-define (aghi r1 i2) insn)
     (define sum
       (bvadd
         (sign-extend (cpu-gpr-bv cpu r1) (bitvector 65))
         (sign-extend i2 (bitvector 65))))
     (set-cpu-gpr!-bv cpu r1 (extract 63 0 sum))
     (set-cpu-CCO! cpu sum)
     #t)])

(struct msr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB2 #x52))
   (define (instruction-run insn cpu)
     (match-define (msr r1 r2) insn)
     (define-values (result cc)
       (mul-single (cpu-gpr32-bv cpu r1) (cpu-gpr32-bv cpu r2)))
     (set-cpu-gpr32!-bv cpu r1 result)
     #t)])

(struct lcgr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x03))
   (define (instruction-run insn cpu)
     (match-define (lcgr r1 r2) insn)
     (define result
       (bvsub
         (bv 0 (bitvector 65))
         (sign-extend (cpu-gpr-bv cpu r2) (bitvector 65))))
     (set-cpu-gpr!-bv cpu r1 (extract 63 0 result))
     (set-cpu-CCO! cpu result)
     #t)])

(struct lgr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x04))
   (define (instruction-run insn cpu)
     (match-define (lgr r1 r2) insn)
     (set-cpu-gpr!-bv cpu r1 (cpu-gpr-bv cpu r2))
     #t)])

(struct agr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x08))
   (define (instruction-run insn cpu)
     (match-define (agr r1 r2) insn)
     (define sum
       (bvadd
         (sign-extend (cpu-gpr-bv cpu r1) (bitvector 65))
         (sign-extend (cpu-gpr-bv cpu r2) (bitvector 65))))
     (set-cpu-gpr!-bv cpu r1 (extract 63 0 sum))
     (set-cpu-CCO! cpu sum)
     #t)])

(struct sgr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x09))
   (define (instruction-run insn cpu)
     (match-define (sgr r1 r2) insn)
     (define diff
       (bvsub
         (sign-extend (cpu-gpr-bv cpu r1) (bitvector 65))
         (sign-extend (cpu-gpr-bv cpu r2) (bitvector 65))))
     (set-cpu-gpr!-bv cpu r1 (extract 63 0 diff))
     (set-cpu-CCO! cpu diff)
     #t)])

(struct msgr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x0C))
   (define (instruction-run insn cpu)
     (match-define (msgr r1 r2) insn)
     (define-values (result cc) (mul-single (cpu-gpr-bv cpu r1) (cpu-gpr-bv cpu r2)))
     (set-cpu-gpr!-bv cpu r1 result)
     #t)])

(struct lrvgr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x0F))
   (define (instruction-run insn cpu)
     (match-define (lrvgr r1 r2) insn)
     (set-cpu-gpr!-bv cpu r1 (list->bitvector/be (bitvector->list/le (cpu-gpr-bv cpu r2))))
     #t)])

(struct llgfr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x16))
   (define (instruction-run insn cpu)
     (match-define (llgfr r1 r2) insn)
     (set-cpu-gpr!-bv cpu r1 (zero-extend (cpu-gpr32-bv cpu r2) (bitvector 64)))
     #t)])

(struct lrvr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x1F))
   (define (instruction-run insn cpu)
     (match-define (lrvr r1 r2) insn)
     (set-cpu-gpr32!-bv cpu r1 (list->bitvector/be (bitvector->list/le (cpu-gpr32-bv cpu r2))))
     #t)])

(struct ngr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x80))
   (define (instruction-run insn cpu)
     (match-define (ngr r1 r2) insn)
     (define result (bvand (cpu-gpr-bv cpu r1) (cpu-gpr-bv cpu r2)))
     (set-cpu-gpr!-bv cpu r1 result)
     (set-cpu-cc! cpu (if (bvzero? result) (bv 0 2) (bv 1 2)))
     #t)])

(struct ogr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x81))
   (define (instruction-run insn cpu)
     (match-define (ogr r1 r2) insn)
     (define result (bvor (cpu-gpr-bv cpu r1) (cpu-gpr-bv cpu r2)))
     (set-cpu-gpr!-bv cpu r1 result)
     (set-cpu-cc! cpu (if (bvzero? result) (bv 0 2) (bv 1 2)))
     #t)])

(struct xgr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x82))
   (define (instruction-run insn cpu)
     (match-define (xgr r1 r2) insn)
     (define result (bvxor (cpu-gpr-bv cpu r1) (cpu-gpr-bv cpu r2)))
     (set-cpu-gpr!-bv cpu r1 result)
     (set-cpu-cc! cpu (if (bvzero? result) (bv 0 2) (bv 1 2)))
     #t)])

(struct llghr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x85))
   (define (instruction-run insn cpu)
     (match-define (llghr r1 r2) insn)
     (set-cpu-gpr!-bv cpu r1 (zero-extend (extract 15 0 (cpu-gpr-bv cpu r2)) (bitvector 64)))
     #t)])

(struct dlgr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x87))
   (define (instruction-run insn cpu)
     (match-define (dlgr r1 r2) insn)
     (divide-logical-64 cpu r1 (cpu-gpr-bv cpu r2))
     #t)])

(struct dlr RRE () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RRE #xB9 #x97))
   (define (instruction-run insn cpu)
     (match-define (dlr r1 r2) insn)
     (divide-logical-32 cpu r1 (cpu-gpr32-bv cpu r2))
     #t)])

(struct larl RIL-b () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-b #xC0 #x0))
   (define (instruction-run insn cpu)
     (match-define (larl r1 ri2) insn)
     (set-cpu-gpr!-bv cpu r1 (relative cpu ri2))
     #t)])

(struct lgfi RIL-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-a #xC0 #x1))
   (define (instruction-run insn cpu)
     (match-define (lgfi r1 i2) insn)
     (set-cpu-gpr!-bv cpu r1 (sign-extend i2 (bitvector 64)))
     #t)])

(struct brcl RIL-c () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-c #xC0 #x4))
   (define (instruction-run insn cpu)
     (match-define (brcl m1 ri2) insn)
     (cond
       [(bvzero? (bvand m1 (bvlshr (bv 8 4) (zero-extend (cpu-cc cpu) (bitvector 4)))))
        #t]
       [else
        (set-cpu-pswa! cpu (relative cpu ri2))
        #f]))])

(struct brasl RIL-b () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-b #xC0 #x5))
   (define (instruction-run insn cpu)
     (match-define (brasl r1 ri2) insn)
     (set-cpu-gpr!-bv cpu r1 (bvadd (cpu-pswa cpu) (bv 6 64)))
     (set-cpu-pswa! cpu (relative cpu ri2))
     #f)])

(struct xilf RIL-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-a #xC0 #x7))
   (define (instruction-run insn cpu)
     (match-define (xilf r1 i2) insn)
     (define result (bvxor (cpu-gpr32-bv cpu r1) i2))
     (set-cpu-gpr32!-bv cpu r1 result)
     (set-cpu-cc! cpu (if (bvzero? result) (bv 0 2) (bv 1 2)))
     #t)])

(struct nilf RIL-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-a #xC0 #xB))
   (define (instruction-run insn cpu)
     (match-define (nilf r1 i2) insn)
     (define result (bvand (cpu-gpr32-bv cpu r1) i2))
     (set-cpu-gpr32!-bv cpu r1 result)
     (set-cpu-cc! cpu (if (bvzero? result) (bv 0 2) (bv 1 2)))
     #t)])

(struct oilf RIL-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-a #xC0 #xD))
   (define (instruction-run insn cpu)
     (match-define (oilf r1 i2) insn)
     (define result (bvor (cpu-gpr32-bv cpu r1) i2))
     (set-cpu-gpr32!-bv cpu r1 result)
     (set-cpu-cc! cpu (if (bvzero? result) (bv 0 2) (bv 1 2)))
     #t)])

(struct llilf RIL-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-a #xC0 #xF))
   (define (instruction-run insn cpu)
     (match-define (llilf r1 i2) insn)
     (set-cpu-gpr!-bv cpu r1 (zero-extend i2 (bitvector 64)))
     #t)])

(struct msgfi RIL-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-a #xC2 #x0))
   (define (instruction-run insn cpu)
     (match-define (msgfi r1 i2) insn)
     (define-values (result cc) (mul-single (cpu-gpr-bv cpu r1) (sign-extend i2 (bitvector 64))))
     (set-cpu-gpr!-bv cpu r1 result)
     #t)])

(struct msfi RIL-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-a #xC2 #x1))
   (define (instruction-run insn cpu)
     (match-define (msfi r1 i2) insn)
     (define-values (result cc) (mul-single (cpu-gpr32-bv cpu r1) i2))
     (set-cpu-gpr32!-bv cpu r1 result)
     #t)])

(struct agfi RIL-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-a #xC2 #x8))
   (define (instruction-run insn cpu)
     (match-define (agfi r1 i2) insn)
     (define sum
       (bvadd
         (sign-extend (cpu-gpr-bv cpu r1) (bitvector 65))
         (sign-extend i2 (bitvector 65))))
     (set-cpu-gpr!-bv cpu r1 (extract 63 0 sum))
     (set-cpu-CCO! cpu sum)
     #t)])

(struct algfi RIL-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-a #xC2 #xA))
   (define (instruction-run insn cpu)
     (match-define (algfi r1 i2) insn)
     (define sum
       (bvadd
         (zero-extend (cpu-gpr-bv cpu r1) (bitvector 65))
         (zero-extend i2 (bitvector 65))))
     (set-cpu-gpr!-bv cpu r1 (extract 63 0 sum))
     (set-cpu-CCL1! cpu sum)
     #t)])

(struct alfi RIL-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-a #xC2 #xB))
   (define (instruction-run insn cpu)
     (match-define (alfi r1 i2) insn)
     (define sum
       (bvadd
         (zero-extend (cpu-gpr32-bv cpu r1) (bitvector 33))
         (zero-extend i2 (bitvector 33))))
     (set-cpu-gpr32!-bv cpu r1 (extract 31 0 sum))
     (set-cpu-CCL1! cpu sum)
     #t)])

(struct lgrl RIL-b () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-b #xC4 #x8))
   (define (instruction-run insn cpu)
     (match-define (lgrl r1 ri2) insn)
     (set-cpu-gpr!-bv
       cpu
       r1
       (memmgr-load (cpu-memmgr cpu) (relative cpu ri2) (bv 0 64) (bv 8 64)))
     #t)])

(struct lgfrl RIL-b () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RIL-b #xC4 #xC))
   (define (instruction-run insn cpu)
     (match-define (lgfrl r1 ri2) insn)
     (set-cpu-gpr!-bv
       cpu
       r1
       (sign-extend
         (memmgr-load (cpu-memmgr cpu) (relative cpu ri2) (bv 0 64) (bv 4 64))
         (bitvector 64)))
     #t)])

(struct xc SS-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-SS-a #xD7))
   (define (instruction-run insn cpu)
     (match-define (xc d1 l1 b1 d2 b2) insn)
     (define addr1 (displacement-base cpu d1 b1 zero-extend))
     (define addr2 (displacement-base cpu d2 b2 zero-extend))
     (define i (bv 0 64))
     (define cc (bv 0 2))
     (define (loop)
       (define val
         (bvxor
           (memmgr-load (cpu-memmgr cpu) addr1 i (bv 1 64))
           (memmgr-load (cpu-memmgr cpu) addr2 i (bv 1 64))))
       (memmgr-store! (cpu-memmgr cpu) addr1 i val (bv 1 64))
       (when (not (bvzero? val))
         (set! cc (bv 1 2)))
       (when (not (bveq i (zero-extend l1 (bitvector 64))))
         (set! i (bvadd1 i))
         (loop)))
     (loop)
     (set-cpu-cc! cpu cc)
     #t)])

(struct lg RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x04))
   (define (instruction-run insn cpu)
     (match-define (lg r1 x2 d2 b2) insn)
     (set-cpu-gpr!-bv
       cpu
       r1
       (memmgr-load
         (cpu-memmgr cpu)
         (displacement-index-base cpu d2 x2 b2 sign-extend)
         (bv 0 64)
         (bv 8 64)))
     #t)])

(struct llgf RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x16))
   (define (instruction-run insn cpu)
     (match-define (llgf r1 x2 d2 b2) insn)
     (set-cpu-gpr!-bv
       cpu
       r1
       (zero-extend
         (memmgr-load
           (cpu-memmgr cpu)
           (displacement-index-base cpu d2 x2 b2 sign-extend)
           (bv 0 64)
           (bv 4 64))
         (bitvector 64)))
     #t)])

(struct stg RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x24))
   (define (instruction-run insn cpu)
     (match-define (stg r1 x2 d2 b2) insn)
     (memmgr-store!
       (cpu-memmgr cpu)
       (displacement-index-base cpu d2 x2 b2 sign-extend)
       (bv 0 64)
       (cpu-gpr-bv cpu r1)
       (bv 8 64))
     #t)])

(struct sty RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x50))
   (define (instruction-run insn cpu)
     (match-define (sty r1 x2 d2 b2) insn)
     (memmgr-store!
       (cpu-memmgr cpu)
       (displacement-index-base cpu d2 x2 b2 sign-extend)
       (bv 0 64)
       (cpu-gpr32-bv cpu r1)
       (bv 4 64))
     #t)])

(struct sthy RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x70))
   (define (instruction-run insn cpu)
     (match-define (sthy r1 x2 d2 b2) insn)
     (memmgr-store!
       (cpu-memmgr cpu)
       (displacement-index-base cpu d2 x2 b2 sign-extend)
       (bv 0 64)
       (extract 15 0 (cpu-gpr-bv cpu r1))
       (bv 2 64))
     #t)])

(struct stcy RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x72))
   (define (instruction-run insn cpu)
     (match-define (stcy r1 x2 d2 b2) insn)
     (memmgr-store!
       (cpu-memmgr cpu)
       (displacement-index-base cpu d2 x2 b2 sign-extend)
       (bv 0 64)
       (extract 7 0 (cpu-gpr-bv cpu r1))
       (bv 1 64))
     #t)])

(struct ng RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x80))
   (define (instruction-run insn cpu)
     (match-define (ng r1 x2 d2 b2) insn)
     (define result
       (bvand (cpu-gpr-bv cpu r1)
       (memmgr-load
         (cpu-memmgr cpu)
         (displacement-index-base cpu d2 x2 b2 sign-extend)
         (bv 0 64)
         (bv 8 64))))
     (set-cpu-gpr!-bv cpu r1 result)
     (set-cpu-cc! cpu (if (bvzero? result) (bv 0 2) (bv 1 2)))
     #t)])

(struct og RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x81))
   (define (instruction-run insn cpu)
     (match-define (og r1 x2 d2 b2) insn)
     (define result
       (bvor (cpu-gpr-bv cpu r1)
       (memmgr-load
         (cpu-memmgr cpu)
         (displacement-index-base cpu d2 x2 b2 sign-extend)
         (bv 0 64)
         (bv 8 64))))
     (set-cpu-gpr!-bv cpu r1 result)
     (set-cpu-cc! cpu (if (bvzero? result) (bv 0 2) (bv 1 2)))
     #t)])

(struct xg RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x82))
   (define (instruction-run insn cpu)
     (match-define (xg r1 x2 d2 b2) insn)
     (define result
       (bvxor (cpu-gpr-bv cpu r1)
       (memmgr-load
         (cpu-memmgr cpu)
         (displacement-index-base cpu d2 x2 b2 sign-extend)
         (bv 0 64)
         (bv 8 64))))
     (set-cpu-gpr!-bv cpu r1 result)
     (set-cpu-cc! cpu (if (bvzero? result) (bv 0 2) (bv 1 2)))
     #t)])

(struct dlg RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x87))
   (define (instruction-run insn cpu)
     (match-define (dlg r1 x2 d2 b2) insn)
     (divide-logical-64
       cpu
       r1
       (memmgr-load
         (cpu-memmgr cpu)
         (displacement-index-base cpu d2 x2 b2 sign-extend)
         (bv 0 64)
         (bv 8 64)))
     #t)])

(struct llgc RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x90))
   (define (instruction-run insn cpu)
     (match-define (llgc r1 x2 d2 b2) insn)
     (set-cpu-gpr!-bv
       cpu
       r1
       (zero-extend
         (memmgr-load
           (cpu-memmgr cpu)
           (displacement-index-base cpu d2 x2 b2 sign-extend)
           (bv 0 64)
           (bv 1 64))
         (bitvector 64)))
     #t)])

(struct llgh RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x91))
   (define (instruction-run insn cpu)
     (match-define (llgh r1 x2 d2 b2) insn)
     (set-cpu-gpr!-bv
       cpu
       r1
       (zero-extend
         (memmgr-load
           (cpu-memmgr cpu)
           (displacement-index-base cpu d2 x2 b2 sign-extend)
           (bv 0 64)
           (bv 2 64))
         (bitvector 64)))
     #t)])

(struct dl RXY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RXY-a #xE3 #x97))
   (define (instruction-run insn cpu)
     (match-define (dl r1 x2 d2 b2) insn)
     (divide-logical-32
       cpu
       r1
       (memmgr-load
         (cpu-memmgr cpu)
         (displacement-index-base cpu d2 x2 b2 sign-extend)
         (bv 0 64)
         (bv 4 64)))
     #t)])

(struct lmg RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #x04))
   (define (instruction-run insn cpu)
     (match-define (lmg r1 r3 d2 b2) insn)
     (define r r1)
     (define base (displacement-base cpu d2 b2 sign-extend))
     (define offset (bv 0 64))
     (define (loop)
       (set-cpu-gpr!-bv cpu r (memmgr-load (cpu-memmgr cpu) base offset (bv 8 64)))
       (when (not (bveq r r3))
         (set! r (bvadd1 r))
         (set! offset (bvadd offset (bv 8 64)))
         (loop)))
     (loop)
     #t)])

(struct srag RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #x0A))
   (define (instruction-run insn cpu)
     (match-define (srag r1 r3 d2 b2) insn)
     (define result
       (bvashr
         (cpu-gpr-bv cpu r3)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 64))))
     (set-cpu-gpr!-bv cpu r1 result)
     (set-cpu-CCS! cpu result)
     #t)])

(struct slag RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #x0B))
   (define (instruction-run insn cpu)
     (match-define (slag r1 r3 d2 b2) insn)
     (define op3 (sign-extend (cpu-gpr-bv cpu r3) (bitvector 127)))
     (define numeric
       (bvshl op3 (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 127))))
     (define result (concat (msb op3) (extract 62 0 numeric)))
     (set-cpu-gpr!-bv cpu r1 result)
     (set-cpu-cc!
       cpu
       (cond
         [(not (bveq (extract 126 63 op3) (extract 126 63 numeric))) (bv 3 2)]
         [(bvzero? result) (bv 0 2)]
         [(bvzero? (msb result)) (bv 2 2)]
         [else (bv 1 2)]))
     #t)])

(struct srlg RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #x0C))
   (define (instruction-run insn cpu)
     (match-define (srlg r1 r3 d2 b2) insn)
     (define result
       (bvlshr
         (cpu-gpr-bv cpu r3)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 64))))
     (set-cpu-gpr!-bv cpu r1 result)
     #t)])

(struct sllg RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #x0D))
   (define (instruction-run insn cpu)
     (match-define (sllg r1 r3 d2 b2) insn)
     (define result
       (bvshl
         (cpu-gpr-bv cpu r3)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 64))))
     (set-cpu-gpr!-bv cpu r1 result)
     #t)])

(struct rllg RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #x1C))
   (define (instruction-run insn cpu)
     (match-define (rllg r1 r3 d2 b2) insn)
     (define result
       (bvrol
         (cpu-gpr-bv cpu r3)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 64))))
     (set-cpu-gpr!-bv cpu r1 result)
     #t)])

(struct rll RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #x1D))
   (define (instruction-run insn cpu)
     (match-define (rll r1 r3 d2 b2) insn)
     (define result
       (bvrol
         (cpu-gpr32-bv cpu r3)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 32))))
     (set-cpu-gpr32!-bv cpu r1 result)
     #t)])

(struct stmg RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #x24))
   (define (instruction-run insn cpu)
     (match-define (stmg r1 r3 d2 b2) insn)
     (define r r1)
     (define base (displacement-base cpu d2 b2 sign-extend))
     (define offset (bv 0 64))
     (define (loop)
       (memmgr-store! (cpu-memmgr cpu) base offset (cpu-gpr-bv cpu r) (bv 8 64))
       (when (not (bveq r r3))
         (set! r (bvadd1 r))
         (set! offset (bvadd offset (bv 8 64)))
         (loop)))
     (loop)
     #t)])

(struct srak RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #xDC))
   (define (instruction-run insn cpu)
     (match-define (srak r1 r3 d2 b2) insn)
     (define result
       (bvashr
         (cpu-gpr32-bv cpu r3)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 32))))
     (set-cpu-gpr32!-bv cpu r1 result)
     (set-cpu-CCS! cpu result)
     #t)])

(struct slak RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #xDD))
   (define (instruction-run insn cpu)
     (match-define (slak r1 r3 d2 b2) insn)
     (define op3 (sign-extend (cpu-gpr32-bv cpu r3) (bitvector 95)))
     (define numeric
       (bvshl op3 (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 95))))
     (define result (concat (msb op3) (extract 30 0 numeric)))
     (set-cpu-gpr32!-bv cpu r1 result)
     (set-cpu-cc!
       cpu
       (cond
         [(not (bveq (extract 94 31 op3) (extract 94 31 numeric))) (bv 3 2)]
         [(bvzero? result) (bv 0 2)]
         [(bvzero? (msb result)) (bv 2 2)]
         [else (bv 1 2)]))
     #t)])

(struct srlk RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #xDE))
   (define (instruction-run insn cpu)
     (match-define (srlk r1 r3 d2 b2) insn)
     (define result
       (bvlshr
         (cpu-gpr32-bv cpu r3)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 32))))
     (set-cpu-gpr32!-bv cpu r1 result)
     #t)])

(struct sllk RSY-a () #:transparent
  #:methods gen:instruction
  [(define instruction-encode (curry encode-RSY-a #xEB #xDF))
   (define (instruction-run insn cpu)
     (match-define (sllk r1 r3 d2 b2) insn)
     (define result
       (bvshl
         (cpu-gpr32-bv cpu r3)
         (zero-extend (extract 5 0 (displacement-base cpu d2 b2 zero-extend)) (bitvector 32))))
     (set-cpu-gpr32!-bv cpu r1 result)
     #t)])

(define (decode-one fetch)
  (cond
    [(is-RR? fetch #x07) (parse-RR fetch bcr)]
    [(is-RR? fetch #x0D) (parse-RR fetch basr)]
    [(is-RR? fetch #x13) (parse-RR fetch lcr)]
    [(is-RR? fetch #x14) (parse-RR fetch nr)]
    [(is-RR? fetch #x16) (parse-RR fetch _or)]
    [(is-RR? fetch #x17) (parse-RR fetch xr)]
    [(is-RR? fetch #x18) (parse-RR fetch lr)]
    [(is-RR? fetch #x1A) (parse-RR fetch ar)]
    [(is-RR? fetch #x1B) (parse-RR fetch sr)]
    [(is-RX-a? fetch #x40) (parse-RX-a fetch sth)]
    [(is-RX-a? fetch #x41) (parse-RX-a fetch la)]
    [(is-RX-a? fetch #x42) (parse-RX-a fetch stc)]
    [(is-RX-a? fetch #x50) (parse-RX-a fetch st)]
    [(is-RS-a? fetch #x88) (parse-RS-a fetch srl)]
    [(is-RS-a? fetch #x89) (parse-RS-a fetch sll)]
    [(is-RS-a? fetch #x8A) (parse-RS-a fetch sra)]
    [(is-RS-a? fetch #x8B) (parse-RS-a fetch sla)]
    [(is-RS-a? fetch #x8C) (parse-RS-a fetch srdl)]
    [(is-RS-a? fetch #x8D) (parse-RS-a fetch sldl)]
    [(is-RS-a? fetch #x8E) (parse-RS-a fetch srda)]
    [(is-RS-a? fetch #x8F) (parse-RS-a fetch slda)]
    [(is-RI-a? fetch #xA7 #x4) (parse-RI-c fetch brc)]
    [(is-RI-a? fetch #xA7 #x8) (parse-RI-a fetch lhi)]
    [(is-RI-a? fetch #xA7 #x9) (parse-RI-a fetch lghi)]
    [(is-RI-a? fetch #xA7 #xB) (parse-RI-a fetch aghi)]
    [(is-RRE? fetch #xB2 #x52) (parse-RRE fetch msr)]
    [(is-RRE? fetch #xB9 #x03) (parse-RRE fetch lcgr)]
    [(is-RRE? fetch #xB9 #x04) (parse-RRE fetch lgr)]
    [(is-RRE? fetch #xB9 #x08) (parse-RRE fetch agr)]
    [(is-RRE? fetch #xB9 #x09) (parse-RRE fetch sgr)]
    [(is-RRE? fetch #xB9 #x0C) (parse-RRE fetch msgr)]
    [(is-RRE? fetch #xB9 #x0F) (parse-RRE fetch lrvgr)]
    [(is-RRE? fetch #xB9 #x16) (parse-RRE fetch llgfr)]
    [(is-RRE? fetch #xB9 #x1F) (parse-RRE fetch lrvr)]
    [(is-RRE? fetch #xB9 #x80) (parse-RRE fetch ngr)]
    [(is-RRE? fetch #xB9 #x81) (parse-RRE fetch ogr)]
    [(is-RRE? fetch #xB9 #x82) (parse-RRE fetch xgr)]
    [(is-RRE? fetch #xB9 #x85) (parse-RRE fetch llghr)]
    [(is-RRE? fetch #xB9 #x87) (parse-RRE fetch dlgr)]
    [(is-RRE? fetch #xB9 #x97) (parse-RRE fetch dlr)]
    [(is-RIL-b? fetch #xC0 #x0) (parse-RIL-b fetch larl)]
    [(is-RIL-a? fetch #xC0 #x1) (parse-RIL-a fetch lgfi)]
    [(is-RIL-c? fetch #xC0 #x4) (parse-RIL-c fetch brcl)]
    [(is-RIL-b? fetch #xC0 #x5) (parse-RIL-b fetch brasl)]
    [(is-RIL-a? fetch #xC0 #x7) (parse-RIL-a fetch xilf)]
    [(is-RIL-a? fetch #xC0 #xB) (parse-RIL-a fetch nilf)]
    [(is-RIL-a? fetch #xC0 #xD) (parse-RIL-a fetch oilf)]
    [(is-RIL-a? fetch #xC0 #xF) (parse-RIL-a fetch llilf)]
    [(is-RIL-a? fetch #xC2 #x0) (parse-RIL-a fetch msgfi)]
    [(is-RIL-a? fetch #xC2 #x1) (parse-RIL-a fetch msfi)]
    [(is-RIL-a? fetch #xC2 #x8) (parse-RIL-a fetch agfi)]
    [(is-RIL-a? fetch #xC2 #xA) (parse-RIL-a fetch algfi)]
    [(is-RIL-a? fetch #xC2 #xB) (parse-RIL-a fetch alfi)]
    [(is-RIL-b? fetch #xC4 #x8) (parse-RIL-b fetch lgrl)]
    [(is-RIL-b? fetch #xC4 #xC) (parse-RIL-b fetch lgfrl)]
    [(is-SS-a? fetch #xD7) (parse-SS-a fetch xc)]
    [(is-RXY-a? fetch #xE3 #x04) (parse-RXY-a fetch lg)]
    [(is-RXY-a? fetch #xE3 #x16) (parse-RXY-a fetch llgf)]
    [(is-RXY-a? fetch #xE3 #x24) (parse-RXY-a fetch stg)]
    [(is-RXY-a? fetch #xE3 #x50) (parse-RXY-a fetch sty)]
    [(is-RXY-a? fetch #xE3 #x70) (parse-RXY-a fetch sthy)]
    [(is-RXY-a? fetch #xE3 #x72) (parse-RXY-a fetch stcy)]
    [(is-RXY-a? fetch #xE3 #x80) (parse-RXY-a fetch ng)]
    [(is-RXY-a? fetch #xE3 #x81) (parse-RXY-a fetch og)]
    [(is-RXY-a? fetch #xE3 #x82) (parse-RXY-a fetch xg)]
    [(is-RXY-a? fetch #xE3 #x87) (parse-RXY-a fetch dlg)]
    [(is-RXY-a? fetch #xE3 #x90) (parse-RXY-a fetch llgc)]
    [(is-RXY-a? fetch #xE3 #x91) (parse-RXY-a fetch llgh)]
    [(is-RXY-a? fetch #xE3 #x97) (parse-RXY-a fetch dl)]
    [(is-RSY-a? fetch #xEB #x04) (parse-RSY-a fetch lmg)]
    [(is-RSY-a? fetch #xEB #x0A) (parse-RSY-a fetch srag)]
    [(is-RSY-a? fetch #xEB #x0B) (parse-RSY-a fetch slag)]
    [(is-RSY-a? fetch #xEB #x0C) (parse-RSY-a fetch srlg)]
    [(is-RSY-a? fetch #xEB #x0D) (parse-RSY-a fetch sllg)]
    [(is-RSY-a? fetch #xEB #x1C) (parse-RSY-a fetch rllg)]
    [(is-RSY-a? fetch #xEB #x1D) (parse-RSY-a fetch rll)]
    [(is-RSY-a? fetch #xEB #x24) (parse-RSY-a fetch stmg)]
    [(is-RSY-a? fetch #xEB #xDC) (parse-RSY-a fetch srak)]
    [(is-RSY-a? fetch #xEB #xDD) (parse-RSY-a fetch slak)]
    [(is-RSY-a? fetch #xEB #xDE) (parse-RSY-a fetch srlk)]
    [(is-RSY-a? fetch #xEB #xDF) (parse-RSY-a fetch sllk)]
    [else #f]))
