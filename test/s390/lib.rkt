#lang errortrace rosette

(require
  serval/s390
  serval/unicorn
  serval/lib/core
  serval/lib/unittest)

(provide
  data-size
  data-size-multiple
  run-tests
  test-case-RI-a
  test-case-RI-c
  test-case-RIL-a
  test-case-RIL-b
  test-case-RIL-c
  test-case-RR
  test-case-RRE
  test-case-RS-a
  test-case-RSY-a
  test-case-RX-a
  test-case-RXY-a
  test-case-SS-a
  test-suite+)

(define (integer->rn i)
  (string->symbol (format "r~a" i)))

(define (page-align addr) (bvand addr (bv #xfffffffffffff000 64)))
(define (page-align-up addr) (page-align (bvadd addr (bv #xfff 64))))
(define (data-to-uc data) (list->bytes (map bitvector->natural (bitvector->list/be data))))

(define (cpu->uc cpu code-addr code data-specs)
  (define uc (uc-open 's390x 'big-endian))
  ; map pages
  (define pages (make-hash))
  (define (add-pages addr size)
    (define page (page-align addr))
    (define end (page-align-up (bvadd addr (bv size 64))))
    (define (loop)
      (when (bvult page end)
        (hash-set! pages (bitvector->natural page) #f)
        (set! page (bvadd page (bv #x1000 64)))
        (loop)))
    (loop))
  (for ([data-spec data-specs])
    (match-define (cons data-addr data) data-spec)
    (add-pages data-addr (/ (bitvector-size (type-of data)) 8)))
  (add-pages code-addr (vector-length code))
  (for [(page (hash-keys pages))] (uc-mem-map uc page #x1000 'all))
  ; write data
  (for ([data-spec data-specs])
    (match-define (cons data-addr data) data-spec)
    (uc-mem-write uc (bitvector->natural data-addr) (data-to-uc data)))
  ; write code
  (uc-mem-write
    uc
    (bitvector->natural code-addr)
    (list->bytes (map bitvector->natural (vector->list code))))
  ; set psw
  (uc-reg-write uc 'pc (bitvector->natural (cpu-pswa cpu)))
  (uc-reg-write
    uc
    'pswm
    (bitvector->natural
      (concat
        (bv 0 18)
        (cpu-cc cpu)
        (bv 0 11)
        (bv #b11 2) ; 64-bit addressing mode
        (bv 0 31))))
  ; set gprs
  (for ([i (range 16)])
    (uc-reg-write uc (integer->rn i) (bitvector->natural (vector-ref (cpu-gprs cpu) i))))
  uc)

(define (uc->cpu uc)
  (define pswa (bv (uc-reg-read uc 'pc) 64))
  (define cc (extract 45 44 (bv (uc-reg-read uc 'pswm) 64)))
  (define gprs
    (for/vector ([i (range 16)])
      (bv (uc-reg-read uc (integer->rn i)) 64)))
  (cpu #f pswa cc gprs))

(define (check-insn data-specs sym-insn-bytes)
  (define sym-code-addr (concat (make-arg (bitvector 63)) (bv 0 1)))
  (define sym-cpu (init-cpu (make-flat-memmgr #:big-endian #t)))
  (set-cpu-pswa! sym-cpu sym-code-addr)
  (match-define (list insn-bytes code-addr cpu) (arbitrary (list sym-insn-bytes sym-code-addr sym-cpu)))
  (define concrete-data-specs
    (map
      (lambda (data-spec)
        (match-define (cons data-addr-func data-size-func) data-spec)
        (cons
          (data-addr-func cpu insn-bytes)
          (arbitrary (make-arg (bitvector (* (data-size-func cpu insn-bytes) 8))))))
      data-specs))
  (with-check-info
    [('insn-bytes insn-bytes)
     ('initial-pc (cpu-pswa cpu))
     ('initial-cc (cpu-cc cpu))
     ('initial-gprs (vector->immutable-vector (cpu-gprs cpu)))
     ('data-specs concrete-data-specs)]
    (define insn
      (decode-one (lambda (i) (if (< i (vector-length insn-bytes)) (vector-ref insn-bytes i) #f))))
    (with-check-info
      [('insn insn)]
      ; check encode-decode consistency
      (check-equal? (instruction-encode insn) insn-bytes)

      ; make sure the initial states match
      (define uc (cpu->uc cpu code-addr insn-bytes concrete-data-specs))
      (let ()
        (define uc-cpu (uc->cpu uc))
        (check-equal? (cpu-pswa cpu) (cpu-pswa uc-cpu))
        (check-equal? (cpu-cc cpu) (cpu-cc uc-cpu))
        (check-equal? (cpu-gprs cpu) (cpu-gprs uc-cpu)))

      ; run the emulator
      (define uc-faulted? #f)
      (with-handlers ([exn:fail? (lambda (exn) (set! uc-faulted? #t))])
        (uc-emu-start
          uc
          (bitvector->natural code-addr)
          (bitvector->natural (bvadd code-addr (bv (vector-length insn-bytes) 64)))
          0
          1))
      (define uc-cpu (uc->cpu uc))
      (with-check-info
        [('expected-pc (cpu-pswa uc-cpu))
         ('expected-cc (cpu-cc uc-cpu))
         ('expected-gprs (vector->immutable-vector (cpu-gprs uc-cpu)))]

        ; if the emulator faulted (e.g., illegal instructions), the interpreter should also fault
        (define emu-pred (if uc-faulted? exn:fail? (lambda (x) #f)))

        ; run the interpreter
        (for ([data-spec concrete-data-specs])
          (match-define (cons data-addr data) data-spec)
          (define data-size (/ (bitvector-size (type-of data)) 8))
          (memmgr-store! (cpu-memmgr cpu) data-addr (bv 0 64) data (bv data-size 64)))
        (with-handlers ([emu-pred (lambda (exn) (clear-vc!))])
          (interpret-insn cpu insn))

        (with-check-info
          [('pc (cpu-pswa cpu))
           ('cc (cpu-cc cpu))
           ('gprs (vector->immutable-vector (cpu-gprs cpu)))]
          ; check if the final states match
          (check-equal? (cpu-pswa cpu) (cpu-pswa uc-cpu) "pswa mismatch")
          (check-equal? (cpu-cc cpu) (cpu-cc uc-cpu) "cc mismatch")
          (for ([i (range 16)])
            (check-equal?
              (vector-ref (cpu-gprs cpu) i)
              (vector-ref (cpu-gprs uc-cpu) i)
              (format "R~a mismatch" i))))
          (for ([data-spec concrete-data-specs])
            (match-define (cons data-addr data) data-spec)
            (define data-size (/ (bitvector-size (type-of data)) 8))
            (check-equal?
              (data-to-uc (memmgr-load (cpu-memmgr cpu) data-addr (bv 0 64) (bv data-size 64)))
              (uc-mem-read uc (bitvector->natural data-addr) data-size)))))))

(define (test-case-common name data-specs . insn-bytes)
  (test-case+
    name
    (quickcheck (check-insn data-specs (apply vector insn-bytes)))))

(define (data-size size)
  (define (result cpu insn-bytes) size)
  result)

(define (data-size-multiple size)
  (define (result cpu insn-bytes)
    (define r1r3 (vector-ref insn-bytes 1))
    (* (+ (bitvector->natural (bvsub (extract 3 0 r1r3) (extract 7 4 r1r3))) 1) size))
  result)

(define (test-case-RI-a name opcode1 opcode2)
  (test-case-common
    name
    (list)
    (bv opcode1 8)
    (concat (make-arg (bitvector 4)) (bv opcode2 4))
    (make-bv8)
    (make-bv8)))

(define test-case-RI-c test-case-RI-a)

(define (test-case-RIL-a name opcode1 opcode2)
  (test-case-common
    name
    (list)
    (bv opcode1 8)
    (concat (make-arg (bitvector 4)) (bv opcode2 4))
    (make-bv8)
    (make-bv8)
    (make-bv8)
    (make-bv8)))

(define (RIL-b-mem cpu insn-bytes)
  (bvadd
    (cpu-pswa cpu)
    (concat
      (sign-extend
        (concat
          (vector-ref insn-bytes 2)
          (vector-ref insn-bytes 3)
          (vector-ref insn-bytes 4)
          (vector-ref insn-bytes 5))
        (bitvector 63))
      (bv 0 1))))

(define (test-case-RIL-b name opcode1 opcode2 #:data-size [data-size #f])
  (test-case-common
    name
    (if data-size (list (cons RIL-b-mem data-size)) (list))
    (bv opcode1 8)
    (concat (make-arg (bitvector 4)) (bv opcode2 4))
    (make-bv8)
    (make-bv8)
    (make-bv8)
    (make-bv8)))

(define test-case-RIL-c test-case-RIL-a)

(define (test-case-RR name opcode)
  (test-case-common name (list) (bv opcode 8) (make-bv8)))

(define (test-case-RRE name opcode1 opcode2)
  (test-case-common
    name
    (list)
    (bv opcode1 8)
    (bv opcode2 8)
    (bv 0 8)
    (make-bv8)))

(define (test-case-RS-a name opcode)
  (test-case-common
    name
    (list)
    (bv opcode 8)
    (concat (make-arg (bitvector 4)) (bv 0 4))
    (make-bv8)
    (make-bv8)))

(define (RSY-a-mem cpu insn-bytes)
  (bvadd
    (sign-extend
      (concat
        (vector-ref insn-bytes 4)
        (extract 3 0 (vector-ref insn-bytes 2))
        (vector-ref insn-bytes 3))
      (bitvector 64))
    (cpu-gpr-0-bv cpu (extract 7 4 (vector-ref insn-bytes 2)))))

(define (test-case-RSY-a name opcode1 opcode2 #:data-size [data-size #f])
  (test-case-common
    name
    (if data-size (list (cons RSY-a-mem data-size)) (list))
    (bv opcode1 8)
    (make-bv8)
    (make-bv8)
    (make-bv8)
    (make-bv8)
    (bv opcode2 8)))

(define (cpu-gpr-0-bv cpu i)
  (if (bvzero? i) (bv 0 64) (vector-ref-bv (cpu-gprs cpu) i)))

(define (RX-a-mem cpu insn-bytes)
  (bvadd
    (zero-extend (concat (extract 3 0 (vector-ref insn-bytes 2)) (vector-ref insn-bytes 3)) (bitvector 64))
    (cpu-gpr-0-bv cpu (extract 3 0 (vector-ref insn-bytes 1)))
    (cpu-gpr-0-bv cpu (extract 7 4 (vector-ref insn-bytes 2)))))

(define (test-case-RX-a name opcode #:data-size [data-size #f])
  (test-case-common
    name
    (if data-size (list (cons RX-a-mem data-size)) (list))
    (bv opcode 8)
    (make-bv8)
    (make-bv8)
    (make-bv8)))

(define (RXY-a-mem cpu insn-bytes)
  (bvadd
    (sign-extend
      (concat
        (vector-ref insn-bytes 4)
        (extract 3 0 (vector-ref insn-bytes 2))
        (vector-ref insn-bytes 3))
      (bitvector 64))
    (cpu-gpr-0-bv cpu (extract 3 0 (vector-ref insn-bytes 1)))
    (cpu-gpr-0-bv cpu (extract 7 4 (vector-ref insn-bytes 2)))))

(define (test-case-RXY-a name opcode1 opcode2 #:data-size [data-size #f])
  (test-case-common
    name
    (if data-size (list (cons RXY-a-mem data-size)) (list))
    (bv opcode1 8)
    (make-bv8)
    (make-bv8)
    (make-bv8)
    (make-bv8)
    (bv opcode2 8)))

(define (SS-a-mem cpu insn-bytes)
  (bvadd
    (zero-extend
      (concat
        (extract 3 0 (vector-ref insn-bytes 2))
        (vector-ref insn-bytes 3))
      (bitvector 64))
    (cpu-gpr-0-bv cpu (extract 7 4 (vector-ref insn-bytes 2)))))

(define (SS-a-mem-2 cpu insn-bytes)
  (bvadd
    (zero-extend
      (concat
        (extract 3 0 (vector-ref insn-bytes 4))
        (vector-ref insn-bytes 5))
      (bitvector 64))
    (cpu-gpr-0-bv cpu (extract 7 4 (vector-ref insn-bytes 4)))))

(define (data-size-SS-a cpu insn-bytes) (+ (bitvector->natural (vector-ref insn-bytes 1)) 1))

(define (test-case-SS-a name opcode)
  (test-case-common
    name
    (list (cons SS-a-mem data-size-SS-a) (cons SS-a-mem-2 data-size-SS-a))
    (bv opcode 8)
    (make-bv8)
    (make-bv8)
    (make-bv8)
    (make-bv8)
    (make-bv8)))
