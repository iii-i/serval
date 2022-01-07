#lang errortrace rosette

(require
  "../lib/core.rkt")

(provide
  (struct-out cpu)
  cpu-cc
  cpu-gprs
  cpu-memmgr
  cpu-pswa
  init-cpu
  gen:instruction
  instruction-encode
  interpret-insn
  set-cpu-cc!
  set-cpu-gprs!
  set-cpu-pswa!)

(struct cpu (memmgr pswa cc gprs)
  #:mutable
  #:transparent
  #:methods gen:gen-cpu
  [(define (gen-cpu-memmgr cpu) (cpu-memmgr cpu))
   (define (gen-cpu-pc cpu) (cpu-pswa cpu))])

(define (init-cpu memmgr)
  (define-symbolic* pc (bitvector 64))
  (define-symbolic* gprs (bitvector 64) #:length 16)
  (define-symbolic* cc (bitvector 2))
  (cpu memmgr pc cc (list->vector gprs)))

(define-generics instruction
  (instruction-encode instruction)
  (instruction-run instruction cpu))

(define (interpret-insn cpu insn)
  (when (instruction-run insn cpu)
    (define size (vector-length-bv (instruction-encode insn) (bitvector 64)))
    ; Symbolic instruction bytes lead to symbolic decoded instructions and
    ; therefore symbolic instruction sizes. The latter cause problems with
    ; jitterbug instruction fetching, which must use non-symbolic PCs, so split
    ; the size.
    (for/all ([size size #:exhaustive])
      (set-cpu-pswa! cpu (bvadd (cpu-pswa cpu) size)))))
