#lang racket/base

(require ffi/unsafe)

(provide (all-defined-out))

(define _uc_s390x_reg
  (_enum '(invalid = 0
           r0 = 1
           r1 = 2
           r2 = 3
           r3 = 4
           r4 = 5
           r5 = 6
           r6 = 7
           r7 = 8
           r8 = 9
           r9 = 10
           r10 = 11
           r11 = 12
           r12 = 13
           r13 = 14
           r14 = 15
           r15 = 16
           f0 = 17
           f1 = 18
           f2 = 19
           f3 = 20
           f4 = 21
           f5 = 22
           f6 = 23
           f7 = 24
           f8 = 25
           f9 = 26
           f10 = 27
           f11 = 28
           f12 = 29
           f13 = 30
           f14 = 31
           f15 = 32
           f16 = 33
           f17 = 34
           f18 = 35
           f19 = 36
           f20 = 37
           f21 = 38
           f22 = 39
           f23 = 40
           f24 = 41
           f25 = 42
           f26 = 43
           f27 = 44
           f28 = 45
           f29 = 46
           f30 = 47
           f31 = 48
           a0 = 49
           a1 = 50
           a2 = 51
           a3 = 52
           a4 = 53
           a5 = 54
           a6 = 55
           a7 = 56
           a8 = 57
           a9 = 58
           a10 = 59
           a11 = 60
           a12 = 61
           a13 = 62
           a14 = 63
           a15 = 64
           pc = 65
           pswm = 66
           ending = 67)))
