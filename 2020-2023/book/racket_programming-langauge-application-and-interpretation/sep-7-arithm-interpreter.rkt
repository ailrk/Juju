#lang plai

;; interpreter as inductive definition


; ArithC is the core langauge for arithmetic
(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)]
  )

; At this level, interp is the semantics.
; If the goal is to interp expression like 1 + 1 * 2
; so the problem is what syntax map to what.

; ArithC -> number
(define (interp e)
  (type-case ArithC e
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r)) ]
    [multC (l r) (* (interp l) (interp r)) ]))
