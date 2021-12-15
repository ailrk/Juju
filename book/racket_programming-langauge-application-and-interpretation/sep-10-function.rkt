#lang plai

;; condition evaluates only one
(define-type ExprC
  [numC (n number?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  [idC (n ExprC?)]
  [appC (sym ExprC?) (var ExprC?)]
  [fundefC (param ExprC?) (body ExprC?)])

; interp : ExprC * [FunDef] -> number

(define (lookup-fundef f fds)
  (if (null? fds) '()
      (let ((head (car fds)))
        (if (equal? (car head) f)
            (cdr head)
            (lookup-fundef f (cdr fds))))))

; substitution is a textual operation, it doesn't take part in
; actual interpretation of the meaning of the expression.
; subst : ExprC * symbol * ExprC -> ExprC
(define (subst var parm body) '())

(define (interp e fds)
  (type-case ExprC e
    [numC (n) n]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]

    [idC (i) ()]
    [appC (f a) (local ((define the-f (lookup-fundef
                                       f fds)))
                  (subst (interp a fds)
                         (fundef-param the-f)
                         (fundef-body the-f)))]

    ))
