#lang plai

;; - looking up: Once we find an identifier we want to replace it's actual
;;    value
;; - efficiency of substitition:
;;    simple substition needs to walk around the expression ast multiple
;;    times, until it finds the variable to substitute.

;; - defer substition

(define-type Env
  [mt]
  [binding (x symbol?)
           (n number?)
           (rest Env?)])

(define-type ExprC
  [numC (n number?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)]
  [idC (n ExprC?)]
  [appC (sym ExprC?) (var ExprC?)]
  [fundefC (param ExprC?) (body ExprC?)])


;; lookup : symbol * env -> number

(define (lookup var env)
  (type-case Env env
    [mt (error "unbound")]
    [binding (y n rest)
             (if (symbol=? x y)
                 n
                 (lookup var rest)
                 )
             ]
    )
  )

; (define (lookup var env)
;   (if (null? env) '()
;       (let ((head (car env)))
;         (if (equal? (car head))
;             (cdr head)
;             (lookup var (cdr env))))))

;; lexical scope: interp-subst, interp-env-fixed

;; dynamic scope: interp-env


; subst : ExprC * symbol * ExprC -> Expr
(define (subst var parm body) '())

; interp : ExprC * Env -> number

(define (interp e env)
  (type-case ExprC e
    [numC (n) n]
    [plusC (l r) (+ (interp l env) (interp r env))]
    [multC (l r) (+ (interp l env) (interp r env))]
    [idC (s) (lookup s env)]
    ; [appC (f a) (local ((define the-f (lookup f env)
    ;                       (interp (fundefC-body f)
    ;                               (binding (fundefC-param f)
    ;                                        (interp a env)
    ;                                        env)))))]

    [appC (f a) (local ((define the-f (lookup f env)))
                  (interp (fundefC-body f)
                          (binding (fundefC-param f)
                                   (interp a enc)
                                   env)))]

    ))
