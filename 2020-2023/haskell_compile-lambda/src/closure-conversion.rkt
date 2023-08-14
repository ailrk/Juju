#lang racket
(require racket/block)
(require racket/trace)
(require test-engine/racket-tests)

;; trace-define to trace for errors

;;;; Closure conversion
;; To compile nested lambda, we can hoist nested definitions to the top level
;; and compile them separately.
;; but a lamdba is not just a procedure alone, it also can have free variables:
;; closure conversion is an open lambda term + an environment dictionary.

;; When compile lambda, before hoist the function alone, we convert them
;; into a closure: a pair of lambda and it's environment.
;; the lambda is modified so it takes env as the first parameter.

;; When applying the function, the current environment will be passed, and
;; repace occurrence of free variables.

;; Two types of closure conversion algorithms,
;; 1. Flat closures / bottom up
;; 2. Shared closures / top down

;; a closure is a struct with code and the environment
;; {lambda env, ...->...,  {a: x, b: y}}

;; <expr> ::= (lambda (<var> ...) <expr>)
;;          | (<exp> <exp> ...)
;;          | <var>

;;          ; for closure conversion
;;          | (lambda* (<var> <var> ...) <exp>)   ;; already converted lambda
;;          | (make-closure <exp> <exp>)
;;          | (make-env (<var> <exp>) ...)
;;          | (env-ref <exp> <var>)
;;          | (apply-closure <exp> <exp> ...)   ;; invoking clousre instead of a procedure

;; compute free variables of the current term
; free : exp => set[var]
(define (free exp)
  (match exp
    (`(lambda ,params ,body)
     (set-subtract (free body) (apply set params)))                 ;; free body contains params already
    (`(lambda* ,params ,body)
     (set-subtract (free body) (apply set params)))                 ;; same as normal lambda
    ((? symbol?) (set exp))
    (`(make-closure ,proc ,env) (set-union (free proc) (free env))) ;; combine environmnets.
    (`(make-env (,vs ,es) ...) (apply set-union (map free es)))     ;; combine env of expressions
    (`(env-ref ,env ,v) (free env))
    (`(apply-closure ,f ,args ...)
     (apply set-union (map free `(,f . ,args))))
    (`(,f ,args ...)
     (apply set-union (map free `(,f . ,args))))))

(check-expect (free '(make-closure (lambda (env x) ((env-ref env f) x))
                                   (make-env (f f))))
              (set 'f))

;; beta substitution.
; substitute : hash[var, exp] exp => exp
(define (substitute sub exp)
  (match exp
    (`(lambda ,params ,body)
     (define params* (apply set params))
     (define sub* (for/hash
                      (((k v) sub)
                       #:when (not (set-member? params* k))) (values k v)))
     `(lambda ,params ,(substitute sub* body)))
    (`(lambda* ,params ,body)
     (define params* (apply set params))
     (define sub* (for/hash
                      (((k v) sub)
                       #:when (not (set-member? params* k))) (values k v)))
     `(lambda* ,params ,(substitute sub* body)))
    ((? symbol?) (if (hash-has-key? sub exp)
                     (hash-ref sub exp)
                     exp))
    (`(make-closure ,lam ,env) `(make-closure ,(substitute sub lam)
                                              ,(substitute sub env)))
    (`(make-env (,vs ,es) ...) `(make-env ,@(map list vs (map (substitute-with sub) es))))
    (`(env-ref ,env ,v) `(env-ref ,(substitute sub env) ,v))
    (`(apply-closure ,f ,args ...) `(apply-closure ,@(map (substitute-with sub) `(,f . ,args))))
    (`(,f ,args ...) (map (substitute-with sub) `(,f . ,args)))))

(define (substitute-with sub) (lambda (exp) (substitute sub exp)))

;; check expect for testing
;; this substittues free variable
(check-expect
 (substitute (make-hash '((f . +) (x . 4)))
             '(make-closure (lambda (env x) ((env-ref env f) x))
                            (make-env (f f))))
 '(make-closure (lambda (env x) ((env-ref env f) x))
                (make-env (f +))))

;; perform the closure conversion
(define (closure-convert exp)
  (match exp
    (`(lambda ,params ,body)              ;; start perform closure conversion here.
     (define $env (gensym 'env))          ;; create a fresh symbol
     (define params* (cons $env params))  ;; pass env as extra parameter
     (define fv (free exp))               ;; get set of free variables
     (define env (for/list ((v fv)) (list v v)))
     (define sub (for/hash ((v fv)) (values v `(env-ref ,$env ,v))))
     (define body* (substitute sub body))   ;; beta reduction
     `(make-closure (lambda* ,params* ,body*) (make-env ,@env)))
    (`(lambda* ,params ,body) exp)    ;; other cases just propagates
    ((? symbol?) exp)
    (`(make-closure ,lam ,env) exp)
    (`(make-env (,vs ,es) ...) exp)
    (`(env-ref ,env ,v) exp)
    (`(apply-closure ,f ,args ...) exp)
    (`(,f ,args ...) `(apply-closure ,f . ,args))))   ;; apply a closure

;; pretty print s expression
(block
 (displayln "[check closure convert]")
 (pretty-write (closure-convert '(lambda (x) (+ x a b)))))

;;;; Now we need to perform closure conversion on every nodes in the tree.
;;;; We can either transform from bottom up or top down order, the result
;;;; should be the same.

;; bottom up tree transformation / bottom up
(define (tranform/bottomup f exp)
  (define (t e) (tranform/bottomup f e))
  (let ((exp* (match exp
                (`(lambda ,params ,body) `(lambda ,params ,(t body)))
                (`(lambda* ,params ,body) `(lambda* ,params ,(t body)))
                ((? symbol?) exp)
                (`(make-closure ,lam ,env) `(make-closure ,(t lam) ,(t env)))
                (`(make-env (,vs ,es) ...) `(make-env ,@(map list vs (map t es))))
                (`(env-ref ,env ,v) `(env-ref ,(t env) ,v))
                (`(apply-closure ,f ,args ...) `(apply-closure ,(t f) ,(map t args)))
                (`(,f ,args ...) `(,(t f) ,@(map t args))))))
    (f exp*)))

;; top down tree transformation
(define (transform/topdown f exp)
  (define (t e) (transform/topdown f e))
  (match (f exp)
    (`(lambda ,params ,body) `(lambda ,params ,(t body)))
    (`(lambda* ,params ,body) `(lambda* ,params ,(t body)))
    ((? symbol?) exp)
    (`(make-closure ,lam ,env) `(make-closure ,(t lam) ,(t env)))
    (`(make-env (,vs ,es) ...) `(make-env ,@(map list vs (map t es))))
    (`(env-ref ,env ,v) `(env-ref ,(t env) ,v))
    (`(apply-closure ,f ,args ...) `(apply-closure ,(t f) ,@(map t args)))
    (`(,f ,args ...) `(,(t f) ,@(map t args)))))

(define (flat-closure-convert exp) (tranform/bottomup closure-convert exp))
(define (shared-closure-convert exp) (transform/topdown closure-convert exp))

(define example1 '(lambda (g) (lambda (z) (lambda (x) (g x z a)))))
(define example2 '(lambda (x) x))

(block
 (displayln "[flat closure] ===")
 (pretty-write (flat-closure-convert example1))
 (displayln "")

 (displayln "[shared closure] ===")
 (pretty-write (shared-closure-convert example1))
 (displayln "")

 (displayln "[flat closure 1] ===")
 (pretty-write (flat-closure-convert example2))
 (displayln ""))

(test)
