#lang lazy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; how many things we can get rid of from a langauge. with only lambda
; encoding things in lambda
;; goal: implement fact in lambda only
(define (fact n)
  (if (zero? n) 1
      (* n (fact (sub1 n)))))

;; abstract recursion
(define fact1
  (let ((mkfact
         (lambda (f)
           (lambda (n)
             (if (zero? n)
                 1
                 (* n ((f f) (sub1 n))))))))
    (mkfact mkfact)))

;; simplest self reflication
(define Omega (lambda (x) (x x)))
(define bottom (lambda (_) (Omega Omega)))

;; refactor fact1 above.
(define Y1
  (let ((fork
         (lambda (g)
           (lambda (f)
             (g (f f))))
         ))
    fork fork))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pair
(define (pair a b) (lambda (sel) (sel a b)))
(define (fst a b) a)
(define (snd a b) b)

;; conditional. needs lazy
(define (cnd c t e) (c t e))
(define (truth a b) a)
(define (falsity a b) b)

;; number. take f and applies it n times.
(define id (lambda (x) x))
(define Z (lambda (f) id))
(define (suc N)
  (lambda (f)
    (lambda (x)
      (f ((N f) x)))))    ;; first apply f N times, then apply one more time.

(define (n->i N) ((N add1) 0))
(define (i->n i) (if (zero? i) Z (suc (i->n (sub1 i)))))

(define (add M N) ((M suc) N))
(define (mult M N) (M (N add) N))

;; Z is identity, falsity will not be called.
(define (Z? N) ((N (lambda (_) falsity)) truth))

;; (0, 0), (0, 1), (1, 2) ... once hit (1, 2), 1 is the pred.
(define (pred N)
  (((N (lambda (p)
         (pair (p snd)
               (suc (p snd)))))
    (pair Z Z))
   fst))

(define Y
  (lambda (g)
    ((lambda (f) (f f))
     (lambda (f) (g (f f))))))

(define fact2
  (Y
    (lambda (fct)
      (lambda (n)
        (if (zero? n)
          1
          (* n (fct (sub1 n))))))))
