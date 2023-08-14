#lang racket


;; call/cc makes the entiure surrounding code into a continuation and get
;; passed to the lambda. Doing so allows you to decide where to call the
;; continutaion.
(define callcc1
  (lambda ()
    (define k 0)
    (+ 2 (call/cc (lambda (cc) (set! k cc) 3)))))

; let/cc is a short hand for call/cc))
(define callcc2 (lambda () (define k 0) (+ 2 (let/cc cc (set! k cc) 3))))

(define callcc-desugared
  (lambda ()
    (define cc (lambda (hole) (+ 2 hole)))
    (cc 3)))

(define-syntax try
  (syntax-rules (catch)
    ((_ body (catch handler))
     (call/cc (lambda (exit)
                (call-with-exception-handler
                 (lambda (condition) handler (exit condition))
                 (lambda () body)))))))

(define notwo
  (lambda ()
    (try
     (begin (display "1\n") (raise 'some-error) (display "2\n"))
     (catch (display "[error]: ")))))


;; logic programming
;; 1. search
;; 2. binding

;; e.g sudoku, some knots we can tweak, some knot config will satisfy our
;;     constraint.

;; P : we can solve the problems efficiently
;; NP: We can recognizing solutions (If we have orcale give us solution)

;; Sudoku is NP problem, we can verify in O(2n) but we can't find a solution
;; very efficiently.

;; Cast problems into a satisfiability problem.
;; e.g tic tac toe

;; turn the board into a satisfiability problem:
;;   0 1 2
;;   3 4 5
;;   6 7 8
;;   (0 && 1 && 2) || (3 && 4 && 5) || (6 && 7 && 8) ||
;;   (0 && 3 && 6) || ... || (no enough space)


;; What do we want for this search engine?
;;  - on each search step we want to know if the constriant is satisfied.
;;  - base on whether it's satisfied, we want to choose different things to do.
;; How?
;;  - every computation is parameterized with two continuation:
;;    success and failure. Which one is called base on the result of the
;;    computation
;;  - wrap logic connectives with continuations above.

;; bottom contiuation
(define (truth f) 'success)
(define (falsity s f) (f 'fail))

(define (try-and t1 t2)
  (lambda (success failure)
    (begin (let/cc succ
             (t1 succ failure))           ; go back to the point of creation
           (t2 success failure))))

(define (try-and2 t1 t2)                  ; same thing, less efficient.
  (lambda (success failure)
    (begin (t1 (lambda () (t2 success failure))
               failure))))
