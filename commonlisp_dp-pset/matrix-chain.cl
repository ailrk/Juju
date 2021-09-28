(defpackage #:matrix-multiplication
  (:use "COMMON-LISP")
  (:shadow "*"))

(in-package #:matrix-multiplication)

;; recall subproblems for strings
;; possible choices:
;;  1. suffixes x[i:] forall i
;;  2. preffixes x[:i] forall i
;;  3. substrings x[i:j] forall i < j
;; choose one to formulate the subproblem.

(defmacro operator-overload (op &rest definitons)
  (let ((fname (read-from-string (concatenate 'string "binary" (symbol-name op)))))
    `(values
       (defun ,op (&rest ts)
         (reduce (quote ,fname) (cdr ts) :initial-value (car ts)))
       (defgeneric ,fname (a b)
         ,@(loop :for def :in definitons :collect
                 `(,@def))))))

;; optimal evalution of associative expression

;; 1. define subproblem
;;      optimal evaluation of Ai ... Aj-1  # of sub problems O(n^2)
;; 2. guess outermost multiplciation (the last operation we do)
;;    choose to split the chain at kth index.
;;    (Ai...Ak) (Ak+1 ... Aj-1)
;;     # choices = O(j - i + 1) = O(n)
;; 3. reccurence:
;;    DP(i, j) = min {
;;        DP(i, k) + DP(k, j) + (cost of Ai:k * Ak:j)
;;        for k in range(i+1, j)
;;    }
;;  time/subproblem = O(n)
;; 4. total time: O(n^3)
;; 5. topological order: increasing substring size

;;  A1 A2 A3 A4 A5 A6
;;  i-------k-------j


;; we don't care about the value of the matrix
(defclass matrix ()
  ((n :type integer
      :initarg :n
      :accessor n
      :initform 0)
   (m :type integer
      :initarg :m
      :accessor m
      :initform 0)))

(defmethod print-object ((obj matrix) stream)
  "print the matrix"
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((m m) (n n))
      obj
      (format stream "n: ~a, m: ~a" n m))))

(defmethod mmult ((a matrix) (b matrix))
  (with-accessors ((an n) (am m) ) a
    (with-accessors ((bn n) (bm m)) b
      (assert (= am bn))
      (make-instance 'matrix :n an :m bm))))

(defmethod cost ((o matrix))
  "we want to minimize this"
  (with-accessors ((m m) (n n)) o
    (* m n)))


(operator-overload *
                   (:method ((a number) (b number)) (cl:+ a b))
                   (:method ((a matrix) (b matrix)) (mmult a b)))

(defparameter *ms*
  (list (make-instance 'matrix :n 2 :m 3)
        (make-instance 'matrix :n 3 :m 2)))

(defun flattern (xs)
  (labels ((rec (xs acc)
             (cond ((null xs) acc)
                   ((atom xs) (cons xs acc))
                   (t (rec (car xs) (rec (cdr xs) acc))))))
    (rec xs nil)))


(defparameter *memo* (make-hash-table))

(defun parenthesization (xs)
  (declare (type (sequence matrix) xs))
  (let ((sz (length xs)))
    (cond
      ((<= sz 1) (cost (car xs)))
      (t
       (apply #'min
              (flattern
                (loop :for len :from 2 :to sz :collect
                      (loop :for i :from 0 :to (- len 1)
                            :with j = (- (+ i len) 1)
                            :collect
                            (loop :for k :from i :to (- j 1) :collect
                                  (let* ((ik (subseq xs i k))
                                         (kj (subseq xs k j)))
                                    (+ (parenthesization ik)
                                       (parenthesization kj)
                                       (* (cost (apply #'* ik))
                                          (cost (apply #'* kj))))))))))))))


;; todo debug. output behaves right but not quite
;;      memoizartion in lisp.
