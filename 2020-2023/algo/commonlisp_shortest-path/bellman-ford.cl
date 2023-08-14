;; bellman ford is commonly compared with dijkstra.
;; bellman ford is
;; 1. slower
;; 2. can handle negative weights.
;; 3. allows for flexibility of weights changes.

;; O(VE)
;; worst case when it's a complete graph, it will be O(E^3)..

;; bellman ford can take at most V -1 iterations, and it guarantee when
;; the algorithm terminates, it gives the shortest path.

;; If there is a negative cycle in the graph, then there is no shortest
;; path. We expect bellman ford detect negative cycle for us.

;; note:
;; we do v-1 iterations, and relax all nodes at each iterations.
;; At ith iteration we get the shortest path between s to v for v in
;; G with at most i edges!

;; https://www.geeksforgeeks.org/bellman-ford-algorithm-dp-23/
;; https://www.youtube.com/watch?v=obWXjtg0L64


(defpackage #:bellmanford
  (:use "COMMON-LISP")
  (:shadow ">")
  (:shadow "<"))

(in-package #:bellmanford)

(defmacro operator-overload (op &rest definitons)
  (let ((fname (read-from-string (concatenate 'string "binary" (symbol-name op)))))
    `(values
       (defun ,op (&rest ts)
         (reduce (quote ,fname) (cdr ts) :initial-value (car ts)))
       (defgeneric ,fname (a b)
         ,@(loop :for def :in definitons :collect
                 `(,@def))))))

(defmacro init-hash-table (xs)
  `(let ((m (make-hash-table)))
     (dolist (kv ,xs)
       (setf (gethash (car kv) m) (cdr kv)))
     m))

;; define nodes we will be using
(defclass node ()
  ((name
     :type symbol :initarg :name
     :accessor name :initform nil)
   (distance
     :type number :initarg :distance
     :accessor distance :initform most-positive-fixnum)
   (predecessor
     :type node :initarg :predecessor
     :accessor predecessor :initform nil)))

(defmethod print-object ((obj node) stream)
  "print the node"
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name)
                     (distance distance))
      obj
      (format stream "<~a,~a>" name distance))))

;; overload the operator

(operator-overload >
                   (:method ((a number) (b number)) (cl:> a b))
                   (:method ((a node) (b node)) (cl:> (distance a) (distance b))))

(operator-overload <
                   (:method ((a number) (b number)) (cl:< a b))
                   (:method ((a node) (b node)) (cl:< (distance a) (distance b))))


(defmacro new-node (a &optional distance)
  `(cons ,a (make-instance 'node :name ,a
                           :distance (or ,distance most-positive-fixnum))))


;; todo unfinished

;; ok graph
(defparameter *graph-1*
  (init-hash-table
    '((#\s . ((#\e . 8) (#\a . 10)))
      (#\a . ((#\c . 2)))
      (#\b . ((#\a . 1)))
      (#\c . ((#\b . -2)))
      (#\d . ((#\a . -4) (#\c . -1)))
      (#\e . ((#\d . 1))))))


(defmacro alphabet (a z)
  `(loop :for i :from 0 to (- (char-code ,z) (char-code ,a))
         :collect (code-char (+ i (char-code ,a)))))

(defparameter *graph-info-1*
  (init-hash-table
    (loop :for n in (cons #\s (alphabet #\a #\e)) :collect
          (new-node n))))

;; negative cycle graph



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bellman ford

(defmacro relaxf (u v w)
  "relax adjacent nodes. u is the current node"
  `(when (> (distance ,v) (+ (distance ,u) ,w))
     (setf (distance ,v) (+ ( distance ,u) ,w))
     (setf (predecessor ,v) ,u)))


(defmacro foreach-edges ((graph table) (u v w) &body body)
  "pass graph info and current node, work on u v w directly"
  `(macrolet ((u* (n table) `(gethash ,n ,table))
              (v* (m table) `(gethash (car ,m) ,table))
              (w* (m) `(cdr ,m)))
     (maphash (lambda (n adjs)
                (dolist (edge adjs)
                  ,@(nsubst `(u* n ,table) u
                            (nsubst `(v* edge ,table) v
                                    (nsubst '(w* edge) w body)))))
              graph)))


(defun bellman-ford (graph info s)
  "bellman-ford shortest path"
  (declare (type hash-table graph) (type hash-table info))
  (setf (distance (gethash s info)) 0)
  (let ((vertex-number (loop :for _ :being :the :hash-keys :in info :sum 1)))
    (dotimes (i (- vertex-number 1))
      (foreach-edges (graph info) (u v w)
        (relaxf u v w)))
    (block cycle  ;; if keep looping get shorter path we have a negative cycle.
           (foreach-edges (graph info) (u v w)
             (when (> (distance v) (+ (distance u) w)) (return-from cycle nil)))
           (loop :for v :being :the :hash-values :in info :collect v))))


; (format t "~a" (bellman-ford *graph* *graph-info* #\s))
; result:
; (#<NODE <s,0>> #<NODE <a,5>> #<NODE <b,5>> #<NODE <c,7>> #<NODE <d,9>>
;  #<NODE <e,8>>)

(defun print-hash (m)
  (maphash (lambda (k v)
             (progn
               (write (list k v))
               (format t "~%")))
           m))

;; prove correctness.

;; Lemma:
;; 1. if d(u) is not infinity, it's the length of some path from s to u.
;; 2. if there is a path from s to u with at most i edges, then d(u) is the most
;;    length of the shortest path from s to u with at most i edges.

;; proof:
;; lemma 1. prove by induction.
;; base case:       when i = 0 s is a path to s itself with lenght 0.
;; inductive step:  let u be one vertex that is i-1 edges away. v be i away, u be
;;                  the predecessor of v.
;;                  d(v) =

;; lemma 2. induction again.
;;  base case d(s) = 0 is the shortest path of i = 0
;;  inductive step;  let u be the predecessor of v which is ith edges away.
;;                   by inductive hypothesis d(u) is the shortest path for i-1
;;                   d(v) = d(u) + w, which is also the shortest path.
