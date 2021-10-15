;; dijkstra works with the shortest path.
;; O(VlogV + E) // dominated by E in lots of cases.
(defpackage #:dijkstra
  (:use "COMMON-LISP")
  (:shadow ">")
  (:shadow "<"))

(in-package #:dijkstra)

(defmacro init-hash-table (xs)
  `(let ((m (make-hash-table)))
     (dolist (kv ,xs)
       (setf (gethash (car kv) m) (cdr kv)))
     m))

;; min heap
(defclass min-heap ()
  ((data
     :type list
     :initarg :data
     :accessor data
     :initform (make-array 256 :adjustable t :fill-pointer 0))))

(defmethod print-object ((obj min-heap) stream)
  "print the min-heap"
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((data data))
      obj
      (format stream "~%minheap data: ~a" data))))

(defun left-child (i)
  (declare (type integer i))
  (+ (* 2 i) 1))

(defun right-child (i)
  (declare (type integer i))
  (+ (* 2 i) 2))

(defun parent (i)
  (floor (cond ((= i 0) 0)
               ((= (mod i 2) 0) (/ (- i 2) 2))
               (t (/ (- i 1) 2)))))

(defmethod is-empty ((o min-heap)) (= (length (data o)) 0))

(defmethod swimup ((o min-heap) i)
  "i is the index of the element to swim up"
  (with-accessors ((data data)) o
    (when (< (elt data i) (elt data (parent i)))
      (rotatef (elt data i) (elt data (parent i)))
      (swimup o (parent i)))))

(defmethod sinkdown ((o min-heap) i)
  "sink down while keep the order invariant"
  (with-accessors ((data data))
    o
    (macrolet ((elt-or-nil (seq i)
                 `(if (>= ,i (length ,seq)) nil (elt ,seq ,i))))
      (let* ((largest-idx i) (e (elt-or-nil data i))
             (left-idx (left-child i))
             (left (elt-or-nil data left-idx))
             (right-idx (right-child i))
             (right (elt-or-nil data right-idx)))
        (when e
          (when (and left (> e left)) (setf largest-idx left-idx))
          (when (and right (> e right)) (setf largest-idx right-idx))
          (when (not (= largest-idx i))
            (rotatef (elt data largest-idx) (elt data i))
            (sinkdown o i)))))))

(defmethod insert-heap ((o min-heap) e)
  "insert into the bottom of the heap then swimup"
  (with-accessors ((data data)) o
    (let ((was-empty (is-empty o)))
      (vector-push-extend e data)
      (when (not was-empty) (swimup o (- (length data) 1))))))

(defmethod extract-heap ((o min-heap))
  "extract the min element, move bottom to top and sinkdown"
  (with-accessors ((data data)) o
    (when (not (is-empty o))
      (let ((top (elt data 0))
            (bottom (vector-pop data)))
        (when (not (is-empty o)) (setf (elt data 0) bottom))
        (sinkdown o 0)
        top))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dijkstra

;; test
; (defparameter *m* (make-instance 'min-heap))
; (let ((xs '(8 3 12 5 1)))
;   (loop for i in xs do
;         (insert-heap *m* i)))

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
;; todo make it a macro

(defmacro operator-overload (op &rest definitons)
  (let ((fname (read-from-string (concatenate 'string "binary" (symbol-name op)))))
    `(values
       (defun ,op (&rest ts)
         (reduce  (quote ,fname) (cdr ts) :initial-value (car ts)))
       (defgeneric ,fname (a b)
         ,@(loop :for def :in definitons :collect
                 `(,@def))))))

(operator-overload >
                   (:method ((a number) (b number)) (cl:> a b))
                   (:method ((a node) (b node)) (cl:> (distance a) (distance b))))

(operator-overload <
                   (:method ((a number) (b number)) (cl:< a b))
                   (:method ((a node) (b node)) (cl:< (distance a) (distance b))))


;; undirected weighted graph. nodes info is stored in the
;; corresponding info table.
;; (2 . 7) means edge to 2 has weight 7.
(defparameter *graph-1*
  (init-hash-table
    '((1 . ((2 . 7) (3 . 9) (6 . 14)))
      (2 . ((1 . 7) (3 . 10) (4 . 15)))
      (3 . ((1 . 9) (2 . 10) (6 . 2) (4 . 11)))
      (4 . ((2 . 15) (3 . 11) (5 . 6)))
      (5 . ((6 . 9) (4 . 6)))
      (6 . ((1 . 14) (3 . 2) (5 . 9))))))

(defmacro new-node (a &optional distance)
  `(cons ,a (make-instance 'node :name ,a
                           :distance (or ,distance most-positive-fixnum))))

;; define graph
(defparameter *graph-1-all-nodes*
  (init-hash-table
    (loop :for i :from 1 :to 6 :collect (new-node i))))


;; lemma. the relaxation operation maintains the invaraint
;; that d[v] >= δ(s, v) for all v ∈ V.
(defmacro relaxf (u v w)
  "relax adjacent nodes. u is the current node"
  `(when (> (distance ,v) (+ (distance ,u) ,w))
     (setf (distance ,v) (+ (distance ,u) ,w))
     (setf (predecessor ,v) ,u)))

(defun backtrace (n)
  "once find the target, collecting the result back til the source"
  (let ((xs nil)
        (v n))
    (loop :while v :do
          (push v xs)
          (setf v (predecessor v)))
    xs))

(defun dijkstra (graph info s d)
  "shortest path
   graph: a weighted graph.
   info:  node indexed by node names
   s:     name of the starting node
   d:     name of the target node
   "
  (declare (type hash-table graph) (type hash-table info))
  (setf (distance (gethash s info)) 0)
  (let ((visited `(,s))   ; avoid visit visited nodes.
        (queue (make-instance 'min-heap)))
    (insert-heap queue (gethash s info))

    (block done
           (loop :while queue :do
                 (let* ((u (extract-heap queue))
                        (adjacents (gethash (name u) graph)))

                   ;; return if find the target
                   (when (equal (name u) d) (return-from done (backtrace u)))

                   (dolist (a adjacents)
                     (macrolet ((v () `(gethash (car a) info)))
                       (relaxf u (v) (cdr a))
                       (when (not (member (car a) visited))
                         (push (car a) visited)
                         (insert-heap queue (v))))))))))

(defun print-hash (m)
  (maphash (lambda (k v)
             (progn
               (write (list k v))
               (format t "~%")))
           m))

(dijkstra *graph-1* *graph-1-all-nodes* 1 5)
