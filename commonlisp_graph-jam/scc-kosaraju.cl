(defpackage #:kosaraju
  (:use "COMMON-LISP"))

(in-package #:kosaraju)
;;;; Strongly connected components.

;; it's not necessarily for you to be able to reach all nodes in a
;; graph from a single starting point.
;; if a directed acyclic graph has multiple strongly connected components,
;; you need to do multiple dfs from  0-in degree nodes.

;; scc is a equivalence class over "mutually reachable" (say Reach) equivalence
;; relation
;; this just means ssc has
;; 1. reflexivity     Reach(a, a)
;; 2. transitivity    Reach(a, b) Reach(b, c) <=> Reach(a, c)
;; 3. symmetric       Reach(a, b) <=> Reach(b, a)

;; main idea of kosaraju ssc:
;;  1. do dfs twice
;;  2. the first dfs push post order traversal of all nodes into a stack.
;;  3. create transpose graph Gt
;;  4. pop element from the stack, dfs from the element on Gt to collect
;;     strongly connected components.

;; strongly connected components themselves generates a new graph, where
;; each scc themselves are considered as a node.

(defmacro init-hash-table (xs)
  `(let ((m (make-hash-table)))
     (loop :for kv :in ,xs do
           (setf (gethash (car kv) m) (cdr kv)))
     m))


;; there are three strongly connected components here.
;; (a b e), (c d h) (f g)
(defparameter *graph*
  (init-hash-table
    '((a . (b))
      (b . (e f c))
      (c . (d g))
      (d . (c h))
      (e . (a f))
      (f . (g))
      (g . (f))
      (h . (g d)))))

(defparameter *graph-1*
  (init-hash-table
    '((3 . (0))
      (0 . (1))
      (1 . (2))
      (2 . (3 4))
      (4 . (5))
      (5 . (6))
      (6 . (4))
      (7 . (8 6))
      (8 . nil))))


;; if we do dfs on the graph above from a, we have
;; (format t "~%dfs a: ~a"  (dfs *graph* 'a))
;; (A B E F C D G H)
;; starting from a you can reach all nodes.
;;
;; but if you start from d you get
;; (format t "~%dfs d: ~a" (dfs *graph* 'd))
;; (D C H G F)
;; starting from d you can not go back to ssc with a in it.
;; why? because you can only go from ssc with a to ssc with d
;; but not vice vera.

(defun all-nodes (graph)
  (remove-duplicates
    (append
      (loop :for k :being :the :hash-keys :in graph :collect k)
      (apply #'append (loop :for v :being :the :hash-values :in graph
                            :collect v)))))


(defun transpose-graph (g)  ;; compute transpose graph
  (let ((m (make-hash-table)))
    (maphash
      (lambda (k v)
        (dolist (n v)
          (setf (gethash n m)
                (cons k (gethash n m))))) g) m))


(defun dfs-tree (graph root &optional visited)
  (let* ((visited visited)
         (result nil)
         (stack `(,root)))
    (loop :while stack do
          (let ((v (pop stack)))
            (dolist (u (gethash v graph))
                  (if (not (member u visited))
                      (progn
                        (push u stack))))
            (pushnew v visited)
            (pushnew v result)))
    ;; return both full visited and newly added nodes
    (values visited result)))


(defun dfs-forest (graph root)
  "keep dfs until find all roots"
  (let* ((stack nil)
         (visited `(,root))
         (unvisited (all-nodes graph)))
    (loop :while (not (null unvisited)) do
          (multiple-value-bind (v s) (dfs-tree graph root visited)
            (push s stack)
            (setf visited v))
          (setf unvisited (set-difference unvisited visited)
                root (car unvisited)))
    (reverse (apply #'append (reverse stack)))))


(defun ssc-kosaraju-* (graph root)
  "O(V+E) uses two dfs"
  (let* ((stack (dfs-forest graph root))
         (transposed (transpose-graph graph))
         (result nil)
         (visited nil))
    (loop :while stack do
          (let* ((n (pop stack)))
            (multiple-value-bind (v connected)
              (dfs-tree transposed n visited)
              (setf visited v
                    stack (remove-if #'(lambda (k) (member k connected)) stack))
              (push connected result))))
    result))


(defun ssc-kosaraju (graph)
  (ssc-kosaraju-* graph (car (loop :for k :being :the :hash-keys :in graph
                                   :collect k))))

(defun print-hash (m)
  (maphash (lambda (k v)
             (progn
               (write (list k v))
               (format t "~%")))
           m))
