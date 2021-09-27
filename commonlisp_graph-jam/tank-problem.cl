(defpackage #:tank-problem
  (:use "COMMON-LISP"))

(in-package #:tank-problem)
;u some fishies
;; Some pair of fish fight if they are in the same tank.
;; put fish in two tanks so they don't fight.

;; use bfs for testing if a graph is bipartite.
;; coloring the graph layer by layer

;; why does this work?
;; if you can color each layers with different color, the graph
;; looks like a tree, you can move odd index layer to one side, even index
;; layers to another.

;; if you have an edge from an odd index and connecet to another odd index,
;; you have a cycle. What should you color the target node, odd color or
;; even color?

(defmacro init-hash-table (xs)
  `(let ((m (make-hash-table)))
     (loop for kv in ,xs do
           (setf (gethash (car kv) m) (cdr kv)))
     m))

(defmacro enqueue (x xs) `(push ,x ,xs))

(defmacro dequeue (xs)
  (let ((v (gensym)))
    `(let ((,v (car (last ,xs))))
       (setf ,xs (butlast ,xs))
       ,v)))

(defparameter *fish-fight-graph*
  (init-hash-table
    '((a . (b c))
      (b . (a d))
      (c . (a d))
      (d . (b c e f))
      (e . (d g h))
      (f . (d g h))
      (g . (f e))
      (h . (f e)))))

(defparameter *bird-graph*
  (init-hash-table
    '((a . (b c))
      (b . (a f g))
      (c . (a d))
      (d . (e))
      (e . (d f g))
      (g . (b e))
      (f . (b e)))))

;; output
;; (C . 1) (A . 0) (D . 0) (F . 0) (G . 0) (B . 1) (E . 1)

(defparameter *fish-fight-counter-example-graph*
  (init-hash-table
    '((a . (b c))
      (b . (a d c))
      (c . (a d b))
      (d . (b c e f))
      (e . (d g h))
      (f . (d g h))
      (g . (f e))
      (h . (f e)))))

;; output nil

(defun bipartite-* (graph root)
  "if the graph is biartite, return the coloring"
  (let* ((queue `(,root))
         (visited `(,root))
         (color-table
           (let ((keys (remove-duplicates
                         (append
                           (loop for k being the hash-keys in graph)
                           (apply #'append
                                  (loop for v being the hash-values in graph
                                        collect v)) ))))
             (mapcar (lambda (n) (cons n -1)) keys)))
         (color 0))
    (labels ((set-color (node color) (setf (cdr (assoc node color-table)) color))
             (next-color (n) (mod (+ n 1) 2)))
      (set-color root color)
      (block done (loop while queue do
               (let ((v (dequeue queue)))
                 ;; u's color should be the reverse of v's color.
                 (setf color (next-color (cdr (assoc v color-table))))
                 (loop for u in (gethash v graph) do
                       (if (not (member u visited))
                         (progn
                           (push u visited)
                           (enqueue u queue)
                           (set-color u color))
                         (when (not (equal (cdr (assoc u color-table)) color))
                           (return-from done nil))))))
             color-table))))

(defun bipartite (graph)
  "find a node with no in degree as the starting node"
  (bipartite-* graph (car (loop for k being the hash-keys in graph collect k))))
