(defpackage #:topological-sort
  (:use "COMMON-LISP")
  (:shadow "+"))

(in-package #:topological-sort)


;; find topological order of a directed acyclic graph (DAG problem)
;; it's useful for many scheduling algorithms

(defmacro init-hash-table (xs)
  `(let ((m (make-hash-table)))
     (dolist (kv ,xs)
       (setf (gethash (car kv) m) (cdr kv)))
     m))

;; topological sort works on a directed acyclic graph.
(defparameter *dependencies*
  (init-hash-table '((tar . nil)
                     (coreutils . (libbz2 libselinux1))
                     (dpkg . (coreutils tar multiarch-support))
                     (multiarch-support . nil)
                     (libselinux1 . (multiarch-support)))))

;; this gives you a topo order directly.

(defun dfs (graph root)
  (let* ((visited `(,root))
         (stack `(,root)))
    (loop :while stack :do
          (let ((v (pop stack)))
            (dolist (u (gethash v graph))
              (when (not (member u visited))
                (pushnew u visited)
                (push u stack)))))
    (reverse visited)))

(defun toposort-dfs (graph)
  "find a node with no in degree as the starting node"
  (let* ((non-zero-indegrees
           (remove-duplicates
             (apply #'append
                    (loop :for v :being :the :hash-values :in graph
                          :collect v))))
         (allnodes
           (remove-duplicates
             (append
               (loop :for k :being :the :hash-keys :in graph :collect k)
               non-zero-indegrees)))

         (zero-indegrees (set-difference allnodes non-zero-indegrees)))
    (dfs graph (car zero-indegrees))))


(format t "~a~%" (toposort-dfs *dependencies*))
