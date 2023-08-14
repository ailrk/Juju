;; quickly formulate the dp problem

(defparameter *prices*
  '(-1 1 5 8 9 10 17 17 20 24 30))

(defparameter *memo* (make-hash-table))

;; memoization + recurrence.
;; each subproblem O(1)
;; it's a suffixes problem, so total number of subproblems O(n)
;; thus total running time O(n)
(defun cut-rod (n prices)
  (format t "~a~%" n)
  (cond
    ((gethash n *memo*) (gethash n *memo*))
    ((= n 0) 0)
    (t (let ((result (apply #'max
                            (loop :for i :from 1 :to n :collect
                                  (+ (elt prices i) (cut-rod (- n i) prices))))))
         (setf (gethash n *memo*) result)
         result))))
