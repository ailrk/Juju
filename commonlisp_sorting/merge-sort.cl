
;;;; Analyse so called divide and conquer algorithm 2.3

;; the merge operator
(defun merge-* (xs p q r)
  (declare (type sequence xs)
           (type number p q r))
  (let* ((n1 (+ (- q p) 1))
         (n2 (- r q))
         (lhs (loop for i from 0 to n1
                    if (= i n1) collect most-positive-double-float
                    else collect (elt xs (+ p i))))
         (rhs (loop for j from 0 to n2
                    if (= j n2) collect most-positive-double-float
                    else collect (elt xs (+ (+ q j) 1))))
         (i 0)
         (j 0))
    (loop for k from p to r do
          (if (<= (elt lhs i) (elt rhs j))
              (progn
                (setf (elt xs k) (elt lhs i))
                (incf i))
              (progn
                (setf (elt xs k) (elt rhs j))
                (incf j)))))
  xs)

(defun merge-sort-* (xs p r)
  (declare (type sequence xs)
           (type number p r))
  (let* ((q 0))
    (if (< p r)
        (progn
          (setf q (floor (/ (+ p r) 2)))
          (merge-sort-* xs p q)
          (merge-sort-* xs (+ q 1) r)
          (merge-* xs p q r))))
  xs)

(defun merge-sort (xs)
  (declare (type sequence xs)
           (type number p r))
  (merge-sort-* xs 0 (- (length xs) 1)))


(let ((xs '(908 23 24 189 890 9734 310 904 8)))
  (merge-sort xs))


;;;; analyzing divide and conquer algorithm
;; Note the run time of a recursive algorithm needs  to be described by itself
;; since it calls itself multiple times.
;; Idea: for each iteration we break problem into subproblems,
;;       recursively solve them until hit the base case,
;;       and combine subproblem into the solution.
;; We can define the running time of merge sort as:
;; T(n) = {
;;   θ(1)                       if n ≤ c,
;;   aT(n/b) + D(n) + C(n)      otherwise.
;; }
;; where D(n): time to divide a problem to subproblems
;;       C(n): time to combine subproblems
;;       a:    number of sub problems
;;       1/b:    the size of input of subproblem relative to n.

;;;; for merge sort,
;; analysis T(n) = {
;;   θ(1)                       if n ≤ c,
;;   2T(n/2) + D(n) + C(n)      otherwise.
;; }
;; divide:        Dividing array by compute the middle index, D(n) = θ(1)
;; concuqer:      two subproblems, each has half the input size. 2T(n/2)
;; combine        Combining array by calling merge-*, C(n) = θ(n)

;;;; Why T(n) = θ(nlgn) ? Take merge sort as an example.
;; We first simplify the cost as:
;; T(n)  =  {
;;   θ(1)             if n ≤ 3,
;;   2T(n/2) + cn     otherwise
;; }
;; where cn is the cost for the cost of divide and combine.
;;         T(n) + cn                               cn
;;          +--+--+                              +--+--+
;;         /       \                            /       \
;;     T(n/2)      T(n/2)                      /         \
;;     + cn/2      + cn/2                   cn/2          cn/2
;;    /    \        /    \                 /    \        /    \
;; T(n/4) T(n/4)  T(n/4)  T(n/4)          /      \      /      \
;; +cn/4  +cn/4   +cn/4   +cn/4         +cn/4  +cn/4   +cn/4   +cn/4

;; We know:
;;  1. at i level, there are 2ⁱ nodes.
;;  2. we keep dividing till base case, e.g when i = n.
;;  3. we know at the bottom there will be n nodes. (divided to the smallest problem).
;;  4. say we have total level I,
;;      2ᴵ⁻¹ = n
;;    ⇔ I - 1 = lgn
;;    ⇔ I = lgn + 1.
;;  5. at each we have cn cost for dividing and combining
;;  6. thus total cost = T(n) = cn(lgn + 1) + cn
;;  7. thus T(n) = θ(nlgn).

;;;; ** The key of recursive algorithm get logn complexity **
;; Is because it divides problems as a tree, and tree height is (lgn + 1).

;;;; visualize how problem growth by (nlogn) and n²
;;      x             x  x  x  x
;;     / \
;;    x   x           x  x  x  x
;;   / \ / \
;;  x  xx   x         x  x  x  x

;;;; Updated merge sort
;; when n is small enough, insertion sort
