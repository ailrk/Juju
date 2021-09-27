;; complexity of insertion sort 2.2

;; input: (a1, a2, ..., an)
;; output: permutation of input that (a1', a2', ..., an'), s.t
;;         a1' <= a2' ... <= an'


(defun insertion-sort (xs)
  (declare (type sequence xs))
  (let* ((bound (- (length xs) 1))
         (key 0))
    (loop for j from 1 to bound   ;; picking a card
          for i = (- j 1) do
          (setf key (elt xs j))
          ;; insetion part
          (loop while (and (>= i 0)
                           (> (elt xs i) key)) do
                (setf (elt xs (+ i 1)) (elt xs i))
                (decf i))
          (setf (elt xs (+ i 1)) key)))
 xs)

(insertion-sort '(3 2 1))
(insertion-sort #(893 32 3 12 1 22 3 23))

;;;; Prove correctness with loop invariants.
;; loop invariant P:
;; start of each iteration, the subarray xs[0..j-1] is sorted

;; initialization:  P true for j = 0, as xs[0..0] is awlays sorted.
;; maintenance:     P true for j => P true for j+1.
;; termination:     when p = len(xs)-1, P still true. and xs[0..j] == xs. proved

;; NOTE: it's all like induction but terminates.
;; NOTE: it's an incremental approach of algorihtm.
;;       on the other hand, merge sort is divide and conquer.

;;;; Algorithm analysis
;;; Some conceptes
;; -- input size --
;;   What to be considered as the input depends on each problem. For instance,
;;   for a graph algorihtm, it's probably more appropriate to consider both number
;;   of vertices and edges as the input.
;; -- running time --
;;   Steps executed. (constant amount of time to perform one step)

;;;; Define runtimes
;; we assign each step a constant cost cₙ, and see how many times they are
;; executed
;;                                                        cost     times
(defun insertion-sort-* (xs)
  (declare (type sequence xs))
  (let* ((bound (- (length xs) 1))
         (key 0))
    (loop for j from 1 to bound                          ; c1        n
          for i = (- j 1) do                             ; c2        n - 1
          (setf key (elt xs j))                          ; c3        n - 1
          (loop while (and (>= i 0)                      ; c4        ∑(j=2, n)(tⱼ)
                           (> (elt xs i) key)) do
                (setf (elt xs (+ i 1)) (elt xs i))       ; c5        ∑(j=2, n)(tⱼ - 1)
                (decf i))                                ; c6        ∑(j=2, n)(tⱼ - 1)
          (setf (elt xs (+ i 1)) key)))                  ; c7        n - 1
 xs)

;; sum them together, this is the total cost
; T(n) = c1 (n) + c2 (n - 1) + c3 (n - 1) + c4 ∑(j=2, n)(tⱼ) +
;                 c5 ∑(j=2, n)(tⱼ - 1) + c6 ∑(j=2, n)(tⱼ - 1) + c7 (n - 1)

;;;; Analysing best case and worst case running time.
;; best case, tᵢ = 1, thus all ∑ are actually constant time, T(n) is now a linear funcion
;; to n. e.g θ(n)
;; worst case, the list is reversely sorted. Thus Tn is a quadratic function of n.
;; we say insertion sort has θ(n²) complexity.

;;;; Order of growth
;; if cₙ is constant, to simply things a bit, we can just set it to 1.
;; the benefit is now we can talk about the rate of growth instead of the actual
;; growth for a specific case.
