;;;; it's usually doesn't worth it to use merge sort for small list.
;; The mit algo book says the constant factor makes it not worth it, but
;; I can't find the constant part from the running time analysis.
;; Although the best case for insertion sort is θ(n), but we can't rely on
;; the best case.
;; The more convincible explaination is recursive algorihtm needs to allocate
;; extra stack frame, the allocation step makes it slower.
;; Consider a list with size n. For an insertion sort, it either swap
;; it's element or not, it's really constant time. If you use merge sort
;; you need to split the list into two lists with only one input and merge them.
;; that makes it slower.

;; Normally we perform some cut off on merge sort, as when n is smaller then
;; certain threshold we switch to insertion sort to get the best of both world.

;; we need an mutation based insertion sort that can sort any part of a list
(defun insertion-sort (xs p q)
  (declare (type sequence xs)
           (type integer p q))
  (let* ((key 0))
    (loop for j from (+ p 1) to q
          for i = (- j 1) do
          (setf key (elt xs j))
          (loop while (and (>= i p)
                           (> (elt xs i) key)) do
                (setf (elt xs (+ i 1)) (elt xs i))
                (decf i))
          (setf (elt xs (+ i 1)) key)))
  xs)

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
        (if (> (- r p) 5)   ;; merge sort for large n
            (progn
              (setf q (floor (/ (+ p r) 2)))
              (merge-sort-* xs p q)
              (merge-sort-* xs (+ q 1) r)
              (merge-* xs p q r))
            (insertion-sort xs p r))))  ;; insertion sort for small n
  xs)

(defun merge-sort (xs)
  (declare (type sequence xs)
           (type number p r))
  (merge-sort-* xs 0 (- (length xs) 1)))

(let ((xs '(908 23 24 189 890 9734 310 904 8 293 3 43 45 2 3 423 4)))
  (merge-sort xs))
