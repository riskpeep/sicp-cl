;Exercise 1.12:
;
;  The following pattern of numbers is called Pascal’s triangle.
;
;                        1
;                      1   1
;                    1   2   1
;                  1   3   3   1
;                1   4   6   4   1
;
;  The numbers at the edge of the triangle are all 1, and each number inside
;  the triangle is the sum of the two numbers above it.  Write a procedure
;  that computes elements of Pascal’s triangle by means of a recursive process.
;

;; For computation, we can consider the triangle to look like so:
;;
;;               1
;;               1   1
;;               1   2   1
;;               1   3   3   1
;;               1   4   6   4   1
;;
;; In this configuration, the row indicates the 'depth' paramenter
;; starting at 1, and the nth item is the 'element' we're calculating.
;;
;; With that, we can write a recursive method that calculates a single
;; element when passed a 'depth' and 'element' to calculate.
;;
(defun pascals-elements (depth element)
  (cond ((> element depth)  nil)
        ((< element 1) nil)
        ((< depth 1) nil)
        ((= depth 1) 1)
        ((= element 1) 1)
        ((= element depth) 1)
        (T (+ (pascals-elements (- depth 1) (- element 1))
              (pascals-elements (- depth 1) element)))))

;; Using the function, we can evaluate the pyramid shown above like so:
(pascals-elements 1 1)

(pascals-elements 2 1)
(pascals-elements 2 2)

(pascals-elements 3 1)
(pascals-elements 3 2)
(pascals-elements 3 3)


(pascals-elements 4 1)
(pascals-elements 4 2)
(pascals-elements 4 3)
(pascals-elements 4 4)

(pascals-elements 5 1)
(pascals-elements 5 2)
(pascals-elements 5 3)
(pascals-elements 5 4)
(pascals-elements 5 5)

;; And some edge cases
(pascals-elements 0 0)
(pascals-elements 1 0)
(pascals-elements 1 -1)
(pascals-elements 2 0)
(pascals-elements 2 3)
