;Exercise 1.3:
;
;  Define a procedure that takes three numbers as arguments and returns
;  the sum of the squares of the two larger numbers.
(defun sum-of-squares-of-largest (a b c)
  (if (and (< a b) (< a c))
      (+ (* b b) (* c c))
      (if (< b c)
        (+ (* a a) (* c c))
        (+ (* a a) (* b b)))))

;; Exclude first number
(= 13 (sum-of-squares-of-largest 1 2 3))

;; Exclude second number
(= 25 (sum-of-squares-of-largest 4 2 3))

;; Exclude third number
(= 41 (sum-of-squares-of-largest 4 5 3))
