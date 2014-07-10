;Exercise 1.7:
;
;  The good-enough? test used in computing square roots will not be very
;  effective for finding the square roots of very small numbers. Also, in real
;  computers, arithmetic operations are almost always performed with limited
;  precision. This makes our test inadequate for very large numbers. Explain
;  these statements, with examples showing how the test fails for small and
;  large numbers. An alternative strategy for implementing good-enough? is
;  to watch how guess changes from one iteration to the next and to stop when
;  the change is a very small fraction of the guess. Design a square-root
;  procedure that uses this kind of end test. Does this work better for small
;  and large numbers?

;; For our work, we'll need some basic functions
;; Re-writing in clojure yields
(defun square (x)
  (* x x))

(defun my-abs (x)
  (cond ((< 0 x) x)
        (T (- x))))

(defun average (x y)
      (/ (+ x y) 2))

(defun improve (guess x)
      (average guess (/ x guess)))

;; And now the methods we care about
(defun good-enough? (guess x)
      (< (my-abs (- (square guess) x)) 0.001))

(defun sqrt-iter (guess x)
   (if (good-enough? guess x)
        guess
       (sqrt-iter (improve guess x) x)))

;; Rename to my sqrt to avoid name collision with CL
(defun my-sqrt (x)
  (sqrt-iter 1.0 x))

(my-sqrt 5)

;; For small numbers (i.e. smaller than 1), the square of the
;; number is less than the original number.  As guess numbers approach zero,
;; the square rapidly becomes small.  In this case, good-enough? will be
;; true, well before the quality of the square-root is good enough.
;;
;; As an example of failing for small numbers, we can see that when guess
;; is slighlty less than 0.04, its square is less than 0.001.  Thus this
;; algorithm will terminate early for any number with a square root smaller
;; than 0.04.  The problem will get successively worse, the smaller the number
;; we start with

;; Not so bad
(square (my-sqrt 0.04))
;; Worse
(square (my-sqrt 0.01))
;; Very wrong
(square (my-sqrt 0.001))

;;
;; For very large numbers, the square of guess may not have sufficient
;; precision to represent all the digits required to enable the difference
;; between itself and the value x to be less than 0.001.  Thus good-enough?
;; will never be true.
;;
;; As an example of large numbers failing, we can see that when numbers lack the
;; precision to represent a difference of 0.001, then the call to sqrt-iter doesn't
;; terminate.
;; Succeeds because we have the precision
(square (sqrt-iter 1 1.0E12))

;; Never returns because good-enough? never returns true
;;(square (sqrt-iter 1 1.0E13))

;; Our new good-enough?
(defun better-good-enough? (guess x)
      (< (my-abs (- guess (improve guess x))) (* guess 0.00001)))

(defun better-sqrt-iter (guess x)
   (if (better-good-enough? guess x)
        guess
       (better-sqrt-iter (improve guess x) x)))

(defun better-sqrt (x)
  (better-sqrt-iter 1.0 x))

;; Our improved version performs better for very small numbers as shown here
;;
(square (sqrt 0.04))

(square (better-sqrt 0.04))

(square (better-sqrt 0.001))


;; For very large numbers this method also performs better and successfully
;; determines square roots in cases where the previous method was unable to.
;;
(square (sqrt 1.0E12))
(square (better-sqrt 1.0E12))
(square (better-sqrt 1.0E13))
