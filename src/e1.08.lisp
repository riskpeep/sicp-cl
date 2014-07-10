;Exercise 1.8:
;
;  Newton’s method for cube roots is based on the fact that if y is an
;  approximation to the cube root of x, then a better approximation is
;  given by the value
;
;   x/y^2 + 2y
;   -----------
;        3     .
;
;  Use this formula to implement a cube-root procedure analogous to the
;  square-root procedure. (In Section 1.3.4 we will see how to implement
;  Newton’s method in general as an abstraction of these square-root and
;  cube-root procedures.)

;; For our work, we'll need some basic functions
;; Re-writing in CL yields
(defun square (x)
  (* x x ))

(defun cube (x)
  (* x x x))

(defun my-abs (x)
  (cond ((< 0 x) x)
        (T (- x))))

;; And now the methods we care about
(defun improve (guess x)
      (/ (+ (/ x (square guess))
            (* 2 guess))
         3))

(defun good-enough? (guess x)
      (< (my-abs (- guess (improve guess x))) (* guess 0.00001)))

(defun cube-iter (guess x)
   (if (good-enough? guess x)
        guess
       (cube-iter (improve guess x) x)))

(defun cube-rt (x)
  (cube-iter 1.0 x))

;; Here it works
(cube (cube-rt 5))
(cube (cube-rt 2))

;; Very small numbers
(cube (cube-rt 0.001))
(cube (cube-rt 0.0001))

;; Very large numbers
(cube (cube-rt 1.0E12))
(cube (cube-rt 1.0E13))
