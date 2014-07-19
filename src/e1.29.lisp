;Exercise 1.29: 
;
;  Simpson’s Rule is a more accurate method of numerical integration than the
;  method illustrated above.  Using Simpson’s Rule, the integral of a 
;  function f between a and b is approximated as
; 
;   h
;  --- (y₀ + 4 * y₁ + 2 * y₂ + 4 * y₃ + 2 * y₄ +  ... + 2 * yn-2 + 4 * yn-1 + yn),
;   3
;
;  where
;
;      (b - a)
;  h = -------, for some even integer n, and
;         n
;
;  yk = f (a + kh).
;
;  (Increasing n increases the accuracy of the approximation.)
; 
;  Define a procedure that takes as arguments f, a, b, and n and returns the 
;  value of the integral, computed using Simpson’s Rule. Use your procedure
;  to integrate cube between 0 and 1 (with n = 100 and n = 1000), and 
;  compare the results to those of the integral procedure shown above.

;; We begin by defining some utility functions for h and yk.
(defun h-term (a b n)
  (/ (- b a) n))

(defun yk-term (f a k h)
  (funcall f (+ a (* k h))))

;; * - In common lisp, when passing a function as an argument, you use funcall
;; to call it.
 
;; Next, we create a utility function to return a coefficient for our yk terms.
(defun yk-coefficient (k n)
  (cond ((= k 0) 1)
        ((= k n) 1)
        ((evenp k) 2)
        (T 4)))

;; Next, we create a utility function to return a coefficient for our yk terms.
(defun simpsons-rule-integral (f a b n)
  (defun simpsons-iterator (f a k h n accumulator)
    (cond ((= k n) accumulator)
          (T (simpsons-iterator f a (1+ k) h n (+ accumulator
                                             (* (yk-coefficient k n)
                                                (yk-term f a k h)))))))
  (cond ((not (evenp n)) nil)
        ((< n 0) nil)
        (T (let ((h-value (h-term a b n)))
             (* (/ h-value 3)
                (simpsons-iterator f a 0 h-value n 0))))))

;; Now that we have a function to calculate an integral using sympson's rule,
;; we need to define the function we'll perform the integral on.  Here we use
;; cube
(defun cube (x)
  (* x x x ))

;; Now we can perform the integration from 0 to 1 as asked
;; First we integrate with n = 100
(= 37/150 (simpsons-rule-integral #'cube 0 1 100))

;; Next we integrate with n = 1000
(= 749/3000 (simpsons-rule-integral #'cube 0 1 1000))

;; From the text we have that the integral of cube between 0 and 1 is exactly
;; 1/4.  1/4 = 0.25
;; 
;; We can evaluate our answers by converting the ratios to decimal numbers.
(= 0.24666667 (/ 37.0 150))
(= 0.24966666 (/ 749.0 3000))

;; Note that the result with n=1000 is much closer to 0.25 than the result
;; with n = 100.

