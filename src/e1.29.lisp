;Exercise 1.29: 
;
; Simpson’s Rule is a more accurate method of numerical integration than the
; method illustrated above.  Using Simpson’s Rule, the integral of a 
; function f between a and b is approximated as
; 
;  h
; --- (y₀ + 4 * y₁ + 2 * y₂ + 4 * y₃ + 2 * y₄ +  ... + 2 * yn-2 + 4 * yn-1 + yn),
;  3
;
; where
;
;     (b - a)
; h = -------, for some even integer n, and
;        n
;
; yk = f (a + kh).
;
; (Increasing n increases the accuracy of the approximation.)
; 
; Define a procedure that takes as arguments f, a, b, and n and returns the 
; value of the integral, computed using Simpson’s Rule. Use your procedure
; to integrate cube between 0 and 1 (with n = 100 and n = 1000), and 
; compare the results to those of the integral procedure shown above.

;; We begin by defining some utility functions for h and yk.
(defun h-term (a b n)
  (/ (- b a) n))

;; TODO need to figure out the syntax to pass in a function here
(defun yk-term (f a k h)
  (#'f (+ a (* k h))))

;; TODO notice that the first and last terms are not multiplied.  Need to make that work
(defun simpsons-rule-integral (f a b n)
  (defun simpsons-iterator (f a k n current-term)
    (cond ((= current-term n) nil)
          (T (+ (yk-term f a k n (1+ current-term))
                (yk-term f a k n current-term)
                (simpsons-iterator f a k n (+ 2 current-term))
                ))
          )
    )
  (cond ((not (evenp n))
         nil
         )))
