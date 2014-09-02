;Exercise 2.34: 
;
;  Evaluating a polynomial in x at a given value of x can be formulated as an
;  accumulation. We evaluate the polynomial
;
;  (an * xⁿ) + (an-1) * xⁿ⁻¹) + ... + a₁x + a₀
;
;  using a well-known algorithm called Horner’s rule, which structures the 
;  computation as
;
;  (... (an * x + a(n-1))x + ... + a₁)x + a₀
;
;  In other words, we start with an, multiply by x, add a(n-1), multiply by x,
;  and so on, until we reach a₀. Fill in the following template to produce
;  a procedure that evaluates a polynomial using Horner’s rule. Assume that
;  the coefficients of the polynomial are arranged in a sequence, from a₀
;  through an.
;
;  (define (horner-eval x coefficient-sequence)
;    (accumulate (lambda (this-coeff higher-terms) <??>)
;                0
;                coefficient-sequence))
;
;  For example, to compute 1 + 3x + 5x³ + x⁵ at x = 2 you would evaluate
;
;  (horner-eval 2 (list 1 3 0 5 0 1))

;; Since CL doesn't have an accumulate, we need that from e2.33
(defun accumulate (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence)
                (accumulate op initial (cdr sequence))))) 

;; Now we may begin
(defun horner-eval (x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;; Test
(= 79 (horner-eval 2 (list 1 3 0 5 0 1)))

;; As a check, we can calculate the answer directly as follows
(defparameter x 2)
(= 79 (+ 1 (* 3 x) (* 5 x x x) (* x x x x x)))
