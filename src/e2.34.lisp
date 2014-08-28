;Exercise 2.34: 
;
;  Evaluating a polynomial in x at a given value of x can be formulated as an
;  accumulation. We evaluate the polynomial
;
;  anxn + an-1xn-1 + ... + a₁x + a₀
;
;  using a well-known algorithm called Horner’s rule, which structures the 
;  computation as
;
;  (... (anx + an-1)x + ... + a₁)x + a₀
;
;  In other words, we start with an, multiply by x, add an-1, multiply by x,
;  and so on, until we reach a₀¹⁶. Fill in the following template to produce
;  a procedure that evaluates a polynomial using Horner’s rule. Assume that
;  the coefficients of the polynomial are arranged in a sequence, from a₀
;  through an.
;
;  (define (horner-eval x coefficient-sequence)
;    (accumulate (lambda (this-coeff higher-terms) ⟨??⟩)
;                0
;                coefficient-sequence))
;
;  For example, to compute 1+3x +5x⁵ +x⁵ at x = 2 you would evaluate
;
;  (horner-eval 2 (list 1 3 0 5 0 1))
