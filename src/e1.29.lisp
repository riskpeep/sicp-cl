;Exercise 1.29: 
;
; Simpson’s Rule is a more accurate method of numerical integration than the
; method illustrated above.  Using Simpson’s Rule, the integral of a 
; function f between a and b is approximated as
; 
;  h
; --- (y0 + 4 * y1 + 2 * y2 + 4 * y3 + 2 * y4 +  ... + 2 * yn-2 + 4 * yn-1 + yn),
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
