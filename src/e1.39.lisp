;Exercise 1.39:
;
;  A continued fraction representation of the tangent function was published in
;  1770 by the German mathematician J.H. Lambert:
;
;                  x
; tan x = -------------------,
;                    x²
;          1 - --------------
;                      x²
;               3 - ---------
;
;                    5 - ...
;
; where x is in radians. Define a procedure (tan-cf x k) that computes an 
; approximation to the tangent function based on Lambert’s formula. k 
; specifies the number of terms to compute, as in Exercise 1.37.

;; We begin by noting that the continued fraction form here follows the form
;; we used in 1.37 with the exception that addition is replaced by subtraction
;; in the fraction.  As such we can use a modified form of the cont-frac 
;; function that takes in a combinter method.  In addition, we must define
;; implementations for Ni and Di.
;; 
;; From e1.37, we have cont-frac, we modify it to add combiner (c) as follows
(defun cont-frac-enh (n d c k)
  (labels ((iter (n d c k current)
             (if (= k current)
               (funcall c (funcall d (1- current)) 
                        (/ (funcall n current)
                           (funcall d current)))
               (funcall c (funcall d (1- current))
                        (/ (funcall n current)
                           (iter n d c k (1+ current)))))))
    (/ (funcall n 1) (iter n d c k 2))))


;; Now for Ni, and Di, by inspection, we note that
;; 
;; Ni = x² for all i > 1, and x for i = 1
;; Di = (2 * i) - 1 for all i
;;
(defun di-term (i) 
  (- (* 2 i) 1))

(defun tan-cf (x k) 
  (labels ((ni-term (i)
             (if (= i 1)
               x
               (* x x)))
           (di-term (i)
             (- (* 2 i) 1)))
    (cont-frac-enh #'ni-term
                   #'di-term
                   #'-
                   k)))

;; And to test
;; CL has a TAN function that takes radians as well, so we can use it to check
;; our work.
;; 
;; (tan 1.0) = 1.574077
(= 1.5574079 (tan-cf 1.0 6))

