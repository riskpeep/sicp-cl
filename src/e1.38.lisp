;Exercise 1.38:
;
;  In 1737, the Swiss mathematician Leonhard Euler published a memoir De 
;  Fractionibus Continuis, which included a continued fraction expansion for
;  e - 2, where e is the base of the natural logarithms. In this fraction, the
;  Ni are all 1, and the Di are successively
;  1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, : : :.
;
;  Write a program that uses your cont-frac procedure from Exercise 1.37 to 
;  approximate e, based on Euler’s expansion.

;; From e1.37, we have
(defun cont-frac (n d k)
  (labels ((iter (n d k current)
             (if (= k current)
               (+ (funcall d (1- k)) 
                  (/ (funcall n k)
                     (funcall d k)))
               (+ (funcall d (1- k))
                  (/ (funcall n k)
                     (iter n d k (1+ current)))))))
    (/ (funcall n 1) (iter n d k 2))))

;; In this exercise, the challenge is writing a function to calculate the Di
;; terms.  We begin by noting that the Di sequence is all 1's except for
;; every third term is a larger number.  Each successive number term increases
;; by two from the previous term.  We could express this as number-term = (* 2 i)
;; given a definition for i.  Looking back at the sequence we note that the 2nd,
;; 5th, 8th, etc. terms are the number terms.  Increasing the term number by one
;; yields a number evenly divisible by 3, and that dividing the term by 3 yields
;; the i value we desire.  We can implement this function as follows
(defun di-term (x)
  (if (= 0 (rem (1+ x) 3))
    (* 2 (/ (1+ x) 3))
    1))

;; Now we may proceed with calculating e as requested.
(defun calc-e ()
  (+ 2
     (cont-frac-iter (lambda (i) 1.0)
                #'di-term 
                11)))

;; And here we show that it works
(= 2.7182817 (calc-e))

