;Exercise 2.12:
;
;  Define a constructor make-center-percent that takes a center and a 
;  percentage tolerance and produces the desired interval. You must also 
;  define a selector percent that produces the percentage tolerance for a
;  given interval. The center selector is the same as the one shown above.

;; From the text (section 2.1.4), we are given
;; 
;; (define (make-center-width c w)
;;   (make-interval (- c w) (+ c w)))
;; (define (center i)
;;   (/ (+ (lower-bound i) (upper-bound i)) 2))
;; (define (width i)
;;   (/ (- (upper-bound i) (lower-bound i)) 2))
;;
;; First, we bring in definitions for make-interval from e2.07
(defun make-interval (a b) (cons a b))
(defun lower-bound (a) (car a))
(defun upper-bound (a) (cdr a))

;; Next we convert the given formulas to CL
(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))
(defun center (i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defun width (i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; For a percentage tolerance, the bounds can be calculated as
;; 
;; bound = center ± (center * (percent / 100)),
;; 
;; Where '-' calculates the lower-bound and '+' calculates the upper-bound
;; 
;; Our version of make-center-percent is as follows
(defun make-center-percent (c p)
  (make-interval (- c (* c (/ p 100))) (+ c (* c (/ p 100)))))

;; To calculate the percent, we calculate the ratio of the width to the mid-
;; point (center) of the interval and then multiply by 100 to obtain a 
;; percentage
(defun percent (i)
  (* (/ (width i) 
        (center i)) 
     100))

;; Testing
(= 10 (percent (make-center-percent 10 10)))
(= 10 (percent (make-center-percent 100 10)))
(= 10 (percent (make-center-percent 1000 10)))
(= 5 (percent (make-center-percent 1000 5)))

;; This fails due to rounding errors.  In this case percent returns 7.5000005
;; I'm sure there is a way to account for this, but I don't know enough CL yet.
(= 7.5 (percent (make-center-percent 1000 7.5)))

