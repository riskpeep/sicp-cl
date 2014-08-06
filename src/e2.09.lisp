;Exercise 2.9: 
;
;  The width of an interval is half of the difference between its upper and 
;  lower bounds. The width is a measure of the uncertainty of the number 
;  specified by the interval. For some arithmetic operations the width of the
;  result of combining two intervals is a function only of the widths of the 
;  argument intervals, whereas for others the width of the combination is not 
;  a function of the widths of the argument intervals. Show that the width of 
;  the sum (or difference) of two intervals is a function only of the widths of
;  the intervals being added (or subtracted). Give examples to show that this
;  is not true for multiplication or division.

;; Note, in the following description, for brevity, we replace lower-bound 
;; with lb and upper-bound with ub
;; 
;; We begin by examining addition.  The text provides the following definition
;; for addition:
;; 
;; (define (add-interval x y)
;;   (make-interval (+ (lb x) (lb y))
;;                  (+ (ub x) (ub y))))
;;
;; Based on this definition, we note that the resulting interval will have the
;; following form
;;
;; ((lb x) + (lb y) , (ub x) + (ub y))
;;
;; The width of the resulting interval then, is calculated as follows
;; 
;;          (((ub x) + (ub y)) - ((lb x) + (lb y)))
;; width = -----------------------------------------
;;                             2
;; 
;; This definition shows that the interval for addition is solely a function of
;; the bounds of the addition argument intervals.
;; 
;; Next, we examing subtraction.  From e2.08, we have
;; 
;; (defun sub-interval (minuend subtrahend)
;;   (make-interval (- (lower-bound minuend) (upper-bound subtrahend))
;;                  (- (upper-bound minuend) (lower-bound subtrahend))))
;;
;; Again, we note that the resulting interval can be expressed as follows
;;
;; ((lb minuend) - (ub subtrahend) , (ub minuend) - (lb subtrahend))
;;
;; The width of the resulting interval then, is calculated as follows
;;
;;          (((ub minuend) - (lb subtrahend)) - ((lb minuend) - (ub subtrahend)))
;; width = -----------------------------------------------------------------------
;;                                            2
;;
;; Again, we see that the interval for subtraction is solely a function of the
;; bounds of the addition argument intervals.
;; 
;; For multiplication, we have 
;; 
;; (define (mul-interval x y)
;;   (let ((p1 (* (lower-bound x) (lower-bound y)))
;;         (p2 (* (lower-bound x) (upper-bound y)))
;;         (p3 (* (upper-bound x) (lower-bound y)))
;;         (p4 (* (upper-bound x) (upper-bound y))))
;;     (make-interval (min p1 p2 p3 p4)
;;                    (max p1 p2 p3 p4))))
;;
;; In this case, we see that the procedure calculates 4 potential terms for
;; lower and upper bounds of the range.  The reason for this is to accommodate
;; for the possibility of negative intervals and the impact they have on 
;; multiplication.  One huge ramification of this is that multiplying 
;; comparatively similar intervals when one interval has negative range has
;; a larger effect that expected on the resulting intervals width.
;; 
;; The easiest way to see this effect is by example
;; 
;; CL-USER> (mul-interval (make-interval 1 10) (make-interval 10 20))
;; (10 . 200)
;; CL-USER> (mul-interval (make-interval -1 10) (make-interval 10 20))
;; (-20 . 200)
;; CL-USER> (mul-interval (make-interval 3 10) (make-interval 10 20))
;; (30 . 200)

;; In the first scenario, the width is 95
;; In the second scenario, the width is 110
;; In the third scenario, the width is 85

;; The difference between the first and second widths is 15, and the difference
;; first and third ranges is just 10, yet the difference between their
;; corresponding intervals is the same (- 20 lower-bound), and thus one would
;; expect that the differences it width would be similarly altered.
;; 
;; 
