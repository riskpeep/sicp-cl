;Exercise 2.8:
;
;  Using reasoning analogous to Alyssa’s, describe how the difference of two
;  intervals may be computed.  Define a corresponding subtraction procedure,
;  called sub-interval.

;; For sub-interval, we're interested in identifying the minimum and maximum
;; possible differences between the two intervals.
;; 
;; The minimum difference would be if the smaller value was at its upper-bound,
;; and the larger value was at its lower bound.
;; The maximum difference would be if the smaller value was at its lower-bound
;; and the larger value was at its upper bound.
;; 
;; We write the sub-interval as follows
(defun sub-interval (a b)
  (let ((sub1 (- (upper-bound a) (lower-bound b)))
        (sub2 (- (lower-bound a) (upper-bound b))))
    (make-interval (min sub1 sub2) (max (sub1 sub2)))))



1
     5------10      15-----20

2
    5-------10
        7---------15

3
    5-------------------20
        10--------15

+
1    (5 + 15)    (10 + 20)
     20-------30

2
    (5 + 7)        (10 + 15)
       12------------25

-
1    (15 - 10)     (20 - 5)
        5--------------15

2    (10 - 7)      (15 - 5)
         3            10

3    (15 - 5)      (20 - 10)
        10            10
