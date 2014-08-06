;Exercise 2.8:
;
;  Using reasoning analogous to Alyssa’s, describe how the difference of two
;  intervals may be computed.  Define a corresponding subtraction procedure,
;  called sub-interval.

;; For sub-interval, we're interested in identifying the minimum and maximum
;; possible differences between the two intervals.
;; 
;; We being by recalling that in subtraction, we name the terms of the 
;; subtraction as the Minuend (the value from which we are subtracting) and 
;; the Subtrahend (the (usually smaller) value we are subtracting).
;; 
;; Considering intervals, we observe that the minimum difference would be if 
;; the Minuend was at its upper-bound, and the Subtrahend was at its lower-
;; bound. Conversely, the maximum difference would be if the Minuend interval
;; was at its lower-bound and the Subtrahend interval was at its upper bound.
;;
;; In the case that the Minuend interval is entirely larger (that is to say
;; that the lower-bound of the Minuend is greater than the upper-bound of the
;; Subtrahend), then the situation described above holds.  If, however, there
;; is any overlap between the intervals, or if the Minuend is wholly smaller
;; than the subtrahend, then the result is a negative interval.
;;
;; With this understanding, we proceed as follows
;; 
;; From e2.07 we have
(defun make-interval (a b) (cons a b))
(defun lower-bound (a) (car a))
(defun upper-bound (a) (cdr a))

;; We write the sub-interval as follows
(defun sub-interval (minuend subtrahend)
  (make-interval (- (lower-bound minuend) (upper-bound subtrahend))   
                 (- (upper-bound minuend) (lower-bound subtrahend))))

;; It might be reasonable, given the problem statement, that negative 
;; intervals would be an implausible answer (i.e with respect to resistence of
;; electronics components)  One could achieve this by adding a (max n 0) to
;; the procedure as follows
;; 
;; (defun sub-interval (minuend subtrahend)
;;   (make-interval (max (- (lower-bound minuend) (upper-bound subtrahend)) 0)
;;                  (max (- (upper-bound minuend) (lower-bound subtrahend))) 0))
;;
;; Since this greater than zero requirement is not specifically mentioned, we
;; do not include it as part of our solution

;; Now for testing
(defun print-interval (interval)
  (format t "( ~A - ~A )" (lower-bound interval) (upper-bound interval)))

;; Non-overlapping, expect ( 3 - 15 )
(let ((upper-interval (make-interval 10 20))
      (lower-interval (make-interval 5 7)))
  (print-interval (sub-interval upper-interval lower-interval)))

;; Touching, non-overlapping, expect ( 0 - 15 )
(let ((upper-interval (make-interval 10 20))
      (lower-interval (make-interval 5 10)))
  (print-interval (sub-interval upper-interval lower-interval)))

;; Slightly-overlapping, expect ( 0 - 15 )
(let ((upper-interval (make-interval 10 20))
      (lower-interval (make-interval 5 15)))
  (print-interval (sub-interval upper-interval lower-interval)))

;; Wholly-overlapping, expect ( 0 - 15 )
(let ((upper-interval (make-interval 10 20))
      (lower-interval (make-interval 5 25)))
  (print-interval (sub-interval upper-interval lower-interval)))

;; Subtrahend overlapping and larger, expect ( 0 - 5 )
(let ((upper-interval (make-interval 10 20))
      (lower-interval (make-interval 15 25)))
  (print-interval (sub-interval upper-interval lower-interval)))

;; Subtrahend wholly larger, expect ( 0 - 0 )
(let ((upper-interval (make-interval 10 20))
      (lower-interval (make-interval 25 30)))
  (print-interval (sub-interval upper-interval lower-interval)))

