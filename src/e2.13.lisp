;Exercise 2.13:
;
;  Show that under the assumption of small percentage tolerances there is a
;  simple formula for the approximate percentage tolerance of the product of
;  two intervals in terms of the tolerances of the factors. You may simplify
;  the problem by assuming that all numbers are positive.

;;
;; From e2.11, we see that for positive intervals, their product is calculated
;; as 
;;
;;      (make-interval (* (lower-bound x) (lower-bound y))
;;                     (* (upper-bound x) (upper-bound y))
;;
;;
;; Before we begin, we bring in definitions for make-interval procedures from 
;; e2.07 - e2.12
(defun make-interval (a b) (cons a b))
(defun lower-bound (a) (car a))
(defun upper-bound (a) (cdr a))

(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))
(defun center (i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(defun width (i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(defun make-center-percent (c p)
  (make-interval (- c (* c (/ p 100))) (+ c (* c (/ p 100)))))
(defun percent (i)
  (* (/ (width i) (center i)) 100.0))
(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
;;
;; Performing a few experiments
(= 5.99204 (percent (mul-interval (make-center-percent 100 2) (make-center-percent 50 4))))
(= 8.982037 (percent (mul-interval (make-center-percent 1000 4) (make-center-percent 500 5))))
(= 8.982037 (percent (mul-interval (make-center-percent 10 4) (make-center-percent 5 5))))

;; We see that the tolerance of the product is very close to the sum of the 
;; tolerances of the multiplicands for small variances.
;; 
;; p = p₁ + p₂
;; 
;; Let us examine why this works...
;;
;; If we examine the percent procedure, we see that it results in this equation
;;
;;            ub - lb   / ub + lb
;; percent = --------- / --------- * 100
;;               2    /      2
;;
;; By simplifying, we obtain the following
;; 
;;            ub - lb        2
;; percent = --------- * --------- * 100
;;               2        ub + lb
;;
;;            2 * (ub - lb)
;; percent = --------------- * 100
;;            2 * (ub + lb)
;;
;;            (ub - lb)
;; percent = ----------- * 100
;;            (ub + lb)
;;
;; Using substitution, we can see how the percent of the product relates to the
;; bounds of the multipicands
;;
;;            (ubx * uby) - (lbx * lby)
;; percent = ----------------------------- * 100
;;            (ubx * uby) + (lbx * lby)
;;
;;
;; Next, we proceed from our expected result and work backwards.  Based on our
;; experiments and conjectured equation for the tolerance of the product of two
;; intervals, we can express the tolerance of the product as follows
;; 
;;                         (ubx - lby)           (uby - lby)
;; percent x + percent y = ------------- * 100 + ------------- * 100
;;                         (ubx + lby)           (uby + lby)
;;
;; We proceed via simplification
;; 
;;                          (uby + lby) * (ubx - lbx)     (ubx + lbx) * (uby - lby)
;; percent x + percent y = --------------------------- + --------------------------- * 100
;;                          (uby + lby) * (ubx + lbx)     (ubx + lbx) * (uby + lby)
;;
;;                          ((uby + lby) * (ubx - lbx)) + ((ubx + lbx) * (uby - lby))
;; percent x + percent y = ----------------------------------------------------------- * 100
;;                                          (uby + lby) * (ubx + lbx)
;;
;;                                       2(ubx * uby) - 2(lby * lbx)
;; percent x + percent y = ------------------------------------------------------- * 100
;;                          (uby * ubx) + (uby * lbx) + (lby * ubx) + (lby * lbx)
;;
;;                                       2(ubx * uby) - 2(lby * lbx)
;; percent x + percent y = ------------------------------------------------------- * 100
;;                          (ubx * uby) + (lbx * uby) + (ubx * lby) + (lbx * lby)
;;
;;                                      2((ubx * uby) - (lby * lbx))
;; percent x + percent y = ------------------------------------------------------- * 100
;;                          (ubx * uby) + (ubx * lby) + (lbx * uby) + (lbx * lby)
;;
;; Now, for small tolerances, the upper bound is not much different from the
;; lower bound. Thus
;; 
;; (ubx * lby) ~ (ubx * uby), and
;; (lbx * uby) ~ (lbx * lby)
;;
;;                                      2((ubx * uby) - (lby * lbx))
;; percent x + percent y ~ ------------------------------------------------------- * 100
;;                          (ubx * uby) + (ubx * uby) + (lbx * lby) + (lbx * lby)
;;
;;                          2((ubx * uby) - (lby * lbx))
;; percent x + percent y ~ ------------------------------ * 100
;;                          2((ubx * uby) + (lbx * lby))
;;
;;
;;                          (ubx * uby) - (lby * lbx)
;; percent x + percent y ~ ------------------------------ * 100
;;                          (ubx * uby) + (lbx * lby)
;;
;; This result is identical to the expected percent of the product of two 
;; intervals

