;Exercise 2.14:
;
;  Demonstrate that Lem is right. Investigate the behavior of the system on a 
;  variety of arithmetic expressions.  Make some intervals A and B, and use 
;  them in computing the expressions A/A and A/B. You will get the most 
;  insight by using intervals whose width is a small percentage of the center 
;  value. Examine the results of the computation in center-percent form 
;  (see Exercise 2.12).
;

;; From the text (sec 2.1.4) we have this relevant passage
;; 
;;  After considerable work, Alyssa P. Hacker delivers her finished system.
;;  Several years later, after she has forgotten all about it, she gets a
;;  frenzied call from an irate user, Lem E. Tweakit. It seems that Lem has
;;  noticed that the formula for parallel resistors can be written in two 
;;  algebraically equivalent ways:
;;
;;    R₁R₂
;;  ---------
;;   R₁ + R₂
;;
;;  and
;;  
;;        1
;;  -------------
;;   1/R₁ + 1/R₂
;;
;;  He has written the following two programs, each of which computes the 
;;  parallel-resistors formula differently:
;;
;;  (define (par1 r1 r2)
;;    (div-interval (mul-interval r1 r2)
;;                  (add-interval r1 r2)))
;;  (define (par2 r1 r2)
;;    (let ((one (make-interval 1 1)))
;;      (div-interval
;;        one (add-interval (div-interval one r1)
;;                          (div-interval one r2)))))
;;
;;  Lem complains that Alyssa’s program gives different answers
;;  for the two ways of computing. This is a serious complaint.
;;
;; Before we begin, we bring in definitions for make-interval procedures from 
;; e2.07 - e2.12
(defun make-interval (a b) (cons a b))
(defun lower-bound (a) (car a))
(defun upper-bound (a) (cdr a))
(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(defun div-interval (x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))
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
 
;; Now we can re-write par1 & par2 in CL
(defun par1 (r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(defun par2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one (add-interval (div-interval one r1)
                        (div-interval one r2)))))

;; Now we can test some resistance calculations
(= 14.900747 (percent (par1 (make-center-percent 1000 5)
                            (make-center-percent 500 5))))
(= 4.9999967 (percent (par2 (make-center-percent 1000 5)
                            (make-center-percent 500 5))))

(= 8.318695  (percent (par1 (make-center-percent 2000 1)
                            (make-center-percent 1000 5))))
(= 3.6687977 (percent (par2 (make-center-percent 2000 1)
                            (make-center-percent 1000 5))))

(= 20.729576  (percent (par1 (make-center-percent 5000 7)
                             (make-center-percent 500 7))))
(= 6.999999 (percent (par2 (make-center-percent 5000 7)
                           (make-center-percent 500 7))))

;; From these experiments, we can see that par2 tends to produce expected
;; variances that are much lower than par1.

;; Having demonstrated that Lem is correct, let us examine the divisions A/A
;; and A/B suggested in the problem
(let ((A (make-center-percent 5000 1))
      (B (make-center-percent 2000 2)))
  (= 1.9998007 (percent (div-interval A A)))
  (= 2.9993937 (percent (div-interval A B))))

(let ((A (make-center-percent 1000 2))
      (B (make-center-percent 2000 1)))
  (= 3.9983966 (percent (div-interval A A)))
  (= 2.999404 (percent (div-interval A B)))) 

;; From these experiments, we can see that for small tolerances, the tolerance
;; of a division is roughly equal to the sum of the tolerances of its divisor
;; and dividend.
