;Exercise 1.17:
;
;  The exponentiation algorithms in this section are based on performing
;  exponentiation by means of repeated multiplication. In a similar way, one
;  can perform integer multiplication by means of repeated addition. The
;  following multiplication procedure (in which it is assumed that our language
;  can only add, not multiply) is analogous to the expt procedure:
;
;  (define (* a b)
;     (if (= b 0)
;         0
;         (+ a (* a (- b 1)))))
;
;  This algorithm takes a number of steps that is linear in b. Now suppose we
;  include, together with addition, operations double, which doubles an
;  integer, and halve, which divides an (even) integer by 2. Using these,
;  design a multiplication procedure analogous to fast-expt that uses a
;  logarithmic number of steps.

;; Re-writing in CL yields
(defun my-* (a b)
   (if (= b 0)
       0
       (+ a (* a (- b 1)))))

;; First, we provide naieve implementations of double and halve
(defun my-double (a)
   (+ a a))

(defun my-evenp (n)
   (= (rem n 2) 0))

(defun my-halve (a)
   (cond ((my-evenp a) (/ a 2))
         (T a)))

;; From the text we have the following for fast-expt
;; (define (fast-expt b n)
;;    (cond ((= n 0) 1)
;;    ((even? n) (square (fast-expt b (/ n 2))))
;;    (else (* b (fast-expt b (- n 1))))))
;;
;; We can write a similar method for multiplication as follows
(defun fast-* (b n)
   (cond ((= n 0) 0)
         ((my-evenp n) (my-double (fast-* b (/ n 2))))
         (T (+ b (fast-* b (- n 1))))))

(= 0 (fast-* 1 0))
(= 0 (fast-* 0 1))
(= 1 (fast-* 1 1))
(= 2 (fast-* 1 2))
(= 4 (fast-* 2 2))
(= 8 (fast-* 2 4))
(= 16 (fast-* 4 4))
(= 20 (fast-* 4 5))

