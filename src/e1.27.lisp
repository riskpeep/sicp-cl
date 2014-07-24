;Exercise 1.27:
;
;  Demonstrate that the Carmichael numbers listed in Footnote 1.47 really do
;  fool the Fermat test. That is, write a procedure that takes an integer n
;  and tests whether an is congruent to a modulo n for every a < n, and try
;  your procedure on the given Carmichael numbers.

;; First, we define some methods we'll need
(defun square (a)
   (* a a))

(defun dividesp (a b)
   (= (rem b a) 0))

;; From the text (Sec 1.26), we have
;;
;; (define (expmod base exp m)
;;    (cond ((= exp 0) 1)
;;          ((even? exp)
;;              (remainder
;;                  (square (expmod base (/ exp 2) m))
;;                  m))
;;          (else
;;              (remainder
;;                  (* base (expmod base (- exp 1) m))
;;                  m))))
;;
;; (define (fermat-test n)
;; (define (try-it a)
;;    (= (expmod a n n) a))
;;    (try-it (+ 1 (random (- n 1)))))
;;
;; (define (fast-prime? n times)
;;    (cond ((= times 0) true)
;;          ((fermat-test n) (fast-prime? n (- times 1)))
;;          (else false)))
;;
;; Re-writing in CL yields
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((evenp exp)
         (rem
           (square (expmod base (/ exp 2) m))
           m))
        (T
         (rem
           (* base (expmod base (- exp 1) m))
           m))))

;; Now we can write a check
(defun fermat-check (n)
  "Tests if a is congruent to a modulo n for all a < a < 0"
  (labels ((try-it (a)
    (cond ((<= a 0) T)
          ((= (expmod a n n) a)
           (print (= a (expmod a n n)))
           (try-it (1- a)))
          (T nil))))
  (try-it (1- n))))

;; First some tests to confirm that everything is working correctly
;; For non-prime numbers numbers we would expect this test to fail
(eql nil (fermat-check 198))
(eql nil (fermat-check 200))

;; For prime numbers numbers we would expect this test to succeed
(eql T (fermat-check 199))
(eql T (fermat-check 1999))

;; Now tests to confirm that the listed Carmichael numbers do fool the Fermat
;; test.  For these numbers, we expect the test to fail since they are not
;; prime, yet they succeed (which makes them Carmichael numbers).
;; 
;; For each of the numbers below, we show that they do indeed fool the test
;; by returning T.  We also show that they are evenly divisible by whole 
;; numbers
;;
(eql T (fermat-check 561))
(= 0 (rem 561 3))
(eql T (fermat-check 1105))
(= 0 (rem 1105 5))
(eql T (fermat-check 1729))
(= 0 (rem 1729 7))
(eql T (fermat-check 2465))
(= 0 (rem 2465 5))
(eql T (fermat-check 2821))
(= 0 (rem 2821 7))
(eql T (fermat-check 6601))
(= 0 (rem 6601 7))
