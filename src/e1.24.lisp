;Exercise 1.24:
;
;  Modify the timed-prime-test procedure of Exercise 1.22 to use fast-prime?
;  (the Fermat method), and test each of the 12 primes you found in that
;  exercise. Since the Fermat test has theta(log n) growth, how would you
;  expect the time to test primes near 1,000,000 to compare with the time
;  needed to test primes near 1000? Do your data bear this out? Can you explain
;  any discrepancy you find?

;; First, we define some methods we'll need
(defun square (a)
   (* a a))

(defun dividesp (a b)
   (= (rem b a) 0))

;
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

;; Here we rename try-it to avoid a collision with CL try-it function
;; CL rand function is random
(defun fermat-test (n)
   (defun my-try-it (a)
      (= (expmod a n n) a))
   (my-try-it (+ 1 (random (- n 1)))))

(defun fast-primep (n times)
   (cond ((= times 0) T)
         ((fermat-test n) (fast-primep n (- times 1)))
         (T nil)))

(defun report-prime (elapsed-time)
   (format t "*** ~A" elapsed-time))

(defun runtime ()
   (get-internal-real-time))

(defun start-fast-prime-test (n times start-time)
   (if (fast-primep n times)
       (report-prime (- (runtime) start-time))))

(defun fast-timed-prime-test (n times)
   (format t "~%")
   (format t "~A " n)
   (start-fast-prime-test n times (runtime)))

(fast-timed-prime-test 1000000007 10)   ;; ~3   e1.22 ~6
(fast-timed-prime-test 1000000009 10)   ;; ~3   e1.22 ~6
(fast-timed-prime-test 1000000021 10)   ;; ~3   e1.22 ~6
(fast-timed-prime-test 10000000019 10)  ;; ~11  e1.22 ~22
(fast-timed-prime-test 10000000033 10)  ;; ~11  e1.22 ~22
(fast-timed-prime-test 10000000061 10)  ;; ~11  e1.22 ~22
(fast-timed-prime-test 100000000003 10) ;; ~36  e1.22 ~72
(fast-timed-prime-test 100000000019 10) ;; ~36  e1.22 ~72
(fast-timed-prime-test 100000000057 10) ;; ~36  e1.22 ~72


