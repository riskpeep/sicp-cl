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

(fast-timed-prime-test 1000000007 1000)   ;; ~varies
(fast-timed-prime-test 1000000009 1000)   ;; ~varies
(fast-timed-prime-test 1000000021 1000)   ;; ~varies
(fast-timed-prime-test 10000000019 1000)  ;; ~varies
(fast-timed-prime-test 10000000033 1000)  ;; ~varies
(fast-timed-prime-test 10000000061 1000)  ;; ~varies
(fast-timed-prime-test 100000000003 1000) ;; ~varies
(fast-timed-prime-test 100000000019 1000) ;; ~varies
(fast-timed-prime-test 100000000057 1000) ;; ~varies

;;  Since the Fermat test has theta(log n) growth, how would you expect the
;;  time to test primes near 1,000,000 to compare with the time needed to 
;;  test primes near 1000? Do your data bear this out? Can you explain any
;;  discrepancy you find?
;;
;;    Given that the Fermat test has theta(log n) growth we would expect that
;;    the time to test primes near 1,000,000 would be 
;;
;;    (- (log 1,000,000) - (log 1,000))
;;    
;;    times slower than the time to test primes near 1000.
;;
(= 6.9077554 (- (log 1000000) (log 1000)))
;;
;;    However, in practice, we find the times to test any of the primes varies
;;    from being much lower than expected, to much longer than expected.  This
;;    is due to the introduction of random number selction into the testing 
;;    procedure.  Since the Fermat test is not an exhaustive test, the number
;;    of tests is always likely to be much lower than the total range of
;;    possible numbers.  This plus the fact that the time to test a number is
;;    based on the selected number means that if the random selection is a low
;;    number, then the test proceeds quickly.  If it is a large number it 
;;    proceeds more slowly.  Given a selection of number that comprises the
;;    sample for the test, if the sample skews towards lower numbers or higher
;;    numbers, the reported time will be lower or higher respectively.
;;    
;;    Taken together, this means that comparing the expected differences in 
;;    time we see widely varying differences.  One method to correct this
;;    would be to run a large number of samples for each calculation and take
;;    an average of the test times, on the expectation that random samples
;;    will tend on average to cover the entire range, and thus would exhibit
;;    consistent behavior in the aggregate.

