;Exercise 1.23:
;
;  The smallest-divisor procedure shown at the start of this section does lots
;  of needless testing: After it checks to see if the number is divisible by 2
;  there is no point in checking to see if it is divisible by any larger even
;  numbers. This suggests that the values used for test-divisor should not be
;  2, 3, 4, 5, 6, : : :, but rather 2, 3, 5, 7, 9, : : :. To implement this
;  change, define a procedure next that returns 3 if its input is equal to 2
;  and otherwise returns its input plus 2. Modify the smallest-divisor
;  procedure to use (next test-divisor) instead of (+ test-divisor 1). With
;  timed-prime-test incorporating this modified version of smallest-divisor,
;  run the test for each of the 12 primes found in Exercise 1.22. Since this
;  modification halves the number of test steps, you should expect it to run
;  about twice as fast. Is this expectation confirmed? If not, what is the
;  observed ratio of the speeds of the two algorithms, and how do you explain
;  the fact that it is different from 2?

;; First, we define some methods we'll need
(defun square (a)
   (* a a))

(defun dividesp (a b)
   (= (rem b a) 0))

;; Next we can implement next
(defun next (n)
   (cond ((= n 2) 3)
         (T (+ n 2))))

;; And test it to confirm its behavior
(= 3 (next 2))
(= 5 (next 3))
(= 7 (next 5))
(= 9 (next 7))

;; Next we modify find-divisor and smallest divisor
(defun faster-find-divisor (n test-divisor)
   (cond ((> (square test-divisor) n) n)
         ((dividesp test-divisor n) test-divisor)
         (T (faster-find-divisor n (next test-divisor)))))

(defun faster-smallest-divisor (n)
   (faster-find-divisor n 2))

;; Re-implement primp
(defun faster-primep (n)
   (= n (faster-smallest-divisor n)))

;; Implementing the timing code
(defun runtime ()
   (get-internal-real-time))

(defun report-prime (elapsed-time)
   (format t "*** ~A" elapsed-time))

(defun start-faster-prime-test (n start-time)
   (if (faster-primep n)
       (report-prime (- (runtime) start-time))))

(defun faster-timed-prime-test (n)
   (format t "~%")
   (format t "~A " n)
   (start-faster-prime-test n (runtime)))

;; Now we test each of the 12 primes with our new function
(faster-timed-prime-test 1000000007)   ;; ~3   e1.22 ~6
(faster-timed-prime-test 1000000009)   ;; ~3   e1.22 ~6
(faster-timed-prime-test 1000000021)   ;; ~3   e1.22 ~6
(faster-timed-prime-test 10000000019)  ;; ~11  e1.22 ~22
(faster-timed-prime-test 10000000033)  ;; ~11  e1.22 ~22
(faster-timed-prime-test 10000000061)  ;; ~11  e1.22 ~22
(faster-timed-prime-test 100000000003) ;; ~36  e1.22 ~72
(faster-timed-prime-test 100000000019) ;; ~36  e1.22 ~72
(faster-timed-prime-test 100000000057) ;; ~36  e1.22 ~72

;; Since this modification halves the number of test steps, you should expect
;; it to run about twice as fast. Is this expectation confirmed? If not, what
;; is the observed ratio of the speeds of the two algorithms, and how do you
;; explain the fact that it is different from 2?
;;
;;   Analyzing the results, we see that the results from this exercise take
;;   approximately half the time as the results from exercise e1.22.  This is
;;   the expected result given that the modified procedure reduces the number
;;   of tests (and thus process steps) by roughly half.
;; 
;;   We explain this result as the expected result.  We anticipate that
;;   procedure execution time would scale linearly with the number of performed
;;   procedure steps and it does as shown by this exercise and the previous
;;   one.
