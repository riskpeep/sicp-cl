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

(= 3 (next 2))
(= 5 (next 3))
(= 7 (next 5))
(= 9 (next 7))

;; And a modified find-divisor
(defun find-divisor (n test-divisor)
   (cond ((> (square test-divisor) n) n)
         ((dividesp test-divisor n) test-divisor)
         (T (find-divisor n (next test-divisor)))))

(defun smallest-divisor (n)
   (find-divisor n 2))

;; Implementing the timing code
;; TODO

;; Now we test each of the 12 primes with our new function
;; TODO

;; Analyzing the results, we see TODO
