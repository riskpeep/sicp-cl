;Exercise 1.33:
;
;  You can obtain an even more general version of accumulate (Exercise 1.32)
;  by introducing the notion of a filter on the terms to be combined. That is,
;  combine only those terms derived from values in the range that satisfy a 
;  specified condition. The resulting filtered-accumulate abstraction takes
;  the same arguments as accumulate, together with an additional predicate of
;  one argument that specifies the filter. Write filtered-accumulate as a
;  procedure.  Show how to express the following using filtered accumulate:
;
;  a. the sum of the squares of the prime numbers in the interval a to b
;  (assuming that you have a prime? predicate already written) 
;
;  b. the product of all the positive integers less than n that are relatively
;  prime to n (i.e., all positive integers i < n such that GCD(i; n) = 1).
;

;; From e1.32 we have
;; (defun accumulate (combiner null-value term a next b)
;;   (defun iter (a result)
;;     (if (> a b)
;;      result
;;      (iter (funcall next a) (funcall combiner result (funcall term a)))))
;;   (iter a null-value))
;;
;; We can modify this procedure to include a filter as follows
(defun filtered-accumulate (filter combiner null-value term a next b)
  (defun iter (a result)
    (if (> a b)
     result
     (if (funcall filter a)
       (iter (funcall next a) (funcall combiner result (funcall term a)))
       (iter (funcall next a) result))))
  (iter a null-value))

;; Now we may address a.
;;
;;  a. the sum of the squares of the prime numbers in the interval a to b
;;  (assuming that you have a prime? predicate already written)
;;
;; First from e1.28 we have functions for square and primality.
(defun square (a)
   (* a a))

(defun expmod (base exp m)
   (cond ((= exp 0) 1)
         ((evenp exp)
          (let* ((a (expmod base (/ exp 2) m))
                 (b (rem (square a) m)))
            (if (and (= 1 b)
                     (not (= (1- m) a))
                     (not (= 1 a)))
              0
              b)))
         (T
             (rem
                 (* base (expmod base (- exp 1) m))
                 m))))

(defun miller-rabin-test (n)
   (defun try-it (a)
      (= (expmod a (1- n) n) 1))
   (try-it (+ 1 (random (- n 1)))))

(defun fast-mr-primep (n times)
   (cond ((= times 0) T)
         ((< n 2) nil)
         ((miller-rabin-test n) (fast-mr-primep n (- times 1)))
         (T nil)))

(defun primep (n)
  (fast-mr-primep n 100))

;; Now we implement the required procedure
(defun sum-of-squared-primes (a b)
  (filtered-accumulate #'primep #'+ 0 #'square a #'1+ b))

;; And test it
;; Primes between 1 and 10 are 2, 3, 5 & 7
(= 87 (sum-of-squared-primes 1 10))


;; And b.
;;
;;  b. the product of all the positive integers less than n that are relatively
;;  prime to n (i.e., all positive integers i < n such that GCD(i; n) = 1).
;;
;; For this we must define a filter to identify relative primality
;; CL includes a greatest common divisor function 'gcd,' we'll use that in our
;; test
;; Note that our filter only takes one parameter, yet depends also on the
;; binding of n.  For this to work, we must use block style
(defun product-of-relative-primes (n)
  (defun relative-primep (i)
    (if (= 1 (gcd i n))
      T
      nil))
  (filtered-accumulate #'relative-primep #'* 1 #'identity 1 #'1+ n))

;; Finally, we test our answer
;; Relative primes of 10 between 1 and 10 are 1, 3, 7 & 9
(= 189 (product-of-relative-primes 10))
