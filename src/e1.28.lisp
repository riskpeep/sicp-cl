;Exercise 1.28:
;
;  One variant of the Fermat test that cannot be fooled is called the Miller-
;  Rabin test (Miller 1976; Rabin 1980). This starts from an alternate form of
;  Fermat’s Little Theorem, which states that if n is a prime number and a is
;  any positive integer less than n, then a raised to the (n-1)-st power is
;  congruent to 1 modulo n. To test the primality of a number n by the Miller-
;  Rabin test, we pick a random number a < n and raise a to the (n-1)-st power
;  modulo n using the expmod procedure. However, whenever we perform the
;  squaring step in expmod, we check to see if we have discovered a
;  “nontrivial square root of 1 modulo n,” that is, a number not equal to 1 or
;  n-1 whose square is equal to 1 modulo n. It is possible to prove that if
;  such a nontrivial square root of 1 exists, then n is not prime. It is also
;  possible to prove that if n is an odd number that is not prime, then, for
;  at least half the numbers a < n, computing a^n-1 in this way will reveal a
;  nontrivial square root of 1 modulo n. (This is why the Miller-Rabin test
;  cannot be fooled.) Modify the expmod procedure to signal if it discovers a
;  nontrivial square root of 1, and use this to implement the Miller-Rabin
;  test with a procedure analogous to fermat-test. Check your procedure by
;  testing various known primes and non-primes. Hint: One convenient way to
;  make expmod signal is to have it return 0.
 
;; First, we define some methods we'll need
(defun square (a)
   (* a a))

;; Modifying the expmod from e1.24, we add the check for a congruence to 1
;; modulo n. If a is congruent, we return zero, otherwise we return the 
;; remainder
;; 
;; Here we introduce the common lisp let* macro.  let* produces bindings
;; between one or more identifiers and declaration forms.  let* performs the
;; bindings in sequence and thus successive bindings may refer to previously
;; bound identifiers.  This is different that the standard let form which may
;; use parallel evaluation and does not allow bindings to refer to previously
;; bound items from the same let form.
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

;; CL rand function is random
(defun miller-rabin-test (n)
   (defun try-it (a)
      (= (expmod a (1- n) n) 1))
   (try-it (+ 1 (random (- n 1)))))

(defun fast-mr-primep (n times)
   (cond ((= times 0) T)
         ((miller-rabin-test n) (fast-mr-primep n (- times 1)))
         (T nil)))

;; Testing some non-primes.  We expect fast-mr-primep to return nil
(eql nil (fast-mr-primep 21 100))
(eql nil (fast-mr-primep 201 100))
(eql nil (fast-mr-primep 2001 100))
(eql nil (fast-mr-primep 19999 100))

;; Testing some primes.  We expect fast-mr-primep to return T
(eql T (fast-mr-primep 199 100))
(eql T (fast-mr-primep 1999 100))
(eql T (fast-mr-primep 1000000007 100))
(eql T (fast-mr-primep 1000000009 100))
(eql T (fast-mr-primep 1000000021 100))

;; And to prove that the Miller Rabin test is not fooled by the Carmichael
;; numbers, we'll test those too.  Since they are not prime, we expect 
;; fast-mr-primep to return nil
(eql nil (fast-mr-primep 561 100))
(eql nil (fast-mr-primep 1105 100))
(eql nil (fast-mr-primep 1729 100))
(eql nil (fast-mr-primep 2465 100))
(eql nil (fast-mr-primep 2821 100))
(eql nil (fast-mr-primep 6601 100))
