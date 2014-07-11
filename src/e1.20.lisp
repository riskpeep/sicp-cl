;Exercise 1.20:
;
;  The process that a procedure generates is of course dependent on the rules
;  used by the interpreter. As an example, consider the iterative gcd
;  procedure given above*. Suppose we were to interpret this procedure using
;  normal-order evaluation, as discussed in Section 1.1.5. (The
;  normal-order-evaluation rule for if is described in Exercise 1.5.) Using
;  the substitution method (for normal order), illustrate the process generated
;  in evaluating (gcd 206 40) and indicate the remainder operations that are
;  actually performed. How many remainder operations are actually performed in
;  the normal-order evaluation of (gcd 206 40)? In the applicative-order
;  evaluation?
;
;  * - From the text we have
;  (define (gcd a b)
;     (if (= b 0)
;         a
;         (gcd b (remainder a b))))

;; Recall that in normal-order evaluation, we defer evaluation of function
;; parameters and instead substitute the call in place of the bound variable
;; in the function body.  Only when we have a form with all atomic values do we
;; perform evaluations.  In contrast, with applicative-order evaluation, we
;; evaluate function parameters prior to applying the function to the
;; parameters.
;;
;; First we examine normal-order evaluation. For the purposes of this exercise,
;; we treat remainder as a primitive operation.  In list below, in the first
;; column we show the places and count of evaluations of remainder.
;;
;;       (gcd 206 40)
;;       (if (= 40 0))
;;       (gcd 40 (remainder 206 40))
;;       (if (= (remainder 206 40) 0)
;;  1    (if (= 6 0)
;;       (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;;       (if (= (remainder 40 (remainder 206 40))) 0)
;;  1    (if (= (remainder 40 6) 0))
;;  1    (if (= 4 0)
;;       (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;;       (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0))
;;  2    (if (= (remainder 6 (remainder 40 6)) 0))
;;  1    (if (= (remainder 6 4) 0))
;;  1    (if (= 2 0)
;;       (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;;       (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0))
;;  3    (if (= (remainder (remainder 40 6) (remainder 6 (remainder 40 6)))) 0))
;;  2    (if (= (remainder 4 (remainder 6 4)) 0))
;;  1    (if (= (remainder 4 2) 0))
;;  1    (if (= 0 0))
;;       (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;  2    (remainder 6 (remainder 40 6))
;;  1    (remainder 6 4)
;;  1    2
;; ---
;; 18
;;
;; In normal-order evaluation, remainder is evaluated 18 times.
;;
;; Next we examine applicative-order evaluation.
;;
;;       (gcd 206 40)
;;       (if (= 40 0))
;;       (gcd 40 (remainder 206 40))
;;  1    (gcd 40 6)
;;       (if (= 6 0))
;;       (gcd 6 (remainder 40 6))
;;  1    (gcd 6 4)
;;       (if (= 4 0))
;;       (gcd 4 (remainder 6 4))
;;  1    (gcd 4 2)
;;       (if (= 2 0))
;;       (gcd 2 (remainder 4 2))
;;  1    (gcd 2 0)
;;       (if (= 0 0))
;;       2
;; ---
;;  4
;;
;; In applicative-order evaluation, remainder is evaluated 4 times.
