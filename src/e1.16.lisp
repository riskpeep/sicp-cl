;Exercise 1.16:
;
;  Design a procedure that evolves an iterative exponentiation process that
;  uses successive squaring and uses a logarithmic number of steps, as does
;  fast-expt. (Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2),
;  keep, along with the exponent n and the base b, an additional state variable
;  a, and define the state transformation in such a way that the product ab^n
;  is unchanged from state to state. At the beginning of the process a is taken
;  to be 1, and the answer is given by the value of a at the end of the
;  process. In general, the technique of defining an invariant quantity that
;  remains unchanged from state to state is a powerful way to think about the
;  design of iterative algorithms.)

;; The text provides a recursive implementation of fast-expt.  We are asked
;; here to implement an iterative implementation.
;;
;; We are given the observation that (b^(n/2))^2 = (b^2)^(n/2), and also that
;; we should transform state in such a way that ab^n is unchanged state to
;; state. Finally we are given that our function should begin with a = 1 and
;; and end with a = answer.
;;
;; We observe that
;;
;; ab^n = b^n if   a = 1 for all n,  (this is our start case)
;; ab^n = a   if b^n = 1 for n = 0.  (this is the end)
;;
;; ab^n = a'b^n' (for the interim cases)
;;
;; From the text (sec 1.2.4), and using the transform given in the hint, we
;; also have
;;
;; b^n = (b^(n/2))^2
;; b^n = (b^2)^(n/2)
;;
;; and
;;
;; b^n = b^(n-1)b
;;
;; Given these relations, observe that we can successively simplify the
;; problem by applying the power n/2 to the square of our base. So long
;; as n is evenly divisible by 2, we can transform the problem into
;; successively simpler cases of (b^2)^(n/2). In cases where n is odd (and thus
;; is not evenly divisible by 2), we use b * b^(n-1) = b^n to update the
;; value in a.  Thus a acts as an accumulator and captures all the earlier
;; simplifications in b^n.
;;
(require ['clojure.math.numeric-tower :as 'math])

(defun my-evenp (n)
   (= (rem n 2) 0))

(defun fast-expt-iter (b n a)
   (cond ((= n 0) a)
         ((not (my-evenp n)) (fast-expt-iter b (- n 1) (* b a)))
         (T (fast-expt-iter (* b b) (/ n 2) a))))

;; And some sugar to kick it .off
(defun fast-expt-i (b n)
   (fast-expt-iter b n 1))

(= 1 (fast-expt-i 2 0))
(= 2 (fast-expt-i 2 1))
(= 4 (fast-expt-i 2 2))
(= 16 (fast-expt-i 2 4))
(= 9 (fast-expt-i 3 2))
(= 27 (fast-expt-i 3 3))

