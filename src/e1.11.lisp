;Exercise 1.11:
;
;  A function f is defined by the rule that
;
;          | n if n < 3,
;  f (n) = |
;          | f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3.
;
;  Write a procedure that computes f by means of a recursive process.
;  Write a procedure that computes f by means of an iterative process.
;
;

;; As a recursive process
(defun f-recur (n)
  (cond ((< n 3) n)
        (T       (+ (* 1 (f (- n 1)))
                    (* 2 (f (- n 2)))
                    (* 3 (f (- n 3)))))))

;; See, it works.
(= 1  (f-recur 1))
(= 2  (f-recur 2))
(= 4  (f-recur 3))
(= 11 (f-recur 4))
(= 25 (f-recur 5))

;; As an iterative process
;;
;; To identify the appropriate iterative process, we perform a manual
;; evaluation using the substitution method.
;;
;; (f 4)
;;=(f 3) + 2(f 2) + 3(f 1)
;;=        2(f 2) + 3(f 1) +
;;          (f 2) + 2(f 1) + 3(f 0)
;;
;; (f 5)
;;=(f 4) + 2(f 3) + 3(f 2)
;;=        2(f 3) + 3(f 2) +
;;          (f 3) + 2(f 2) + 3(f 1)
;;=        3(f 3) + 5(f 2) + 3(f 1) +
;;=                 5(f 2) + 3(f 1)
;;                 3((f 2) + 2(f 1) + 3(f 0))
;;=                 5(f 2) + 3(f 1) +
;;                  3(f 2) + 6(f 1) + 9(f 0))
;;=                 8(f 2) + 9(f 1) + 9(f 0))
;;
;; Upon analysis we can see that each of the expansions ultimately results
;; in a(f 2) + b(f 1) + c(f 0).  We can form an iterative process by using
;; three accumulator values to calculate the values for the final x y and z.
;; In the initial step we initialize the accumulators to a=1, b=2, and c=3 to
;; to represent the factors in the formula expansion.  On each successive
;; iteration, we set a'= b + (a * 1), b' = c + (a * 2), and c' = (a * 3).
;; For each repetition we decrement n until n < 3 at which point we evaluate
;; a(f 2) + b(f 1) + c(f 0).  The c term always evaluates to zero, but we
;; include it for clarity and completeness.
;;
(defun f-iter-iter (a b c count)
  (cond ((< count 3) (+ (* a 2)
                        (* b 1)
                        (* c 0)))
        (T           (f-iter-iter (+ b (* a 1))
                                  (+ c (* a 2))
                                  (* a 3)
                                  (- count 1)))))

(defun f-iter (n)
  (cond ((< n 3) n)
        (T       (f-iter-iter 1 2 3 (- n 1)))))

;; See, it works.
(= 1  (f-iter 1))
(= 2  (f-iter 2))
(= 4  (f-iter 3))
(= 11 (f-iter 4))
(= 25 (f-iter 5))
