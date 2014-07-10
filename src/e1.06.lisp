;Exercise 1.6:
;
;  Alyssa P. Hacker doesn’t see why if needs to be provided as a special form.
;  “Why can’t I just define it as an ordinary procedure in terms of cond?” she
;  asks. Alyssa’s friend Eva Lu Ator claims this can indeed be done, and she
;  defines a new version of if:
;
;  (define (new-if predicate then-clause else-clause)
;                  (cond (predicate then-clause)
;                        (else else-clause)))
;
;  Eva demonstrates the program for Alyssa:
;
;  (new-if (= 2 3) 0 5)
;  5
;
;  (new-if (= 1 1) 0 5)
;  0
;
;  Delighted, Alyssa uses new-if to rewrite the square-root program:
;
;  (define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter (improve guess x) x)))
;
;  What happens when Alyssa attempts to use this to compute square roots?
;  Explain.

;; The call will never return.
;;
;; Exercise 1.5 demonstrated that Common Lisp uses applicative order 
;; evaluation.  Since new-if is a function call, the call attempts to evaluate 
;; each of the operands prior to applying the operation to the operands.  The
;; else-clause that Alyssa wrote is recursive and evaluation expands into a 
;; call to sqrt-iter which results in another call to new-if, which results 
;; in an attempt to evaluate its operands, which results in a call to 
;; sqrt-iter...infinate recursive evaluation.
;;
;; Eva Lu Ator's demonstrations work because all of the parameters to the call
;; can be successfully evaluated prior to the call and the call operates as
;; expected.
;;

;; Re-writing in Common Lisp yields
(defun new-if (predicate then-clause else-clause)
      (cond (predicate then-clause)
            (T else-clause)))

(= 5 (new-if (= 2 3) 0 5))

(= 0 (new-if (= 1 1) 0 5))

;; Need abs, square, average, improve, and good-enough? functions to be able to re-write Alyssa's
;; function.  Use square from 1.1.4, abs from 1.1.6, and average, improve, and
;; good-enough? from 1.1.7.
(defun square (x)
  (* x x))

;; Change name of abs so we don't collide with the CL abs function
(defun new-abs (x)
  (cond ((< 0 x) x)
        (T (- x))))

(defun average (x y)
      (/ (+ x y) 2))

(defun improve (guess x)
      (average guess (/ x guess)))

(defun good-enough? (guess x)
      (< (new-abs (- (square guess) x)) 0.001))

;; CL version of sqrt-iter
(defun sqrt-iter (guess x)
      (new-if (good-enough? guess x)
              guess
              (sqrt-iter (improve guess x) x)))

;; This will never return, do not evaluate it.
;; (sqrt-iter 1 2)

