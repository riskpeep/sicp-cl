;Exercise 1.15:
;
;  The sine of an angle (specified in radians) can be computed by making use
;  of the approximation sin x ~ x if x is sufficiently small, and the
;  trigonometric identity
;
;                 x             x
;  sin x = 3 sin --- - 4 sin^3 ---
;                 3             3
;
;  to reduce the size of the argument of sin. (For purposes of this exercise
;  an angle is considered “sufficiently small” if its magnitude is not greater
;  than 0.1 radians.) These ideas are incorporated in the following
;  procedures:
;
;  (define (cube x) (* x x x))
;  (define (p x) (- (* 3 x) (* 4 (cube x))))
;  (define (sine angle)
;     (if (not (> (abs angle) 0.1))
;         angle
;         (p (sine (/ angle 3.0)))))
;
;  a. How many times is the procedure p applied when (sine 12.15) is
;     evaluated?
;  b. What is the order of growth in space and number of steps (as a function
;     of a) used by the process generated by the sine procedure when (sine a)
;     is evaluated?

;; Re-writing in CL yields
(defun cube (x) (* x x x))
(defun p (x) (- (* 3 x) (* 4 (cube x))))
(defun sine (angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;; a. How many times is the procedure p applied when (sine 12.15) is
;;    evaluated?
;;
;; We can answer this by performing an execution analysis using substitution
;;
;; (sine 12.15)
;; (p (sine 4.05))
;; (p (p (sine 1.35)))
;; (p (p (p (sine 0.45))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))
;; (p (p (p (p (p 0.05)))))
;; (p (p (p (p (- (* 3 0.05) (* 4 (cube 0.05)))))))
;; (p (p (p (p 0.1495))))
;; (p (p (p (- (* 3 0.14950001) (* 4 (cube 0.14950001))))))
;; (p (p (p 0.4351346)))
;; (p (p (- (* 3 0.4351346) (* 4 (cube 0.4351346))))))
;; (p (p 0.9758465))
;; (p (- (* 3 0.9758465) (* 4 (cube 0.9758465))))
;; (p -0.7895632)
;; (- (* 3 -0.7895632) (* 4 (cube -0.7895632)))
;; -0.39980328
(= -0.39980328 (sine 12.15))

;;
;;
;; p is evaluated 5 times.
;;

;; b. What is the order of growth in space and number of steps (as a function
;;    of a) used by the process generated by the sine procedure when (sine a)
;;    is evaluated?
;;
;; This is a linear recursive process and as such we can expect that it will
;; be O(n) growth in space and number of steps.  To confirm our expectations,
;; we review the sine function and see that it will be evaluated successively
;; until the angle is smaller than 0.1.  For each successive recursion, we
;; divide the angle by 3 and recurse.  We can write this relationship as shown
;; below:
;;
;;      angle
;;    -------- <= 0.1
;;       3^n            , where n equals the number of recursions.
;;
;; If we solve for 'n' we can show the relationship between 'angle' and 'n.'
;;
;; angle <= 0.1 * 3^n
;; angle/0.1 <= 3^n
;; log(base 3)(angle/0.1) <= n
;; n = log(base 3)(angle/0.1)
;;
;; Based on this we can see that
;;
;; Growth in space and time is O(log angle)
;;
