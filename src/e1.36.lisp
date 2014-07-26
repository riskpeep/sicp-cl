;Exercise 1.36:
;
;  Modify fixed-point so that it prints the sequence of approximations it 
;  generates, using the newline and display primitives shown in Exercise 1.22.
;  Then find a solution to x^x = 1000 by finding a fixed point of
;
;      log(1000)
;  x → ---------.
;       log(x)
;
;  (Use Scheme’s primitive log procedure, which computes natural logarithms.)
;
;  Compare the number of steps this takes with and without average damping. 
;  (Note that you cannot start fixed-point with a guess of 1, as this would 
;  cause division by log(1) = 0.)
;

;; From e1.35, we have a CL fixed point we modify that to print the steps
;; Note that next is given as simply the result from the previous call.  This
;; procedure does not use average damping.  We can modify the method to accept
;; a next-guess function that calculates the next guess for us.  In practice
;; this could be further simplified by changing the passed in function instead
;; however for this exercise we use two methods to make obvious our work with
;; average damping
(defparameter tolerance 0.00001)
(defun fixed-point (f next-guess first-guess)
  (labels ((close-enough? (v1 v2)
             (< (abs (- v1 v2))
                tolerance))
           (try (guess)
             (let ((next (funcall next-guess f guess)))
               (print next)
               (if (close-enough? guess next)
                 next
                 (try next)))))
    (try first-guess)))

;      log(1000)
;  x → ---------.
;       log(x)

;; First we test without average damping
(= 4.5555325 (fixed-point (lambda (x) (/ (log 1000) (log x)))
                          (lambda (f x) (funcall f x))
                          2.0))
;; 34 Steps

;; And next we test with average damping
(= 4.5555377 (fixed-point (lambda (x) (/ (log 1000) (log x)))
                          (lambda (f x) (/ (+ x 
                                                (funcall f x)) 
                                            2.0))
                          2.0))
;; 9 Steps!

;; As expected, the implementation with average damping converges in fewer
;; steps than the implementation without damping.  (9 steps vs. 34 steps)
;; Looking at the actual results, we note that the undamped version bounces
;; from above to below and back getting successively closer.  In contrast,
;; the average damped version starts high and then steps closer and closer
;; to the answer without overshooting.
