;Exercise 1.45:
;
;  We saw in Section 1.3.3 that attempting to compute square roots by naively 
;  finding a fixed point of y → x/y does not converge, and that this can be 
;  fixed by average damping. The same method works for finding cube roots as 
;  fixed points of the average-damped y → x/y². Unfortunately, the process 
;  does not work for fourth roots — a single average damp is not enough to 
;  make a fixed-point search for y → x/y³ converge. On the other hand, if we
;  average damp twice (i.e., use the average damp of the average damp of 
;  y → x/y³) the fixed-point search does converge.  Do some experiments to 
;  determine how many average damps are required to compute nth roots as a 
;  fixedpoint search based upon repeated average damping of y → x/y^n-1. Use
;  this to implement a simple procedure for computing nth roots using 
;  fixed-point, average-damp, and the repeated procedure of Exercise 1.43. 
;  Assume that any arithmetic operations you need are available as primitives.

;; First we bring over compose and repeated from e1.41 & e1.43
(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

(defun repeated (f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

;; Next we define average-damp.
(defun average-damp (f)
  (lambda (x) 
    (/ (+ x 
          (funcall f x))
       2.0)))

;; From e1.36, we have fixed point
(defparameter tolerance 0.00001)
(defun fixed-point (f first-guess)
  (labels ((close-enough? (v1 v2)
             (< (abs (- v1 v2))
                tolerance))
           (try (guess)
             (let ((next (funcall f guess)))
               (print next)
               (if (close-enough? guess next)
                 next
                 (try next)))))
    (try first-guess)))

;; Lets define an nth root function
(defun nth-root (x n damp-fn)
  (labels ((root-fn (y) (/ x (expt y (1- n)))))
    (fixed-point (funcall damp-fn #'root-fn)
                 1.0)))

;; First we check y → x/y
(= (sqrt 5) (nth-root 5 2 #'average-damp))

;; Cube root
(nth-root 5 3 #'average-damp)

;; Quad root
;; Converges, but only if we repeat the average-damping
(= 1.4953488 (nth-root 5 4 (repeated #'average-damp 2)))

;; Need 3rd level of damping now
(= 1.2228446 (nth-root 5 8 (repeated #'average-damp 3)))

;; Need 4th level of damping now
(= 1.105823 (nth-root 5 16 (repeated #'average-damp 4)))

;; Need 5th level of damping now
(= 1.0515811 (nth-root 5 32 (repeated #'average-damp 5)))

;; On inspection, we note that the damping required for the nth root is the
;; floor (largest mathematical integer) of log of the nth value divided by
;; the log of 2, as
;; 
;; damping = floor( log(n) / log(2) )
;; 
(defun nth-root-improved (x n)
  (labels ((root-fn (y) (/ x (expt y (1- n)))))
    (fixed-point (funcall (repeated #'average-damp (floor (/ (log n) (log 2)))) #'root-fn)
                 1.0)))

;; Finally we can test our new simple nth-root-improved
(= 1.4953488 (nth-root-improved 5 4))
(= 1.2228446 (nth-root-improved 5 8))
(= 1.105823 (nth-root-improved 5 16))
(= 1.0515811 (nth-root-improved 5 32))
