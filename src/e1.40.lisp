;Exercise 1.40:
;
;  Define a procedure cubic that can be used together with the newtons-method 
;  procedure in expressions of the form
;
;  (newtons-method (cubic a b c) 1)
;
; to approximate zeros of the cubic x3 + ax2 + bx + c.

;; From the text we have newtons-method and all of its dependencies
;;
;; (define tolerance 0.00001)
;; (define (fixed-point f first-guess)
;;   (define (close-enough? v1 v2)
;;     (< (abs (- v1 v2))
;;        tolerance))
;;   (define (try guess)
;;     (let ((next (f guess)))
;;       (if (close-enough? guess next)
;;         next
;;         (try next))))
;;   (try first-guess))
;;
;; (define dx 0.00001)
;; (define (deriv g)
;;   (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
;;
;; (define (newton-transform g)
;;   (lambda (x) (- x (/ (g x) ((deriv g) x)))))
;;
;; (define (newtons-method g guess)
;;   (fixed-point (newton-transform g) guess))
;;
;; Re-writing all of these in CL yields
(defparameter tolerance 0.00001)
(defun fixed-point (f first-guess)
  (labels ((close-enough? (v1 v2)
             (< (abs (- v1 v2))
                tolerance))
           (try (guess)
             (let ((next (funcall f guess)))
               (if (close-enough? guess next)
                 next
                 (try next)))))
    (try first-guess)))

(defparameter dx 0.00001)
(defun deriv (g)
  (lambda (x) (/ (- (funcall g (+ x dx)) (funcall g x)) dx)))

(defun newton-transform (g)
  (lambda (x) (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))

;; We begin by noting that newtons-method takes a function of one parameter x
;; and a guess and returns the fixed point.  Our cubic function, then, must 
;; return a function of one parameter x that that implements the cubic 
;; function x3 + ax2 + bx + c.
;; 
(defun cubic (a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))

;; Using this function, we can test our implementation by selecting values
;; for a, b, and c, and a guess, and evaluating our newtons-method function.
;; Then, using the returned value, we can check that the value is indeed a
;; zero of the function returned by cubic.
;; We begin by setting a b and c equal to 2, and our guess equal to 1.0.  Then
;; we evaluate our newtons-method function.
(= -1.543689 (newtons-method (cubic 2 2 2) 1.0))

;; Next we test the returned value to show that it is indeed a zero of the
;; cubic function
(= 0 (funcall (cubic 2 2 2) -1.543689))
