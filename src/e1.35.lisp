;Exercise 1.35:
;
;  Show that the golden ratio ϕ (Section 1.2.2) is a fixed point of the 
;  transformation
;
;           1
;  x → 1 + ---, 
;           x
;
;  and use this fact to compute ϕ by means of the fixed-point procedure.

;; From the text (sec 1.2.2), we have
;; 
;;      1 + sqrt(5)
;; ϕ = ------------- ~= 1.6180, and
;;          2
;;
;; ϕ² = ϕ + 1
;;
;; From the text (sec 1.3.3), we have that a number x is a fixed point of
;; function f if x satisfies the equation f(x) = x.
;;
;; We begin by writing the transformation as an equation
;;
;;                      f(x) = 1 + (1 / x)
;;                      f(ϕ) = 1 + (1 / ϕ)
;;                         ϕ = 1 + (1 / ϕ)
;;           (1 + sqrt(5))/2 = 1 + (1 / ((1 + sqrt(5))/2)
;;           (1 + sqrt(5))/2 = (((1 + sqrt(5))/2) + 1) / ((1 + sqrt(5))/2)
;;        ((1 + sqrt(5))/2)² = ((1 + sqrt(5))/2) + 1
;;        ((1 + sqrt(5))/2)² = (1 + sqrt(5))/2 + 1
;;   (1 + 2 * sqrt(5) + 5)/4 = (1 + sqrt(5))/2 + 1
;;       (6 + 2 * sqrt(5))/4 = (1 + sqrt(5) + 2)/2
;;           (3 + sqrt(5))/2 = (3 + sqrt(5))/2
;;                         1 = 1
;;
;; The above proof shows that ϕ = 1 + (1 / ϕ).
;; 
;; Next we compute ϕ using the fixed-point procedure.
;; 
;; From the text (sec 1.3.3) we have a definition for fixed-point
;;
;; (define tolerance 0.00001)
;; (define (fixed-point f first-guess)
;;   (define (close-enough? v1 v2)
;;     (< (abs (- v1 v2))
;;        tolerance))
;;   (define (try guess)
;;     (let ((next (f guess)))
;;       (if (close-enough? guess next)
;;           next
;;           (try next))))
;;   (try first-guess))
;;
;; Re-writing in CL yields
;; Here we introduce defparameter which dynamically binds the given name to
;; the indicated value
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

;; Finally, we test to show that it works.  We'll use lambda notation to 
;; define our function, and use 1.0 as our initial guess
(= 1.6180328  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
                           1.0))
