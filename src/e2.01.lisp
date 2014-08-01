;Exercise 2.1:
;
;  Define a better version of make-rat that handles both positive and negative 
;  arguments. Make-rat should normalize the sign so that if the rational number
;  is positive, both the numerator and denominator are positive, and if the
;  rational number is negative, only the numerator is negative.

;; From the text (2.1.1), we have the original make rat:
;;
;; (define (make-rat n d) (cons n d))
;;
;; Our make-rat must check the signs of the numerator and denominator and set
;; the signs of the created number correctly.  If neither number is negative or
;; both numbers are negative, then the fraction is positive, otherwise, it is
;; negative.
;;
(defun make-rat (n d)
  (cond ((and (plusp n) (plusp d)) (cons n d))
        ((and (minusp n) (minusp d)) (cons (- n) (- d)))
        ((minusp n) (cons n d))
        (T (cons (- n) (- d)))))

;; For testing, we also bring in print-rat from the text (2.1.1)
;; 
;; (define (numer x) (car x))
;; (define (denom x) (cdr x))
;; (define (print-rat x)
;;   (newline)
;;   (display (numer x))
;;   (display "/")
;;   (display (denom x)))
;;
;; In CL we have the following implementation
(defun numer (x) (car x))
(defun denom (x) (cdr x))
(defun print-rat (x)
  (format t "~%~A/~A" (numer x) (denom x)))

;; Finally, we can test make-rat
(print-rat (make-rat 1 2))   ;; 1/2
(print-rat (make-rat -1 -2)) ;; 1/2
(print-rat (make-rat -1 2))  ;; -1/2
(print-rat (make-rat 1 -2))  ;; -1/2

