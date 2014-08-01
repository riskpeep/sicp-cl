;Exercise 2.5:
;
;  Show that we can represent pairs of nonnegative integers using only numbers
;  and arithmetic operations if we represent the pair a and b as the integer 
;  that is the product 2^a * 3^b . Give the corresponding definitions of the
;  procedures cons, car, and cdr.

;; This representation holds due to 5 math facts
;;    - Multiplying all even factors produces an even number
;;    - Multiplying all odd factors produces an odd number
;;    - Multiplying even AND odd factors produces an even number
;;    - n^0 = 1
;;    - n^1 = n
;;
;; We form the cons by performing the multiplication and exponentiation in the
;; formula
;; 
;; For car, we divide by 2 successively until we have an odd remainder.  The
;; number of divisions is the car.
;; 
;; For cdr, we divide by 3 successively until we have an even remainder. The
;; number of divisions is the cdr.
;;
;; Now we can begin our definitions
;; Cons is the easiest
;; Note the CL expt function raises a number to the given power
(defun my-cons (a b)
  (* (expt 2 a) (expt 3 b)))

(defun my-car (a) 
  ;; TODO
  )

(defun my-cdr (a) 
  ;; TODO
  )

