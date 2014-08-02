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
  (labels ((iter (a count)
             (if (= (rem a 2) 0)
               (iter (/ a 2) (1+ count))
               count)))
    (iter a 0)))

(defun my-cdr (a) 
  (labels ((iter (a count)
             (if (= (rem a 3) 0)
               (iter (/ a 3) (1+ count))
               count)))
    (iter a 0)))

;; Now to test we can create a cons and then prove that we can pull out the
;; car and cdr
(= 108 (my-cons 2 3))
(= 2 (my-car 108))
(= 3 (my-cdr 108))

(= 81 (my-cons 0 4))
(= 0 (my-car 81))
(= 4 (my-cdr 81))

(= 16 (my-cons 4 0))
(= 4 (my-car 16))
(= 0 (my-cdr 16))
