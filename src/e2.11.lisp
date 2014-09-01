;Exercise 2.11:
;
;  In passing, Ben also cryptically comments: "By testing the signs of the 
;  endpoints of the intervals, it is possible to break mul-interval into nine
;  cases, only one of which requires more than two multiplications." Rewrite
;  this procedure using Ben’s suggestion.
;

;; Before we begin, bring in definitions from e2.07
(defun make-interval (a b) (cons a b))
(defun lower-bound (a) (car a))
(defun upper-bound (a) (cdr a)) 

;; From the text, we are given
;; 
;; (define (mul-interval x y)
;;   (let ((p1 (* (lower-bound x) (lower-bound y)))
;;         (p2 (* (lower-bound x) (upper-bound y)))
;;         (p3 (* (upper-bound x) (lower-bound y)))
;;         (p4 (* (upper-bound x) (upper-bound y))))
;;     (make-interval (min p1 p2 p3 p4)
;;                    (max p1 p2 p3 p4))))
;;
;; The nine cases to be considered are as follows
;; 
;; 1. All bounds > 0,
;; 2. lower-bound x < 0, all other bounds > 0,
;; 3. lower-bound y < 0, all other bounds > 0,
;; 4. both lower-bounds < 0, all other bounds > 0,
;; 5. both x bounds < 0, both y bounds > 0,
;; 6. both y bounds < 0, both x bounds > 0,
;; 7. both x bounds, and lower-y bound < 0,
;; 8. both y bounds, and lower-x bound < 0, and
;; 9. All bounds < 0.
;;
;; For each of the cases, we next determine the correct combination
;; 1. All bounds > 0,
;;      (make-interval (* (lower-bound x) (lower-bound y))
;;                     (* (upper-bound x) (upper-bound y))
;; 2. lower-bound x < 0, all other bounds > 0,
;;      (make-interval (* (lower-bound x) (lower-bound y))
;;                     (* (upper-bound x) (upper-bound y))
;; 3. lower-bound y < 0, all other bounds > 0,
;;      (make-interval (* (lower-bound x) (lower-bound y))
;;                     (* (upper-bound x) (upper-bound y))
;; 4. both lower-bounds < 0, all other bounds > 0,
;; 5. both x bounds < 0, both y bounds > 0,
;; 6. both y bounds < 0, both x bounds > 0,
;; 7. both x bounds, and lower-y bound < 0,
;;      (make-interval (* (upper-bound x) (upper-bound y))
;;                     (* (lower-bound x) (lower-bound y))
;; 8. both y bounds, and lower-x bound < 0, and
;;      (make-interval (* (upper-bound x) (upper-bound y))
;;                     (* (lower-bound x) (lower-bound y))
;; 9. All bounds < 0.
;;      (make-interval (* (upper-bound x) (upper-bound y))
;;                     (* (lower-bound x) (lower-bound y))
;;
;; We can re-write the given method in CL with the conditional tests as follows
(defun mul-interval (x y)
  (cond ((and (plusp (lower-bound x)) 
              (plusp (upper-bound x))
              (plusp (lower-bound y))
              (plusp (upper-bound y)))
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (minusp (lower-bound x)) 
              (plusp (upper-bound x))
              (plusp (lower-bound y))
              (plusp (upper-bound y)))
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y)))) 
        ((and (plusp (lower-bound x)) 
              (plusp (upper-bound x))
              (minusp (lower-bound y))
              (plusp (upper-bound y)))
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y)))) 
        ((and (minusp (lower-bound x)) 
              (plusp (upper-bound x))
              (minusp (lower-bound y))
              (plusp (upper-bound y)))
         (let ((p1 (* (lower-bound x) (lower-bound y)))
               (p2 (* (upper-bound x) (upper-bound y))))
           (make-interval (min p1 p2) (max p1 p2)))) 
        ((and (minusp (lower-bound x)) 
              (minusp (upper-bound x))
              (plusp (lower-bound y))
              (plusp (upper-bound y)))
         (let ((p1 (* (lower-bound x) (lower-bound y)))
               (p2 (* (upper-bound x) (upper-bound y)))
               (p3 (* (lower-bound x) (upper-bound y)))
               (p4 (* (upper-bound x) (lower-bound y))))
           (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
        ((and (plusp (lower-bound x)) 
              (plusp (upper-bound x))
              (minusp (lower-bound y))
              (minusp (upper-bound y)))
         (let ((p1 (* (lower-bound x) (lower-bound y)))
               (p2 (* (upper-bound x) (upper-bound y)))
               (p3 (* (lower-bound x) (upper-bound y)))
               (p4 (* (upper-bound x) (lower-bound y))))
           (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
        ((and (minusp (lower-bound x)) 
              (minusp (upper-bound x))
              (minusp (lower-bound y))
              (plusp (upper-bound y))) 
         (make-interval (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (minusp (lower-bound x)) 
              (plusp (upper-bound x))
              (minusp (lower-bound y))
              (minusp (upper-bound y)))
         (make-interval (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (minusp (lower-bound x)) 
              (minusp (upper-bound x))
              (minusp (lower-bound y))
              (minusp (upper-bound y)))
         (make-interval (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        (T ;; No other cases
          )))

;; Testing
;; 
;; Note that in CL, we can use the predicate equal to verify equality for
;; lists and other complex types.
;;
;; 1. All bounds > 0,
(equal '(5 . 50) (mul-interval (make-interval 1 5) (make-interval 5 10)))

;; 2. lower-bound x < 0, all other bounds > 0,
(equal '(-5 . 50) (mul-interval (make-interval -1 5) (make-interval 5 10)))

;; 3. lower-bound y < 0, all other bounds > 0,
(equal '(-5 . 50) (mul-interval (make-interval 1 5) (make-interval -5 10)))

;; 4. both lower-bounds < 0, all other bounds > 0,
(equal '(5 . 50) (mul-interval (make-interval -1 5) (make-interval -5 10)))

;; 5. both x bounds < 0, both y bounds > 0,
(equal '(-50 . -5) (mul-interval (make-interval -5 -1) (make-interval 5 10)))

;; 6. both y bounds < 0, both x bounds > 0,
(equal '(-50 . -5) (mul-interval (make-interval 1 5) (make-interval -10 -5)))

;; 7. both x bounds, and lower-y bound < 0,
(equal '(-10 . 25) (mul-interval (make-interval -5 -1) (make-interval -5 10)))

;; 8. both y bounds, and lower-x bound < 0, and
(equal '(-25 . 10) (mul-interval (make-interval -1 5) (make-interval -10 -5)))

;; 9. All bounds < 0.
(equal '(5 . 50) (mul-interval (make-interval -5 -1) (make-interval -10 -5)))
