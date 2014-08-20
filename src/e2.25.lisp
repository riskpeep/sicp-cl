;Exercise 2.25: 
;
;  Give combinations of cars and cdrs that will pick 7 from each of the 
;  following lists:
;
;  (1 3 (5 7) 9)
;  ((7))
;  (1 (2 (3 (4 (5 (6 7))))))

;; First we create definitions of the lists
(defparameter a (list 1 3 (list 5 7) 9))
(defparameter b (list (list 7)))
(defparameter c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

;; Evaluating A, B, and C demonstrates that we have the correct lists
A 
;;  (1 3 (5 7) 9)
B
;;  ((7))
C
;;  (1 (2 (3 (4 (5 (6 7))))))

;; Now we may begin extracting the 7s.  We begin with the first list
(car (cdr (car (cdr (cdr a)))))
;; 7

;; An the second
(car (car b))

;; Finally the third
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))

;; Note that since cdr returns a list, a common pattern is cdr followed
;; by car.  CL provides a maens to simplify these in the cadr method.  Thus
;; it is possible to simplify the first and third extractions above using cadr
;; as follows:
;; 
;; First
(cadr (cadr (cdr a)))

;; Third
(cadr (cadr (cadr (cadr (cadr (cadr c))))))

