;Exercise 2.27:
;
;  Modify your reverse procedure of Exercise 2.18 to produce a deep-reverse 
;  procedure that takes a list as argument and returns as its value the list
;  with its elements reversed and with all sublists deep-reversed as well.
;
;  For example,
;
;  (define x (list (list 1 2) (list 3 4)))
;  x
;  ((1 2) (3 4))
;  (reverse x)
;  ((3 4) (1 2))
;  (deep-reverse x)
;  ((4 3) (2 1))

;; From e2.18, we have
;; 
(defun my-reverse (list)
  (labels ((iter (list result)
             (if (eq list nil)
               nil
               (if (eq (cdr list) nil)
                 (cons (car list) result)
                 (iter (cdr list) (cons (car list) result))))))
    (iter list nil)) 
;;
;; We modify this procedure by checking to see if the car is a list
(defun my-reverse (list)
  (labels ((iter (list result)
             (if (eq list nil)
               nil
               (if (eq (cdr list) nil)
                 (cons (car list) result)
                 (iter (cdr list) (cons (car list) result))))))
    (iter list nil)) 

