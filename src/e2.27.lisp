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
    (iter list nil)))
;;
;; We modify this procedure by attempting to iterate on the car as well as on
;; the cdr if the car is a pair.  Note the CL equivalent to pair? is consp.
(defun deep-reverse (list)
  (labels ((iter (list result)
             (if (eq list nil)
               nil
               (let ((car-val (cond
                                ((consp (car list))
                                 (iter (car list) nil))
                                (T
                                 (car list)))))
                 (if (eq (cdr list) nil)
                   (cons car-val result)
                   (iter (cdr list) (cons car-val result)))))))
    (iter list nil)))

;; Testing
(defparameter x (list (list 1 2) (list 3 4)))

(deep-reverse x)
;; ((4 3) (2 1))
