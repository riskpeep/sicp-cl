;Exercise 2.18:
;
;  Define a procedure reverse that takes a list as argument and returns a list
;  of the same elements in reverse order:
;
;  (reverse (list 1 4 9 16 25))
;  (25 16 9 4 1)

;; Noting that cons adds the cons'd item to the front of the resulting list
;; we can proceed as follows.
(defun my-reverse (list)
  (labels ((iter (list result)
             (if (eq list nil)
               nil
               (if (eq (cdr list) nil)
                 (cons (car list) result)
                 (iter (cdr list) (cons (car list) result))))))
    (iter list nil)))

;; Testing
(my-reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)
(my-reverse (list 23))
;; (23)
(my-reverse nil)
;; NIL
