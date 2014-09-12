;Exercise 2.39:
;
;  Complete the following definitions of reverse (Exercise 2.18) in terms of
;  fold-right and fold-left from Exercise 2.38:
;  
;  (define (reverse sequence)
;    (fold-right (lambda (x y) <??>) nil sequence))
;  (define (reverse sequence)
;    (fold-left (lambda (x y) <??>) nil sequence))

;; First we bring in fold-left and fold-right from e2.38
(defun fold-left (op initial sequence)
  (labels ((iter (result rest)
                (if (null rest)
                  result
                  (iter (funcall op result (car rest))
                        (cdr rest)))))
    (iter initial sequence)))

(defun accumulate (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence)
                (accumulate op initial (cdr sequence)))))

(defun fold-right (op initial sequence)
  (accumulate op initial sequence))

;; Now may begin
(defun my-reverse (sequence)
  (fold-right (lambda (x y) (cons y (list  x))) nil sequence))
  (accumulate #'cons seq2 seq1) 

(defun my-reverse2 (sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; Testing
(my-reverse (list 1 2 3))
(my-reverse2 (list 1 2 3))

