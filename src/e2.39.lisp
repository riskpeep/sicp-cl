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
;; For fold-right, we use the implementation of append from e2.33
(defun my-reverse (sequence)
  (fold-right (lambda (x y) (accumulate #'cons (list x) y)) nil sequence))

;; Fold-left can be implemented directly using cons
(defun my-reverse2 (sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; Testing
(equal '(3 2 1) (my-reverse (list 1 2 3)))
(equal '(3 2 1) (my-reverse2 (list 1 2 3)))
