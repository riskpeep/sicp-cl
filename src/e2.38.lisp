;Exercise 2.38:
;
;  The accumulate procedure is also known as fold-right, because it combines
;  the first element of the sequence with the result of combining all the
;  elements to the right. There is also a fold-left, which is similar to 
;  foldright, except that it combines elements working in the opposite
;  direction:
;
;  (define (fold-left op initial sequence)
;    (define (iter result rest)
;      (if (null? rest)
;        result
;        (iter (op result (car rest))
;              (cdr rest))))
;    (iter initial sequence))
;
;  What are the values of
;
;  (fold-right / 1 (list 1 2 3))
;  (fold-left / 1 (list 1 2 3))
;  (fold-right list nil (list 1 2 3))
;  (fold-left list nil (list 1 2 3))
;
;  Give a property that op should satisfy to guarantee that
;  fold-right and fold-left will produce the same values
;  for any sequence.

;; We can solve this problem empirically.  First, we convert the given 
;; fold-left to CL
(defun fold-left (op initial sequence)
  (labels ((iter (result rest)
                (if (null rest)
                  result
                  (iter (funcall op result (car rest))
                        (cdr rest)))))
    (iter initial sequence)))

;; For fold-right, we can use accumulate
(defun accumulate (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence)
                (accumulate op initial (cdr sequence)))))

(defun fold-right (op initial sequence)
  (accumulate op initial sequence))

;; Now we can evaluate the given forms
(= 3/2 (fold-right #'/ 1 (list 1 2 3)))
(= 1/6 (fold-left #'/ 1 (list 1 2 3)))
(equal '(1 (2 (3 nil))) (fold-right #'list nil (list 1 2 3)))
(equal '(((nil 1) 2) 3) (fold-left #'list nil (list 1 2 3)))

;; ;  Give a property that op should satisfy to guarantee that
;; ;  fold-right and fold-left will produce the same values
;; ;  for any sequence.
;;
;; For fold-left and fold-right to have have the same values for any sequence
;; the given operation must have the mathematical associative property   Both
;; addition and multiplication are associative so we can use them to 
;; demonstrate the truism

;; Addition
(= 15 (fold-right #'+ 0 (list 1 2 3 4 5)))
(= 15 (fold-left #'+ 0 (list 1 2 3 4 5)))

;; Multiplication
(= 120 (fold-right #'* 1 (list 1 2 3 4 5)))
(= 120 (fold-left #'* 1 (list 1 2 3 4 5)))

