;Exercise 2.35:
;
;  Redefine count-leaves from Section 2.2.2 as an accumulation:
;
;  (define (count-leaves t)
;    (accumulate <??> <??> (map <??> <??>)))

;; Before we begin, we bring in the definition for accumulate
(defun accumulate (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence)
                (accumulate op initial (cdr sequence)))))

;; From the definition of accumulate, we see that accumulate requires three
;; parameters, op, initial, and sequence.  Since we're counting leaves, we
;; would expect op to be an increment type operator, and initial to be zero
;; to start.  With that understanding, we would need a map function that
;; returns a list of the leaves that can be counted when given a tree.  Based
;; on that approach, we proceed as follows:
(defun count-leaves (l)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (map 'list
                   (let ((leaves (lambda (x)
                                   (cond ((null x) nil)
                                         ((atom x) x)
                                         (T (concatenate (funcall leaves (car x))
                                                         (funcall leaves (cdr x))))))))
                     (funcall leaves x))
                   l)))


