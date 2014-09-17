;Exercise 2.41:
;
;  Write a procedure to find all ordered triples of distinct positive integers
;  i, j, and k less than or equal to a given integer n that sum to a given 
;  integer s.

;; This procedure should follow the pattern of e2.40, so we'll use that as a
;; template.
;;
;; Before we begin, lets bring in some utility methods
(defun enumerate-interval (low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(defun accumulate (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence)
                (accumulate op initial (cdr sequence)))))

(defun flatmap (proc seq)
  (accumulate #'append nil (map 'list proc seq)))

(flatmap #'append (enumerate-interval 1 4))


;; For our sequence generator, we need all distinct triples, not just those
;; where 1 ≤ k ≤ j ≤ i ≤ n.  So we use a simplified enumerate-interval call.
(defun unique-triples (n)
  (accumulate
    #'append nil (map 'list (lambda (i)
                              (map 'list (lambda (j)
                                           (map 'list (lambda (k) (list i j k))
                                                (enumerate-interval 1 n)))
                                   (enumerate-interval 1 n)))
                      (enumerate-interval 1 n))))

;; Bring in filter
(defun filter (predicate sequence)
  (cond ((null sequence) nil)
        ((funcall predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (T (filter predicate (cdr sequence)))))

;; Now we can implement our procedure.  We embed the test for each element in 
;; the body so we can refer to the variable s in its definition
(defun triple-sum-triples (n s)
  (labels ((triple-sum-s-p (triple)
             (= s (+ (car triple) (cadr triple) (cadr (cdr triple))))))
    (map 'list (lambda (x) x)
         (filter #'triple-sum-s-p (unique-triples n)))))

(triple-sum-triples 4 4)


