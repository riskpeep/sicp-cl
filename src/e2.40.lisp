;Exercise 2.40:
;
;  Define a procedure unique-pairs that, given an integer n, generates the 
;  sequence of pairs (i, j) with 1 ≤ j < i ≤ n. Use unique-pairs to simplify 
;  the definition of prime-sum-pairs given above.

;; From the text we have 
;;
;; (accumulate
;;   append nil (map (lambda (i)
;;                     (map (lambda (j) (list i j))
;;                          (enumerate-interval 1 (- i 1))))
;;                   (enumerate-interval 1 n))) 
;;
;; The above procedure produces the required sequence of unique pairs, however
;; it is not a complete function definition.  We can complete the definition
;; by providing a name and definining the parameter n.
;; 
;; Before we begin, lets bring in some utility methods we'll need
(defun enumerate-interval (low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(defun accumulate (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence)
                (accumulate op initial (cdr sequence)))))

;; Now we can wrap the given procedure with a method definition as required
(defun unique-pairs (n)
  (accumulate
    #'append nil (map 'list (lambda (i)
                              (map 'list (lambda (j) (list i j))
                                   (enumerate-interval 1 (- i 1))))
                      (enumerate-interval 1 n))))

;; Testing
(equal '((2 1)) (unique-pairs 2))
(equal '((2 1) (3 1) (3 2)) (unique-pairs 3))

;; To simplify prime-sum-pairs, we must replace the sequence generating 
;; portion of the prime-sum-pairs procedure.
;; 
;; Utility methods we'll need
(defun divides? (a b) (= (rem b a) 0))
(defun square (x) (* x x))
(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (T (find-divisor n (+ test-divisor 1)))))
(defun smallest-divisor (n) (find-divisor n 2))
(defun prime? (n)
  (= n (smallest-divisor n))) 
(defun flatmap (proc seq)
  (accumulate #'append nil (map 'list proc seq)))
(defun prime-sum? (pair)
  (prime? (+ (car pair) (cadr pair))))
(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(defun filter (predicate sequence)
  (cond ((null sequence) nil)
        ((funcall predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (T (filter predicate (cdr sequence)))))

;; Now we proceed with our definition.  In the text, we are given
;; 
;; (define (prime-sum-pairs n)
;;   (map make-pair-sum
;;        (filter prime-sum? (flatmap
;;                             (lambda (i)
;;                               (map (lambda (j) (list i j))
;;                                    (enumerate-interval 1 (- i 1))))
;;                             (enumerate-interval 1 n)))))
;;
;; We can re-write this procedure using unique-pairs by replacing the flatmap
;; call with a call to unique-pairs as shown below
(defun prime-sum-pairs (n)
  (map 'list #'make-pair-sum
       (filter #'prime-sum? (unique-pairs n))))

;; Testing
(equal '((2 1 3) (3 2 5) (4 1 5) (4 3 7)) (prime-sum-pairs 4))
