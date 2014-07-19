;Exercise 1.30:
;
;  The sum procedure above generates a linear recursion. The procedure can be 
;  rewritten so that the sum is performed iteratively. Show how to do this by
;  filling in the missing expressions in the following definition:
; 
;  (define (sum term a next b)
;    (define (iter a result)
;      (if <??>
;        <??>
;        (iter <??> <??>)))
;    (iter <??> <??>))

;; To create a linear recursion, the general pattern has three parts:
;;   - include an accumulator term (here we use result) as parameter to the 
;;      iterator method, 
;;   - include an if test in the iterator method with a case that terminates
;;      the calculation and returns the accumulator term, and
;;   - perform the actual work in the parameter definitions for the recursive
;;      call to the iterator function.
;;
;; Following the pattern here and re-writing in CL, we arrive at the following definition:
(defun sum (term a next b)
  (defun iter (a result)
    (if (> a b)
      result
      (iter (funcall next a) (+ result (funcall term a)))))
  (iter a 0))

;; Following 1.3.1 in the text, we have:
;; 
;; (define (cube x) (* x x x))
;; (define (inc n) (+ n 1))
;; (define (sum-cubes a b)
;; (sum cube a inc b))
;; 
;; 
;; Converting to CL yields
(defun cube (x)
  (* x x x))

(defun inc (n) (+ n 1))

(defun sum-cubes (a b)
  (sum #'cube a #'inc b)) 

(= 3025 (sum-cubes 1 10))
