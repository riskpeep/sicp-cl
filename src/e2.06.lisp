;Exercise 2.6:
;
;  In case representing pairs as procedures wasn’t mind-boggling enough, 
;  consider that, in a language that can manipulate procedures, we can get by 
;  without numbers (at least insofar as nonnegative integers are concerned) by
;  implementing 0 and the operation of adding 1 as
;
;  (define zero (lambda (f) (lambda (x) x)))
;  (define (add-1 n)
;    (lambda (f) (lambda (x) (f ((n f) x)))))
;
;  This representation is known as Church numerals, after its inventor, Alonzo 
;  Church, the logician who invented the λ-calculus.  Define one and two 
;  directly (not in terms of zero and add-1). (Hint: Use substitution to 
;  evaluate (add-1 zero)). Give a direct definition of the addition procedure +
;  (not in terms of repeated application of add-1).

;; In order to define 1, we use substitution to evaluate (add-1 zero)
;; 
;; (add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))                               ; Sub zero
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))) ; Sub add-1
;; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))                  ; Apply lambda (f)
;; (lambda (f) (lambda (x) (f x)))                                   ; Apply lambda (x)
;; 
;; (define one (lambda (f) (lambda (x) (f x))))
;; 
;; Similarly, we can define two as (add-1 one) and use subsitution
;; 
;; (add-1 one)
;; (add-1 (lambda (f) (lambda (x) (f x))))                               ; Sub one
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x)))) ; Sub add-1
;; (lambda (f) (lambda (x) (f ((lambda(x) (f x)) x))))                   ; Apply lambda (f)
;; (lambda (f) (lambda (x) (f (f x))))                                   ; Apply lambda (x)
;; 
;; (define two   (lambda (f) (lambda (x) (f (f x)))))
;; 
;; Greater numbers follow the pattern
;; (define three (lambda (f) (lambda (x) (f (f (f x))))))
;; (define four  (lambda (f) (lambda (x) (f (f (f (f x)))))))
;; (define five  (lambda (f) (lambda (x) (f (f (f (f (f x))))))))
;; (define six   (lambda (f) (lambda (x) (f (f (f (f (f (f x)))))))))
;; 
;; To implement add, we consider this pattern and the existing add-1 to create
;; the following implementation
;;
;; (define (add-ab a b)
;;   (lambda (f) (lambda (x) ((b f) ((a f) x)))))
;;
;; We show that this works by adding two to three
;; 
;; (add-ab two three)
;; (add-ab (lambda (f) (lambda (x) (f (f x))))       ; Sub a
;;         (lambda (f) (lambda (x) (f (f (f x))))))  ; Sub b
;; (lambda (f) (lambda (x) (                         ; Sub add-ab
;;    ((lambda (f) (lambda (x) (f (f (f x))))) f)
;;    (((lambda (f) (lambda (x) (f (f x)))) f) x))))
;; (lambda (f) (lambda (x) (
;;    (lambda (x) (f (f (f x))))                     ; Apply lambda (f)
;;    ((lambda (x) (f (f x))) x))))                  ; Apply lambda (f)
;; (lambda (f) (lambda (x) (
;;    (lambda (x) (f (f (f x))))
;;    (f (f x)))))                                   ; Apply lambda (x)
;; 
;; (lambda (f) (lambda (x) ((f (f (f (f (f x)))))))) ; Apply lambda (x)
;; (lambda (f) (lambda (x) ((f (f (f (f (f x)))))))) ; Final Result
;;
;; From inspection, we see that our result is the same as five, our expected
;; result:
;;    (define five  (lambda (f) (lambda (x) (f (f (f (f (f x))))))))
;; 
;; We can write zero, add-1, etc. in CL as follows
(defvar zero (lambda (f) (lambda (x) x)))
(defun add-1 (n)
  (lambda (f) (lambda (x) (funcall f (funcall (funcall n f) x)))))


(defvar one (lambda (f) (lambda (x) (funcall f x))))
(defvar two (lambda (f) (lambda (x) (funcall f (funcall f x)))))
(defvar three (lambda (f) (lambda (x) (funcall f (funcall f (funcall f x))))))

(defun add-ab (a b)
  (lambda (f) (lambda (x) (funcall (funcall b f) (funcall (funcall a f) x)))))

;; Testing
(add-ab two three)

