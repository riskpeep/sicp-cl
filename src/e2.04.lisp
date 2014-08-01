;Exercise 2.4:
;
;  Here is an alternative procedural representation of pairs. For this 
;  representation, verify that (car (cons x y)) yields x for any 
;  objects x and y.
;
;  (define (cons x y)
;    (lambda (m) (m x y)))
;  (define (car z)
;    (z (lambda (p q) p)))
;
;  What is the corresponding definition of cdr? (Hint: To verify that this 
;  works, make use of the substitution model of Section 1.1.5.)

;; To verify that (car (cons x y)) yields x for any objects x and y, we use
;; the substitution model
;; 
;; (car (cons x y))                        ;; Start
;; (car (lambda(m) (m x y)))               ;; Substitute defn of cons
;; ((lambda (m) (m x y)) (lambda (p q) p)) ;; Substitute defn of car
;; ((lambda(p q) p) x y))                  ;; Apply lambda(m) function
;; (x)                                     ;; Apply lambda(p q) function

