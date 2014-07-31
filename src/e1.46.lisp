;Exercise 1.46:
;
;  Several of the numerical methods described in this chapter are instances of
;  an extremely general computational strategy known as iterative improvement. 
;  Iterative improvement says that, to compute something, we start with an 
;  initial guess for the answer, test if the guess is good enough, and 
;  otherwise improve the guess and continue the process using the improved 
;  guess as the new guess. Write a procedure iterative-improve that takes two 
;  procedures as arguments: a method for telling whether a guess is good 
;  enough and a method for improving a guess. Iterative improve should return 
;  as its value a procedure that takes a guess as argument and keeps improving 
;  the guess until it is good enough. Rewrite the sqrt procedure of 
;  Section 1.1.7 and the fixed-point procedure of Section 1.3.3 in terms of
;  iterative-improve.

;; We begin by writing iterative-improve
(defun iterative-improve (good-enoughp improve-guess)
  (labels ((iteration (guess x) (if (funcall good-enoughp guess x)
                                     guess
                                     (iteration (funcall improve-guess guess x) x))))
    (lambda (x) (iteration (funcall improve-guess 1.0 x) x))))

;; To implement sqrt, we need first to define the procedures for good-enough 
;; and improve-guesss.  From 1.1.7, we have
;; 
;; (define (average x y)
;;   (/ (+ x y) 2))
;;
;; (define (improve guess x)
;;   (average guess (/ x guess))) 
;;
;; (define (good-enough? guess x)
;;   (< (abs (- (square guess) x)) 0.001))
;;
;; In CL we get
(defun square (x)
  (* x x))

(defun average (x y)
  (/ (+ x y) 2))

(defun improve (guess x)
  (average guess (/ x guess))) 

(defun good-enoughp (guess x)
  (< (abs (- (square guess) x)) 0.001))

;; Now we can implment sqrt
(defun im-sqrt (x)
  (funcall (iterative-improve #'good-enoughp #'improve) x))

;; Finally we test our sqrt procedure
;; sqrt of 5.0 = 2.23068
(= 2.2360687 (im-sqrt 5.0))


