;Exercise 1.44:
;
;  The idea of smoothing a function is an important concept in signal 
;  processing. If f is a function and dx is some small number, then the 
;  smoothed version of f is the function whose value at a point x is the 
;  average of f (x - dx), f (x), and f (x+dx). Write a procedure smooth that 
;  takes as input a procedure that computes f and returns a procedure that 
;  computes the smoothed f . It is sometimes valuable to repeatedly smooth a 
;  function (that is, smooth the smoothed function, and so on) to obtain the 
;  n-fold smoothed function. Show how to generate the n-fold smoothed function
;  of any given function using smooth and repeated from Exercise 1.43.

;; We begin by defining some terms we need
(defparameter dx 0.00001)

;; Next we define smooth
(defun smooth (f)
  (lambda (x) (/ (+ (funcall f (- x dx))
                    (funcall f x)
                    (funcall f (+ x dx)))
                 3)))

;; ;  ... It is sometimes valuable to repeatedly smooth a function (that is, 
;; ;  smooth the smoothed function, and so on) to obtain the n-fold smoothed 
;; ;  function. Show how to generate the n-fold smoothed function of any given 
;; ;  function using smooth and repeated from Exercise 1.43.
;; 
;; From e1.41, we have compose
(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

;; From e1.43, we have repeated
(defun repeated (f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

;; To generate the n-fold smooth of a function using repeated we call repeated
;; as shown here
(repeated (smooth #'1+) 5)

;; Not really sure how to to test this.  Need a noisy function that we can 
;; smooth
