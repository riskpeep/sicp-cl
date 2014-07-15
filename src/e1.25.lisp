;Exercise 1.25:
;
;  Alyssa P. Hacker complains that we went to a lot of extra work in writing
;  expmod. After all, she says, since we already know how to compute
;  exponentials, we could have simply written
;
;  (define (expmod base exp m)
;     (remainder (fast-expt base exp) m))
;
;  Is she correct? Would this procedure serve as well for our fast prime
;  tester? Explain.

;; In this problem, we must determine if Alyssa P. Hacker's implementation of
;; expmod is equivalent the recursive implementation of expmod given in sec.
;; 1.2.6
;;
;; We can evaluate this empirically by testing both methods to see if they
;; produce the same result.
;;
;; First, we define some methods we'll need
;; To simplify comparison, we rename the parameters to match exp-mod
(defun square (a)
   (* a a))

;; To simplify our comparison with the sec 1.2.6 expmod, we rename the
;; parameters in fast-expt to match expmod
(defun fast-expt (base exp)
   "A fast method for calculating the exponential of a given number."
   (cond ((= exp 0) 1)
         ((evenp exp)
            (square (fast-expt base (/ exp 2))))
         (T
            (* base (fast-expt base (- exp 1))))))

;; Next we create sec. 1.2.6's version of expmod
(defun expmod (base exp m)
   "Computes the exponential of a number modulo another number"
   (cond ((= exp 0) 1)
         ((evenp exp)
             (rem
                 (square (expmod base (/ exp 2) m))
                 m))
         (T
             (rem
                 (* base (expmod base (- exp 1) m))
                 m))))

;; And finally Alyssa P. Hacker's implementation
(defun expmod-aph (base exp m)
   "Alyssa P. Hackers version of computing the exponential of a number modulo another number"
   (rem (fast-expt base exp) m))

;; Now we evaluate several examples to demonstrate the equivalence empirically
(= 4 (expmod 5 2 7))
(= 4 (expmod-aph 5 2 7))
(= 9 (expmod 4 3 11))
(= 9 (expmod-aph 4 3 11))

;; From an straight comparison, we see the approaches produce equivalent
;; results.
;;
;; If we examine the two implementations more closely, we see that the sec
;; 1.2.6 implementation of expmod makes to remainder in the recursive 
;; iterations, whereas Alyssa P. Hacker's implementation does not and instead 
;; makes one call to remainder at the end of the exponentiation process.
;; 
;; Based on that observation, we would expect that Alyssa P. Hacker's 
;; implementation will perform marginally better in most cases.  However, that
;; is not the end of the story.  Alyssa P. Hacker's implementation also 
;; rapidly creates extremely large numbers on which it is doing squares.  This
;; means that most implementations will need to rely on the Lisp BigNum type,
;; which cannot take advantage of processor accelerated mathematical 
;; operations.  The result is that BigNum operations are much slower than 
;; operations on smaller values.  
;;
;; Thus Alyssa P. Hacker's implementation will acutally be much slower in 
;; most cases than the expmod implementation given in section 1.2.6.
;;
