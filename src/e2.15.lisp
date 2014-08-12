;Exercise 2.15:
;
;  Eva Lu Ator, another user, has also noticed the different intervals 
;  computed by different but algebraically equivalent expressions. She says
;  that a formula to compute with intervals using Alyssa’s system will 
;  produce tighter error bounds if it can be written in such a form that no
;  variable that represents an uncertain number is repeated. Thus, she says,
;  par2 is a “better” program for parallel resistances than par1. Is she 
;  right? Why?

;; For reference, we bring in the definitions of par1 and par2 from e2.14
(defun par1 (r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)) 
(defun par2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one (add-interval (div-interval one r1)
                        (div-interval one r2)))))

;; Now, to consider Eva Lu Ator's comment that the system will produce tighter
;; error bounds if no variable that represents an uncertain number is repeated.
;;
;; We've demonstrated in e2.13 that multiplying two intervals results in a new
;; interval with an uncertainty roughly equal to the sum of the uncertainties.
;; In e2.14, we demonstrated that the same relationship holds true for 
;; division operations.
;; 
;; With these considerations in mind, we note that similar relationships hold
;; for addition and subtraction.  For each interval operation, the uncertainty
;; in the interval increases.  The best procedure, then, will be those that
;; minimize the number of operations that increase the uncertainty.  Hence
;; Eva Lu Ator's comment about not repeating uncertain numbers. Under Eva Lu 
;; Ator's reasoning, fewer terms should result in fewer operations and thus 
;; will have a smaller resulting uncertainty.
;; 
;; Examining par2, at first glance it would appear that there are more 
;; operations (4) than in par1 (3), however, closer examination reveals that
;; three of the operations in par2 are performed with an interval that carries
;; no uncertainty (the div-interval operations with 'one') and thus that does
;; not increase the uncertainty in the resulting interval.
;; 
;; With this consideration, we can definatively state that Eva Lu Ator is 
;; correct, that par2 produces better results, and that the reason why is that
;; each interval operation produces a new interval with a greater expected
;; tolerance.
