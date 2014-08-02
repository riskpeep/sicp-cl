;Exercise 2.14:
;
;  After considerable work, Alyssa P. Hacker delivers her finished system.
;  Several years later, aer she has forgotten all about it, she gets a
;  frenzied call from an irate user, Lem E. Tweakit. It seems that Lem has
;  noticed that the formula for parallel resistors can be written in two 
;  algebraically equivalent ways:
;
;     R₁R₂
;  ---------
;   R₁ + R₂
;
;  and
;  
;        1
;  -------------
;   1/R₁ + 1/R₂
;
;  He has written the following two programs, each of which computes the 
;  parallel-resistors formula differently:
;
;  (define (par1 r1 r2)
;    (div-interval (mul-interval r1 r2)
;                  (add-interval r1 r2)))
;  (define (par2 r1 r2)
;    (let ((one (make-interval 1 1)))
;      (div-interval
;        one (add-interval (div-interval one r1)
;                          (div-interval one r2)))))
;
;  Lem complains that Alyssa’s program gives different answers
;  for the two ways of computing. This is a serious complaint.
;
;  Exercise 2.14:
;
;  Demonstrate that Lem is right. Investigate the behavior of the system on a 
;  variety of arithmetic expressions.  Make some intervals A and B, and use 
;  them in computing the expressions A/A and A/B. You will get the most 
;  insight by using intervals whose width is a small percentage of the center 
;  value. Examine the results of the computation in center-percent form 
;  (see Exercise 2.12).
