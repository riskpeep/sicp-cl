;Exercise 1.37:
;
;  a. An infinite continued fraction is an expression of the form
;
;                  N₁
;    f = ----------------------
;                     N₂
;         D₁ + ----------------
;                        N₃
;               D₂ + ----------
;                     D₃ + ...
;
;  As an example, one can show that the infinite continued fraction expansion 
;  with the Ni and the Di all equal to 1 produces 1/ϕ, where ϕ is the golden
;  ratio (described in Section 1.2.2). One way to approximate an infinite 
;  continued fraction is to truncate the expansion after a given number of 
;  terms. Such a truncation — a so-called k-term finite continued fraction
;  — has the form
;
;              N₁
;    ----------------------
;                 N₂
;     D₁ + ----------------
;                    Nk
;           ...  ----------
;                    Dk
;
;  Suppose that n and d are procedures of one argument (the term index i) that
;  return the Ni and Di of the terms of the continued fraction. Define a 
;  procedure cont-frac such that evaluating
;
;  (cont-frac n d k)
;
;  computes the value of the k-term finite continued fraction.
;
;  Check your procedure by approximating 1/ϕ using
;
;  (cont-frac (lambda (i) 1.0)
;             (lambda (i) 1.0)
;             k)
;
;  for successive values of k. How large must you make k in order to get an 
;  approximation that is accurate to 4 decimal places?
;
;  b. If your cont-frac procedure generates a recursive process, write one 
;  that generates an iterative process. If it generates an iterative process,
;  write one that generates a recursive process.

;; We can create an implementation of cont-frac as follows
(defun cont-frac (n d k)
  (labels ((iter (n d k current)
             (if (= k current)
               (+ (funcall d (1- current)) 
                  (/ (funcall n current)
                     (funcall d current)))
               (+ (funcall d (1- current))
                  (/ (funcall n current)
                     (iter n d k (1+ current)))))))
    (/ (funcall n 1) (iter n d k 2))))

;; We test cont-frac using the call given in the problem.  We set k = 10.
;; Note the use of (declare (ignore i)) to suppress an SBCL warning about
;; the unused variable i.  We leave this declaration out for the remainder
;; of this exercise for clarity 
(cont-frac (lambda (i) (declare (ignore i)) 1.0)
           (lambda (i) (declare (ignore i)) 1.0)
           10)

(= 0.6179775 (cont-frac (lambda (i) (declare (ignore i)) 1.0)
                        (lambda (i) (declare (ignore i)) 1.0)
                        10))

;; The golden ratio is 1.618033987 (from wikipedia)
;; 1/ϕ = 1/1.618033987 = 0.618034
(= 0.6179775 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10))
(= 0.6180556 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11))

;; For k > 11, we obtain a precision of at least 4 decimal places.
;; 
;;   b. If your cont-frac procedure generates a recursive process, write one 
;;   that generates an iterative process. If it generates an iterative process,
;;   write one that generates a recursive process.
;;
(defun cont-frac-iter (n d k)
  (labels ((iter (n d k accumulator)
             (if (= k 1)
               (/ (funcall n k) accumulator)
               (iter n d (1- k) (+ (funcall d (1- k)) (/ (funcall n k) accumulator))))))
    (iter n d k (funcall d k))))

;; And to test
(= 0.6179775 (cont-frac-iter (lambda (i) 1.0)
                             (lambda (i) 1.0)
                             10))
(= 0.6180556 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11))

