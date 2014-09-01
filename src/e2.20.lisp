;Exercise 2.20:
;
;  The procedures +, *, and list take arbitrary numbers of arguments. One way
;  to define such procedures is to use define with dotted-tail notation. In a
;  procedure definition, a parameter list that has a dot before the last 
;  parameter name indicates that, when the procedure is called, the initial 
;  parameters (if any) will have as values the initial arguments, as usual, but
;  the final parameter’s value will be a list of any remaining arguments. For
;  instance, given the definition
;
;  (define (f x y . z) <body>)
;
;  the procedure f can be called with two or more arguments.  If we evaluate
;
;  (f 1 2 3 4 5 6)
;
;  then in the body of f, x will be 1, y will be 2, and z will be the list
;  (3 4 5 6). Given the definition
;
;  (define (g . w) <body>)
;
;  the procedure g can be called with zero or more arguments.  If we evaluate
;
;  (g 1 2 3 4 5 6)
;
;  then in the body of g, w will be the list (1 2 3 4 5 6).  (To define f and
;  g using lambda we would write
;
;    (define f (lambda (x y . z) <body>))
;    (define g (lambda w <body>))
;
;  Use this notation to write a procedure same-parity that takes one or more
;  integers and returns a list of all the arguments that have the same even-odd
;  parity as the first argument. For example,
;
;  (same-parity 1 2 3 4 5 6 7)
;  (1 3 5 7)
;
;  (same-parity 2 3 4 5 6 7)
;  (2 4 6)

;; To begin we note that in CL, the variable parameter notation differs 
;; somewhat from scheme.  In CL, instead of using dot (.) to indicate 
;; variable parameters, the notation &rest is used.  Otherwise the
;; operation is the same - passed values are assigned to named
;; parameters and any remaining passed values are assigned to the list
;; named by the &rest notation.
;; 
;; To define same-parity, we must identify the parity of the initial
;; value, and then iterate the list of optional values to find values
;; that match the parity
;;
(defun same-parity-alt (initial &rest values)
  (labels ((iter (parityp values result)
             (if (funcall parityp (car values))
               (cons result (car values)))
             (if (eql nil (cdr values))
               result
               (iter parityp 
                     (cdr values) 
                     (if (funcall parityp (car values))
                       (cons (car values) result)
                       result)))))
    (if (evenp initial)
      (iter #'evenp values (list initial))
      (iter #'oddp values (list initial)))))

(defun same-parity (initial &rest values)
  (labels ((iter (parityp values)
             (if (eql nil (cdr values))
               (if (funcall parityp (car values))
                 (list  (car values))
                 ())
               (if (funcall parityp (car values))
                 (cons (car values) (iter parityp (cdr values)))
                 (iter parityp (cdr values))))))
    (if (evenp initial)
      (cons initial (iter #'evenp values))
      (cons initial (iter #'oddp values)))))

;; Test
(equal '(1 3 5 7) (same-parity 1 2 3 4 5 6 7))
;;
(equal '(2 4 6) (same-parity 2 3 4 5 6 7))

;; Below, we provide an alternate implementation that is tail call optimized.
;; It returns its results in reverse order.  The problem statement does not
;; specifically indicate a desired return order, however, and this approach
;; being tail-call optimized should work for infinately large lists
(defun same-parity-alt (initial &rest values)
  (labels ((iter (parityp values result)
             (if (eql nil (cdr values))
               (if (funcall parityp (car values))
                 (cons (car values) result)
                 result)
               (iter parityp 
                     (cdr values) 
                     (if (funcall parityp (car values))
                       (cons (car values) result)
                       result)))))
    (if (evenp initial)
      (iter #'evenp values (list initial))
      (iter #'oddp values (list initial)))))

;; Testing same-parity-alt
(equal '(7 5 3 1) (same-parity-alt 1 2 3 4 5 6 7))

(equal '(8 6 4 2) (same-parity-alt 2 3 4 5 6 7 8))

;; As a final note, CL provides a number of list manipulation functions 
;; including REMOVE-IF, that would make the implementation of this procedure
;; trivial.  Our solutions above do not use these methods as they deviate from
;; the spirit of the problem.
;;
(defun same-parity-alt2 (initial &rest values)
    (if (evenp initial)
      (cons initial (remove-if-not #'evenp values))
      (cons initial (remove-if-not #'oddp values))))

(equal '(1 3 5 7) (same-parity-alt2 1 2 3 4 5 6 7))
