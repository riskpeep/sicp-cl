;Exercise 2.36:
;
;  The procedure accumulate-n is similar to accumu-late except that it takes
;  as its third argument a sequence of sequences, which are all assumed to have
;  the same number of elements. It applies the designated accumulation
;  procedure to combine all the first elements of the sequences, all the second
;  elements of the sequences, and so on, and returns a sequence of the results. 
;  For instance, if s is a sequence containing four sequences,
;
;  ((1 2 3) (4 5 6) (7 8 9) (10 11 12)),
;
;  then the value of (accumulate-n + 0 s) should be the sequence (22 26 30).
;  Fill in the missing expressions in the following definition of accumulate-n:
;
;  (define (accumulate-n op init seqs)
;    (if (null? (car seqs))
;      nil
;      (cons (accumulate op init <??>)
;            (accumulate-n op init <??>))))

;; Before we begin, we bring in accumulate
(defun accumulate (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence)
                (accumulate op initial (cdr sequence)))))

;; Now we may proceed with the definition of accumulate-n.  For the two
;; missing expressions, the first should return a list of the cars of each
;; list in the seqs parameter.  The second should return a new seqs that has
;; the cdr of each list in seqs.
(defun accumulate-n (op init seqs)
  (if (null (car seqs))
    nil
    (cons (accumulate op init (map 'list #'car seqs))
          (accumulate-n op init (map 'list #'cdr seqs)))))

;; Test
(defparameter s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(equal '(22 26 30) (accumulate-n #'+ 0 s))
