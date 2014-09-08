;Exercise 2.37: 
;
;  Suppose we represent vectors v = (vi) as sequences of numbers, and matrices
;  m = (mij) as sequences of vectors (the rows of the matrix). For example, 
;  the matrix
;
;  | 1 2 3 4 |
;  | 4 5 6 6 |
;  | 6 7 8 9 |
;
;  is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this
;  representation, we can use sequence operations to concisely express the 
;  basic matrix and vector operations. These operations (which are described
;  in any book on matrix algebra) are the following:
;
;      (dot-product v w) returns the sum ∑ v w ;
;                                         i i i
;  (matrix-*-vector m v) returns the vector t;
;                        where t  = ∑ m  v ;
;                               i    j ij j
;  (matrix-*-matrix m n) returns the matrix p;
;                        where p   = ∑ m  n  ;
;                               ij    k ik kj
;          (transpose m) returns the matrix n;
;                        where n   = m   :
;                               ij    ji
;
;  We can define the dot product as
;
;  (define (dot-product v w)
;    (accumulate + 0 (map * v w)))
;
;  Fill in the missing expressions in the following procedures for computing 
;  the other matrix operations. (The procedure accumulate-n is defined in
;  Exercise 2.36.)
;
;  (define (matrix-*-vector m v)
;    (map <??> m))
;  (define (transpose mat)
;    (accumulate-n <??> <??> mat))
;  (define (matrix-*-matrix m n)
;    (let ((cols (transpose n)))
;      (map <??> m)))

;; Before we begin, we bring in accumulate and accumulate-n
(defun accumulate (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence)
                (accumulate op initial (cdr sequence)))))

(defun accumulate-n (op init seqs)
  (if (null (car seqs))
    nil
    (cons (accumulate op init (map 'list #'car seqs))
          (accumulate-n op init (map 'list #'cdr seqs)))))


;; Converting the given dot-product to CL yields:
(defun dot-product (v w)
  (accumulate #'+ 0 (map 'list #'* v w)))

;; For the remaining procedures we proceed as follows.  Not much we can say
;; about the required approach other than to present the solutions based on
;; the the given formulas:
(defun matrix-*-vector (m v)
  (map 'list (lambda (x) (dot-product x v)) m))

(defun transpose (mat)
  (accumulate-n #'cons nil mat))

(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (map 'list (lambda (x) (matrix-*-vector cols x)) m)))

;; Testing
;;  | 1 2 3 4 |
;;  | 4 5 6 6 |
;;  | 6 7 8 9 |
;;  
(defparameter m1 (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(defparameter m2 (list (list 1 -1 2) (list 0 -3 1)))

;;  v1 = (2 3)
;;  v2 = (-4 2)
;;  v3 = (2 -3 7)
;;  v4 = (-4 2 -4)
(defparameter v1 (list 2 -3))
(defparameter v2 (list -4 2))
(defparameter v3 (list 2 -3 7))
(defparameter v4 (list -4 2 -4))
(defparameter v5 (list 2 1 0))

;; Dot-product
(= -14 (dot-product v1 v2))
(= -42 (dot-product v3 v4))

;; matrix-*-vector
(equal '(1 -3) (matrix-*-vector m2 v5))

;; transpose
(equal '((1 4 6) (2 5 7) (3 6 8) (4 6 9)) (transpose m1))

;; matrix-*-matrix
(equal '((1 -7 4) (4 -19 13) (6 -27 19)) (matrix-*-matrix m1 m2))
