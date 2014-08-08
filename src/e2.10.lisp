;Exercise 2.10:
;
;  Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s shoulder
;  and comments that it is not clear what it means to divide by an interval 
;  that spans zero. Modify Alyssa’s code to check for this condition and to
;  signal an error if it occurs.

;; From e2.08 we have definitions we'll need
(defun make-interval (a b) (cons a b))
(defun lower-bound (a) (car a))
(defun upper-bound (a) (cdr a))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


;; From the text we have division as follows:
;;
;; (define (div-interval x y)
;;   (mul-interval
;;     x
;;     (make-interval (/ 1.0 (upper-bound y))
;;                    (/ 1.0 (lower-bound y)))))
;;
;; To check for spanning zero, we need to see if the signs of the upper and
;; lower bounds of the divisor differ.  If they do, we return nil to signal
;; an error
;; 
;; Note the CL functions minusp and plusp check to see if their arguments are
;; less-than or greater-than zero respectively
;; 
(defun div-interval (x y)
  (cond ( ; Does the divisor span zero?
         (or (and (plusp  (lower-bound y))
                  (minusp (upper-bound y)))
             (and (minusp (lower-bound y))
                  (plusp  (upper-bound y))))
         nil)
        (T (mul-interval
             x
             (make-interval (/ 1.0 (upper-bound y))
                            (/ 1.0 (lower-bound y)))))))

;; To test, we confirm that normal divisions work still
(div-interval (make-interval 1 10) (make-interval 10 20))
;; (0.05 . 1.0)
(div-interval (make-interval -1 10) (make-interval 10 20))
;; (-0.1 . 1.0)

;; And then we confirm that divisions where the divisor spans zero return nil
(eql nil (div-interval (make-interval 1 10) (make-interval -10 20)))

