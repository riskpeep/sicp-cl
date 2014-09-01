;Exercise 2.17:
;
;  Define a procedure last-pair that returns the list that contains only the 
;  last element of a given (nonempty) list:
;
;  (last-pair (list 23 72 149 34))
;  (34)

;; The last cons pair will have nil as its cdr, so we can just walk the list
;; looking for a cdr of NIL and return that cons
(defun last-pair (list)
  (if (eql (cdr list) nil)
    list
    (last-pair (cdr list))))

;; Tests
(equal '(34) (last-pair (list 23 72 149 34)))

(equal '(23) (last-pair (list 23)))

(equal nil (last-pair nil))
