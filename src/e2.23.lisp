;Exercise 2.23:
;
;  The procedure for-each is similar to map. It takes as arguments a procedure
;  and a list of elements. However, rather than forming a list of the results,
;  for-each just applies the procedure to each of the elements in turn, from
;  left to right. The values returned by applying the procedure to the 
;  elements are not used at all — for-each is used with procedures that 
;  perform an action, such as printing. For example,
;
;  (for-each (lambda (x)
;              (newline)
;              (display x))
;            (list 57 321 88))
;  57
;  321
;  88
;
;  The value returned by the call to for-each (not illustrated above) can be
;  something arbitrary, such as true. Give an implementation of for-each.

;; Our implementation of for each will use a front to back recursive iterator
;; to apply the method to the elements of the list
;;
(defun for-each (procedure items)
  (if (null items)
    T
    (progn 
      (funcall procedure (car items))
      (for-each procedure (cdr items)))))

(for-each (lambda (x)
            (format t "~%~A" x))
          (list 57 321 88))

;; The above implementation of for-each is unsatisfying in our opinion due to 
;; the use of progn.  While progn is a valid CL construct, we believe a more
;; elegant implementation must be possible.
;; 
;; A review of other approaches yeilds the following.
;; 
;; Eli Bendersky 
;; (http://eli.thegreenplace.net/2007/08/10/sicp-section-221/)
;; provides two implementations, one using the CL method mapc, and another 
;; using dolist.
;; 
;; ;; (mapc #'print '(1 2 3 4))
;; ;;
;; ;; (defun for-each (func items)
;; ;;  (dolist (item items)
;; ;;    (funcall func item)))
;;
;; Another approach is examplified by Bill the Lizard 
;; (http://www.billthelizard.com/2011/01/sicp-221-223-mapping-over-lists.html)
;; 
;; ;; (define (for-each proc items)
;; ;;    (cond ((null? items) #t)
;; ;;          (else (proc (car items))
;; ;;                (for-each proc (cdr items)))))
;; 
;; This one uses cond and feels more lispy because of the implicit progn in the
;; cond statement.
