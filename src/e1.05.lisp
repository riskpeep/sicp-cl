;Exercise 1.5:
;
;  Ben Bitdiddle has invented a test to determine whether the interpreter he
;  is faced with is using applicative order evaluation or normal-order
;  evaluation. He defines the following two procedures:
;
;  (define (p) (p))
;
;  (define (test x y)
;    (if (= x 0) 0 y))
;
;  Then he evaluates the expression
;
;  (test 0 (p))
;
;  What behavior will Ben observe with an interpreter that uses applicative-
;  order evaluation? What behavior will he observe with an interpreter that
;  uses normal-order evaluation?
;
;  Explain your answer. (Assume that the evaluation rule for the special form
;  if is the same whether the interpreter is using normal or applicative order:
;  The predicate expression is evaluated first, and the result determines
;  whether to evaluate the consequent or the alternative expression.)


;; On inspection it can be seen that the function p is infinately recursive,
;; thus any evaluation of p will enter an infinate recursion loop and never
;; return (Or at least only return when stack space is exhausted.)
;;
;; With that understanding, we would expect that any evaluation approach that
;; attempts to evaluate p will fail to return.
;;
;; In applicative order evaluation, operands are fully evaluated prior to
;; applying the operator to them.  Since applicative order evaluation would
;; attempt to evaluate p, we would expect that an applicative order evaluation
;; would never return.
;;
;; In contrast, normal order evaluation delays evaluation until the values are
;; acutally needed.  In that case, we would expect that the method would return
;; with the value 0.  In normal order evaluation, p would never be evaluated
;; because it would be sent to test unevaluated, only to be expanded when
;; needed

;;  Re-writing in Common Lisp yeilds
(defun p () (p))

(defun test (x y)
  (if (= x 0) 0 #'y))

(test 0 (p))
