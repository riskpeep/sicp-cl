;Exercise 1.26:
;
;  Louis Reasoner is having great difficulty doing Exercise 1.24. His
;  fast-prime? test seems to run more slowly than his prime? test. Louis calls
;  his friend Eva Lu Ator over to help. When they examine Louis’s code, they
;  find that he has rewritten the expmod procedure to use an explicit
;  multiplication, rather than calling square:
;
;  (define (expmod base exp m)
;     (cond ((= exp 0) 1)
;           ((even? exp)
;                   (remainder (* (expmod base (/ exp 2) m)
;                                 (expmod base (/ exp 2) m))
;                              m))
;           (else
;                (remainder (* base
;                              (expmod base (- exp 1) m))
;                              m))))
;
;  “I don’t see what difference that could make,” says Louis. “I do.” says Eva.
;  “By writing the procedure like that, you have transformed the theta(log n)
;  process into a theta(n) process.” Explain.

;; The Lisp (and Clojure) evaluator performs applicative-order evaluation
;; (See sec 1.1.5).  In applicative-order evaluation, function parameters
;; are evaluated by the interpreter before applying the function.
;;
;; The critical section of the expmod implementation is the call to remainder
;; shown here as a stand-alone call:
;;
;; (remainder (* (expmod base (/ exp 2) m)
;;               (expmod base (/ exp 2) m))
;;            m)
;;
;;
;; In this implmentation, and with the knowledge that Lisp uses applicative-
;; order evaluation, we see that the expmod function must be evaluated twice
;; before applying the * function.
;;
;; In contrast, with square, we would see the following implmeentation:
;;
;; (remainder (square (expmod base (/ exp 2) m))
;;            m)
;;
;; In this implementation, there is only one expmod parameter and the result
;; value is used twice in the body of square as parameters to *.

