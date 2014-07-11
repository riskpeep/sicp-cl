;Exercise 1.13:
;
;  Prove that Fib(n) is the closest integer to phi^n / (sqrt 5)
;  where phi = (1 + (sqrt 5))/2. Hint: Let psi = (1 - (sqrt 5)/2.
;  Use induction and the definition of the Fibonacci numbers
;  (see Section 1.2.2) to prove that Fib(n) = (phi^n - psi^n)/(sqrt 5).
;

;; * - This proof appears largely unrelated to the primary subject of SICP
;;     however it is a given problem so I have completed it here.  That said
;;     while I gave this one the 'old college try,' I must admit that I relied
;;     in part to others who helped me bring this to a complete proof.  My
;;     references included these sites:
;;
;;     http://jots-jottings.blogspot.com/2011/08/sicp-113-fibonacci-numbers.html
;;     http://danboykis.com/2009/05/exercise-113-of-sicp/, and
;;     http://dwaynecrooks.wordpress.com/2010/11/07/sicp-a-proof-for-exercise-1-13/.
;;

;; Fibonacci numbers can be defined by the rule
;;
;;
;;          | 0                         if n = 0,
;;          |
;; Fib(n) = | 1                         if n = 1,
;;          |
;;          | Fib(n - 1) + Fib(n - 2)   Otherwise.
;;

;; We also have from the book that
;;
;; phi^2 = phi + 1
;;

;; To prove by induction, we'll first demonstrate the base cases of 0, and 1.
;;
;; To begin, let us assume the given
;;
;;     psi = (1 - (sqrt 5))/ 2
;;
;;     Fib(n) = (phi^n - psi^n)/(sqrt 5)
;;
;; Then, for 0:
;;
;; Fib(0) = (phi^0 - psi^0) / (sqrt 5)
;; Fib(0) = ( 1 - 1 ) / (sqrt 5)
;; Fib(0) = 0 / (sqrt 5)
;; Fib(0) = 0
;;
;; Similarly for 1:
;;
;; Fib(1) = (phi^1 - psi^1) / (sqrt 5)
;; Fib(1) = (phi - psi) / (sqrt 5)
;; Fib(1) = (((1 + (sqrt 5)) / 2) - ((1 - (sqrt 5)) / 2)) / (sqrt 5)
;; Fib(1) = ((1/2 + (sqrt 5)/2) - (1/2 - (sqrt 5)/2)) / (sqrt 5)
;; Fib(1) = (1/2 + (sqrt 5)/2 - 1/2 + (sqrt 5)/2) / (sqrt 5)
;; Fib(1) = (1/2 - 1/2 + (sqrt 5)/2 + (sqrt 5)/2) / (sqrt 5)
;; Fib(1) = ((sqrt 5)/2 + (sqrt 5)/2) / (sqrt 5)
;; Fib(1) = (sqrt 5) / (sqrt 5)
;; Fib(1) = 1
;;
;; Before demonstrating that the relation holds for all n > 1, we must
;; examine psi.
;;
;; The book gives phi^2 = phi + 1
;;
;; We can also show that this same relation holds for psi
;;
;; psi = (1 - (sqrt 5)) / 2
;; psi^2 = ((1 - (sqrt 5)) / 2)^2
;; psi^2 = ((1 - (sqrt 5))^2 / 4
;; psi^2 = (1 - 2(sqrt 5) + 5) / 4
;; psi^2 = (6 - 2(sqrt 5)) / 4
;; psi^2 = (3 - (sqrt 5)) / 2
;; psi^2 = (1 + 2 - (sqrt 5)) / 2
;; psi^2 = (2 + 1 - (sqrt 5)) / 2
;; psi^2 = 2/2 + (1 - (sqrt 5)) / 2
;; psi^2 = 1 + (1 - (sqrt 5)) / 2
;; psi^2 = (1 - (sqrt 5)) / 2 + 1
;; psi^2 = psi + 1
;;
;; Now we can proceed to demonstrate that this relation holds for all n > 1
;;
;; Fib(n) = Fib(n-1) + Fib(n-2)
;; Fib(n) = (phi^(n-1) - psi^(n-1))/(sqrt 5) + (phi^(n-2) - psi^(n-2))/(sqrt 5)
;; Fib(n) = (phi^(n-1) - psi^(n-1) + phi^(n-2) - psi^(n-2))/(sqrt 5)
;; Fib(n) = (phi^(n-1) + phi^(n-2) - psi^(n-1) - psi^(n-2))/(sqrt 5)
;; Fib(n) = ((phi + 1)phi^(n-2) - (psi - 1)psi^(n-2))/(sqrt 5)
;; Fib(n) = ((phi^2)phi^(n-2) - (psi^2)psi^(n-2))/(sqrt 5)
;; Fib(n) = (phi^n - psi^n)/(sqrt 5)
;;
;; This demonstrates that Fib(n) = (phi^n - psi^n)/(sqrt 5), which is given.
;;
;; But how does this prove that Fib(n) = int(phi^n/(sqrt 5))
;;
;; For Fib(n) = int(phi^n/(sqrt 5)), we must show that
;;
;; | Fib(n) - phi^n/(sqrt 5) | <= 1/2 for all n
;;
;; Let us examine this relation
;;
;; | Fib(n) - phi^n/(sqrt 5) | <= 1/2
;; | (phi^n - psi^n)/(sqrt 5) - phi^n/(sqrt 5) | <= 1/2
;; | (phi^n - psi^n - phi^n)/(sqrt 5) | <= 1/2
;; | (-psi^n)/(sqrt 5) | <= 1/2
;; | psi^n/(sqrt 5) | <= 1/2
;; | psi^n | <= (sqrt 5)/2
;;
;; Thus if we can show that psi^n is <= (sqrt 5)/2 for all n, we've proven that
;; Fib(n) = int(phi^n/(sqrt 5))
;;
;; psi = (1 - (sqrt 5))/ 2
;; psi = ~(1 - 2.3606)/2
(/ (- 1 2.3606) 2)

;; psi = ~-0.6083
;;
;; For | x | < 1 we know that 0 < x^n < x.
;;
;; thus psi^n <= 1 for all n
;;
;; (sqrt 5) / 2 ~= 1.1803
( / 2.3606 2)

;; So psi^n <= (sqrt 5)/2 for all n.
