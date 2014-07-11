;Exercise 1.19:
;
;  There is a clever algorithm for computing the Fibonacci numbers in a
;  logarithmic number of steps. Recall the transformation of the state
;  variables a and b in the fib-iter process of Section 1.2.2: a <- a+b and
;  b <- a. Call this transformation T, and observe that applying T over and
;  over again n times, starting with 1 and 0, produces the pair Fib(n + 1) and
;  Fib(n). In other words, the Fibonacci numbers are produced by applying T^n,
;  the nth power of the transformation T, starting with the pair (1, 0). Now
;  consider T to be the special case of p = 0 and q = 1 in a family of
;  transformations Tpq , where Tpq transforms the pair (a, b) according to
;  a <- bq + aq + ap and b <- bp + aq. Show that if we apply such a
;  transformation Tpq twice, the effect is the same as using a single
;  transformation Tp′q′ of the same form, and compute p′ and q′ in terms of p
;  and q. This gives us an explicit way to square these transformations, and
;  thus we can compute T^n using successive squaring, as in the fast-expt
;  procedure. Put this all together to complete the following procedure,
;  which runs in a logarithmic number of steps:
;
;  (define (fib n)
;     (fib-iter 1 0 0 1 n))
;  (define (fib-iter a b p q count)
;     (cond ((= count 0) b)
;           ((even? count)
;           (fib-iter a
;                     b
;                     <??> ; compute p′
;                     <??> ; compute q′
;                     (/ count 2)))
;           (else (fib-iter (+ (* b q) (* a q) (* a p))
;                           (+ (* b p) (* a q))
;                           p
;                           q
;                           (- count 1)))))

;; To begin, we examine the transform Tpq to prove that we can define p' and q'
;; such that Tpq(Tpq) = Tp'q'.
;;
;; Tpq(Tpq) = Tp'q'
;; Tpq(a <- bq + aq + ap, b <- bp + aq ) = Tp'q'
;; (a = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,      b = (bp + aq)p + (bq + aq + ap)q ) = Tp'q'
;; (a = bpq + aqq + bqq + aqq + apq + bqp + aqp + app,       b = bpp + aqp + bqq + aqq + apq  ) = Tp'q'
;; (a = 2bpq + 2aqq + bqq + 2apq + app,                      b = bpp + 2aqp + bqq + aqq       ) = Tp'q'
;; (a = 2bpq + 2aq^2 + bq^2 + 2apq + ap^2,                   b = bp^2 + 2aqp + bq^2 + aq^2    ) = Tp'q'
;; (a = 2aq^2 + 2apq + ap^2 + 2bpq + bq^2,                   b = 2aqp + aq^2 + bp^2 + bq^2    ) = Tp'q'
;; (a = a(2q^2 + 2pq + p^2) + b(2pq + q^2),                  b = a(2qp + q^2) + b(p^2 + q^2)  ) = Tp'q'
;; (a = a(2q^2 + 2pq + p^2) + b(2pq + q^2),                  b = a(2qp + q^2) + b(p^2 + q^2)  ) = (a = bq' + aq' + ap', b = bp' + aq')
;; bq' + aq' + ap' = a(2q^2 + 2pq + p^2) + b(2pq + q^2)      bp' + aq' = a(2qp + q^2) + b(p^2 + q^2)
;;
;;                                                           aq' + bp' = a(2qp + q^2) + b(p^2 + q^2)
;;
;; From inspection of the b side, we have
;;                                                            p' = (p^2 + q^2)
;;                                                            q' = (2qp + q^2)
;;
;; To check, we can substitute p' and q' in the a side
;;
;;                                          a = a(2q^2 + 2pq + p^2) + b(2pq + q^2)
;;                            bq' + aq' + ap' = a(2q^2 + 2pq + p^2) + b(2pq + q^2)
;; b(2qp + q^2) + a(2qp + q^2) + a(p^2 + q^2) = a(2q^2 + 2pq + p^2) + b(2pq + q^2)
;;   b(2qp + q^2) + 2aqp + aq^2 + ap^2 + aq^2 = a(2q^2 + 2pq + p^2) + b(2pq + q^2)
;;         2aqp + 2aq^2 + ap^2 + b(2qp + q^2) = a(2q^2 + 2pq + p^2) + b(2pq + q^2)
;;         a(2qp + 2q^2 + p^2) + b(2qp + q^2) = a(2q^2 + 2pq + p^2) + b(2pq + q^2)
;;         a(2q^2 + 2qp + p^2) + b(2qp + q^2) = a(2q^2 + 2pq + p^2) + b(2pq + q^2)
;;                                          1 = 1
;;
;; Now we can complete the given function:
(defun fib-iter (a b p q count)
   (cond ((= count 0) b)
         ((evenp count)
             (fib-iter a
                       b
                       (+ (* p p) (* q q)) ; compute p′
                       (+ (* 2 q p) (* q q)) ; compute q′
                       (/ count 2)))
         (T (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1)))))

(defun fib (n)
   (fib-iter 1 0 0 1 n))

;; And demonstrate that it works
(= 0 (fib 0))
(= 1 (fib 1))
(= 1 (fib 2))
(= 2 (fib 3))
(= 3 (fib 4))
(= 5 (fib 5))
(= 8 (fib 6))
