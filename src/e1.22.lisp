;Exercise 1.22:
;
;  Most Lisp implementations include a primitive called runtime that returns
;  an integer that specifies the amount of time the system has been running
;  (measured, for example, in microseconds). The following timed-prime-test
;  procedure, when called with an integer n, prints n and checks to see if n is
;  prime. If n is prime, the procedure prints three asterisks followed by the
;  amount of time used in performing the test.
;
;  (define (timed-prime-test n)
;     (newline)
;     (display n)
;     (start-prime-test n (runtime)))
;  (define (start-prime-test n start-time)
;     (if (prime? n)
;         (report-prime (- (runtime) start-time))))
;  (define (report-prime elapsed-time)
;     (display " *** ")
;     (display elapsed-time))
;
;  Using this procedure, write a procedure search-for-primes that checks the
;  primality of consecutive odd integers in a specified range. Use your
;  procedure to find the three smallest primes larger than 1000; larger than
;  10,000; larger than 100,000; larger than 1,000,000. Note the time needed to
;  test each prime. Since the testing algorithm has order of growth of
;  theta( (sqrt n) ), you should expect that testing for primes around 10,000
;  should take about (sqrt 10) times as long as testing for primes around 1000.
;  Do your timing data bear this out? How well do the data for 100,000 and
;  1,000,000 support the theta( (sqrt n) ) prediction? Is your result
;  compatible with the notion that programs on your machine run in time
;  proportional to the number of steps required for the computation?

;; First, we must identify the Common Lisp equivalent to the scheme runtime
;; function.  In Common Lisp, there are two runtime functions: 
;; get-internal-real-time and get-internal-run-time.  get-internal-real-time
;; returns elapsed real-time in seconds.  This is not of sufficient resolution
;; for our work here.  get-internal-run-time returns an implementation defined
;; time that is measured in CPU time.  The units of get-internal-run-time are 
;; undefined and thus the only useful comparison is between internal run times.
;; Internal run times are, however, generally much more fine grained than 
;; seconds.  The global parameter internal-time-units-per-second may be used
;; to determine the translation between internal run time and real time.
;;
;; We can examine this translation as shown here
internal-time-units-per-second

;; Now, we may begin. First we bring in some methods defined in the text
(defun square (a)
   (* a a))

(defun dividesp (a b)
   (= (rem b a) 0))

(defun runtime ()
   (get-internal-run-time))

(defun find-divisor (n test-divisor)
   (cond ((> (square test-divisor) n) n)
         ((dividesp test-divisor n) test-divisor)
         (T (find-divisor n (+ test-divisor 1)))))

(defun smallest-divisor (n)
   (find-divisor n 2))

(defun primep (n)
   (= n (smallest-divisor n)))

;; Now we can implement the functions given in the problem
(defun report-prime (elapsed-time)
   (format t "*** ~A" elapsed-time))

(defun start-prime-test (n start-time)
   (if (primep n)
       (report-prime (- (runtime) start-time))))

(defun timed-prime-test (n)
   (format t "~%")
   (format t "~A " n)
   (start-prime-test n (runtime)))

;; Testing timed-prime-test
(timed-prime-test 7)
(timed-prime-test 1999)
(timed-prime-test 199)
(timed-prime-test 19999)

;; Implementing search-for-primes
(defun search-for-primes (startnum endnum)
  (cond ((>= startnum endnum) nil)
        ((evenp startnum) (search-for-primes (1+ startnum) endnum))
        (T (timed-prime-test startnum) 
           (search-for-primes (+ startnum 2) endnum))))

;; Now we can find primes
(search-for-primes 1000 1020)
(search-for-primes 10000 10040)
(search-for-primes 100000 100040)
(search-for-primes 1000000 1000040)

