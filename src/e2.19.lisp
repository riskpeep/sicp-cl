;Exercise 2.19:
;
;  Consider the change-counting program of Section 1.2.2. It would be nice to
;  be able to easily change the currency used by the program, so that we could
;  compute the number of ways to change a British pound, for example.  As the
;  program is written, the knowledge of the currency is distributed partly
;  into the procedure first-denomination and partly into the procedure 
;  count-change (which knows that there are five kinds of U.S. coins). It
;  would be nicer to be able to supply a list of coins to be used for making
;  change.
;
;  We want to rewrite the procedure cc so that its second argument is a list
;  of the values of the coins to use rather than an integer specifying which
;  coins to use. We could then have lists that defined each kind of currency:
;
;  (define us-coins (list 50 25 10 5 1))
;  (define uk-coins (list 100 50 20 10 5 2 1 0.5))
;
;  We could then call cc as follows:
;  
;  (cc 100 us-coins)
;  292
;  
;  To do this will require changing the program cc somewhat.  It will still
;  have the same form, but it will access its second argument differently, as
;  follows:
;
;  (define (cc amount coin-values)
;    (cond ((= amount 0) 1)
;          ((or (< amount 0) (no-more? coin-values)) 0)
;          (else
;            (+ (cc amount
;                   (except-first-denomination
;                     coin-values))
;               (cc (- amount
;                      (first-denomination
;                        coin-values))
;                   coin-values)))))
;
;  Define the procedures first-denomination, except-first-de-nomination, and
;  no-more? in terms of primitive operations on list structures. Does the order
;  of the list coinvalues affect the answer produced by cc? Why or why not?

;; To begin, we define first-denomination, except-first-denomination, and 
;; no-more
(defun first-denomination (coin-values)
  (car coin-values))

(defun except-first-denomination (coin-values)
  (cdr coin-values))

(defun no-more? (coin-values)
  (if (eq nil coin-values)
    T
    nil))

;; Next, we re-write cc in CL
(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (T
         (+ (cc amount
                (except-first-denomination
                  coin-values))
            (cc (- amount
                   (first-denomination
                     coin-values))
                coin-values)))))

;; Now we can do some testing
(defvar us-coins (list 50 25 10 5 1))
(defvar uk-coins (list 100 50 20 10 5 2 1 0.5))

(= 292 (cc 100 us-coins))
(= 292 (cc 104 us-coins))
(= 335 (cc 105 us-coins))
(= 104561 (cc 100 uk-coins))

;; And we can also look at if the order matters
(defvar us-coins-alt (list 1 5 10 25 50))
(= 292 (cc 100 us-coins-alt))
(= 292 (cc 104 us-coins-alt))
(= 335 (cc 105 us-coins-alt))

;; From these examples, we can see that the order of the denominations does 
;; not matter.  Understanding why is more interesting.
;; 
;; In both cases, the procedure identifies a change counting method by creating
;; a recursive search tree.  An example of the search tree can be seen in 
;; e1.11.  Now, the way that the given cc procedure works is to recursively
;; call itself with fewer coins and with a smaller amount to count change on.
;; It then adds the sum of the two recursions.  These two branches explore
;; different sides of the tree through the search space.  In the descending
;; currency order case, the tree is explored from the opposite direction as
;; the ascending currency order case.  For example, from e1.11 we see that one
;; side of the tree is the case of only pennies (or 0.5 in the case of uk 
;; coins).  This side of the tree is recursed first in the ascending currency
;; order case.  In the descending currency order case, this branch is recursed
;; last.

