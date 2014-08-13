;Exercise 2.16:
;
;  Explain, in general, why equivalent algebraic expressions may lead to 
;  different answers. Can you devise an interval-arithmetic package that does 
;  not have this shortcoming, or is this task impossible?
;
;  (Warning: This problem is very difficult.)

;; * - This problem veers pretty strongly away from the primary SICP topic and
;;     into number theory, however, it is a given problem so I've taken a shot
;;     at it here.  That said, while I gave this one the 'old college try,' I 
;;     must admit that I relied in large part on others who helped me bring 
;;     this to a satisfactory completion.  My references included these sites:
;;
;;     http://www.cs.utep.edu/interval-comp/hayes.pdf
;;     http://en.wikipedia.org/wiki/Interval_arithmetic#Implementations
;;     http://jots-jottings.blogspot.com/2011/09/sicp-exercise-216-ieee-interval.html
;;     http://danboykis.com/2009/08/exercise-2-16-of-sicp/
;;     http://eli.thegreenplace.net/2007/07/27/sicp-section-214/
;;     
;;     I did learn a lot, but the conclusions drawn here is based on the work
;;     others.
;;
;; To begin, we examine the question of why equivalent algebraic expressions
;; may lead to different answers.  As we noted first in e2.15, For each 
;; interval operation, the uncertainty in the resulting interval increases.
;; The best procedure, then, will be those that minimize the number of 
;; operations that increase the uncertainty.
;; 
;; With this understanding, we can take up the second part of the question,
;; can we devise an interval-arithmetic package that does not have the 
;; shortcoming that different algebraically equivalent expressions may
;; result in different answers.
;; 
;; In brief, no.
;; 
;; To understand why, we consider two factors.  First, is the idea described
;; above, that different equations do indeed produce different answers.  If
;; it is possible to re-write the equations such that each term is used once
;; and we minimize the number of operations, then that representation will
;; produce the correct, minimal interval.  Let us assume that all equations
;; could be re-written such that each term is used only once. (This is not
;; true, but that proof is beyond the scope here.)  In that case, if we
;; postulate a system that is capable of reducing given equations such that
;; their solution is a potentially different equation that includes each term
;; just once.  If such a system existed, we can be sure that however complex
;; creating such an implementation might be, that it should produce equal
;; answers when given algebraically equivalent equations.  (Because both
;; inputs would be reduced ultimatly to a potentially different third equation
;; that used each term once.)
;; 
;; However, we must consider one the second factor.  In arithmetic, we rely
;; on mathematical relations that describe how equations may be solved such as
;; x + -x = 0.  With intervals, however, some of these laws fail to hold.
;; (e.g. There is no -x interval that when added to x produces the zero 
;; interval.  Such an interval would need to have negative tolerance, a
;; tolerance less than zero, which is an impossibility.)  Because these
;; relations fail to hold, the algorithmic manipulations required to restate
;; equivalent equations in forms that use their terms only once necessarily
;; change the resulting interval.
;; 
;; Thus, it is impossible to create an interval mathmatics system that does
;; produces equal answers for algorithmically equivalent equations.
