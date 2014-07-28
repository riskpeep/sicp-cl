;Exercise 1.45:
;
;  We saw in Section 1.3.3 that aempting to compute square roots by naively 
;  finding a fixed point of y → x/y does not converge, and that this can be 
;  fixed by average damping. The same method works for finding cube roots as 
;  fixed points of the average-damped y 7! x/y². Unfortunately, the process 
;  does not work for fourth roots — a single average damp is not enough to 
;  make a fixed-point search for y → x/y³ converge. On the other hand, if we
;  average damp twice (i.e., use the average damp of the average damp of 
;  y → x/y³) the fixed-point search does converge.  Do some experiments to 
;  determine how many average damps are required to compute nth roots as a 
;  fixedpoint search based upon repeated average damping of y → x/y^n-1. Use
;  this to implement a simple procedure for computing nth roots using 
;  fixed-point, average-damp, and the repeated procedure of Exercise 1.43. 
;  Assume that any arithmetic operations you need are available as primitives.
