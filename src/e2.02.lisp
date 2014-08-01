;Exercise 2.2:
;
;  Consider the problem of representing line segments in a plane. Each segment
;  is represented as a pair of points: a starting point and an ending point. 
;  Define a constructor make-segment and selectors start-segment and end-
;  segment that define the representation of segments in terms of points. 
;  Furthermore, a point can be represented as a pair of numbers: the x 
;  coordinate and the y coordinate.  Accordingly, specify a constructor make-
;  point and selectors x-point and y-point that define this representation.
;  Finally, using your selectors and constructors, define a procedure midpoint-
;  segment that takes a line segment as argument and returns its midpoint (the
;  point whose coordinates are the average of the coordinates of the 
;  endpoints).  To try your procedures, you’ll need a way to print points:
;
;  (define (print-point p)
;    (newline)
;    (display "(")
;    (display (x-point p))
;    (display ",")
;    (display (y-point p))
;    (display ")"))

;; We can define a point as a cons that contains the x and y coordinates.  A
;; segment, being two points, would be a cons that contains the two points.
;; 
;; We can build up the abstraction as follows
;; First, we begin by creating points
(defun make-point (x y)
  (cons x y))

(defun x-point (x)
  (car x))

(defun y-point (x)
  (cdr x))

;; Next we create segments and their selectors
(defun make-segment (start end)
  (cons start end))

(defun start-segment (seg)
  (car seg))

(defun end-segment (seg)
  (cdr seg))

;; And finally, we can define midpoint-segment.  The midpoint of a segment is 
;; the point that lies halfway between the start and end points of the segment.
;; We can find this point via the following equations:
;; 
;;          abs(start-x - end-x)                 abs(start-y - end-y)
;; mid-x = ----------------------       mid-y = ---------------------- 
;;                    2                                    2
;;
;; (Note these functons assume that all points are in the positive quadrant 
;;  where both x and y for both points are greater than zero.)
;;
(defun midpoint-segment (s)
  (let ((start-point (start-segment s))
        (end-point (end-segment s)))
    (make-point (/ (abs (- (x-point start-point)
                           (x-point end-point)))
                   2.0)
                (/ (abs (- (y-point start-point)
                           (y-point end-point)))
                   2.0))))

;; To support testing, we define print point as follows
(defun print-point (p)
  (format t "~%(~A,~A)" (x-point p) (y-point p)))

;; While we're at it, lets make a print-segment
(defun print-segment (s)
  (format t "~%(~A,~A)-(~A,~A)" 
          (x-point (start-segment s)) 
          (y-point (start-segment s))
          (x-point (end-segment s)) 
          (y-point (end-segment s))))

;; Now we can do some testing
;; Make some points
;; Note in CL let* is like let in that it defines named variables, however, 
;; unlike let, let* enables successive definitions to refer to the results
;; from previous definitions
(let* ((point1 (make-point 5 5))
      (point2 (make-point 1 5))
      (point3 (make-point 5 1))
      (point4 (make-point 0 0))
      (segment1 (make-segment point1 point2))
      (segment2 (make-segment point1 point4)))
  (print-point point1)
  (print-point point2)
  (print-point point3)
  (print-point point4)
  (print-segment segment1)
  (print-segment segment2)
  (print-point (midpoint-segment segment2)))
