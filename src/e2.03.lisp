;Exercise 2.3:
;
;  Implement a representation for rectangles in a plane. (Hint: You may want to
;  make use of Exercise 2.2.) In terms of your constructors and selectors, 
;  create procedures that compute the perimeter and the area of a given
;  rectangle.  Now implement a different representation for rectangles.  Can you
;  design your system with suitable abstraction barriers, so that the same 
;  perimeter and area procedures will work using either representation?

;; To begin we carry forward the representation of point from e2.02
(defun make-point (x y)
  (cons x y))

(defun x-point (x)
  (car x))

(defun y-point (x)
  (cdr x))

;; Now we can define a rectangle by two offset points representing the upper
;; left corner, and the lower right corner.
(defun make-rect (ul lr)
  (cons ul lr))
(defun ul-rect (r)
  (car r))
(defun lr-rect (r)
  (cdr r))
(defun ur-rect (r)
  (make-point (x-point (car r)) (y-point (cdr r))))
(defun ll-rect (r)
  (make-point (x-point (cdr r)) (y-point (car r))))

(defun height-rect (r)
  (- (x-point (ul-rect r)) (x-point (ll-rect r))))
(defun width-rect (r)
  (- (y-point (ur-rect r)) (y-point (ul-rect r))))

;; The area of a rectangle is its height and its width
(defun area-rect (r)
  (* (height-rect r) (width-rect r)))
 
(defun perimeter-rect (r)
  (+ (* (height-rect r) 2.0) 
     (* (width-rect r)  2.0)))

;; Test
(= 16 (perimeter-rect (make-rect (make-point 5 1) (make-point 1 5))))
(= 16 (area-rect (make-rect (make-point 5 1) (make-point 1 5))))

(= 10 (perimeter-rect (make-rect (make-point 5 5) (make-point 1 6))))
(= 4 (area-rect (make-rect (make-point 5 5) (make-point 1 6))))

;; We can create an alternative representation for rectangle if
;; we define a rectangle as a point and the height and width of the
;; rectangle from that point.
(defun make-rect-alt (ll height width)
  (cons ll (cons height width)))
(defun height-rect-alt (r)
  (car (cdr r)))
(defun width-rect-alt (r)
  (cdr (cdr r)))

(defun ul-rect-alt (r)
  (make-point (+ (x-point (car r)) (height-rect-alt r)) (y-point (car r))))
(defun lr-rect-alt (r)
  (make-point (x-point (car r)) (+ (y-point (car r)) (width-rect-alt r)))) 
(defun ur-rect-alt (r)
  (make-point (+ (x-point (car r)) (height-rect r)) (+ (y-point (car r)) (width-rect-alt r))))
(defun ll-rect-alt (r)
  (cdr r))

;; Re-implementing area and perimeter with different names to make use of the
;; alternate reprepresentation as required in the problem.  Use -alt naming
;; convention so as not to collide with the other rect representation from this
;; problem
(defun area-rect-alt (r)
  (* (height-rect-alt r) (width-rect-alt r)))
 
(defun perimeter-rect-alt (r)
  (+ (* (height-rect-alt r) 2.0) 
     (* (width-rect-alt r)  2.0)))

(= 16 (perimeter-rect-alt (make-rect-alt (make-point 5 1) 4 4)))
(= 16 (area-rect-alt (make-rect-alt (make-point 5 1) 4 4)))

(= 10 (perimeter-rect-alt (make-rect-alt (make-point 5 5) 4 1)))
(= 4 (area-rect-alt (make-rect-alt (make-point 5 5) 4 1)))

