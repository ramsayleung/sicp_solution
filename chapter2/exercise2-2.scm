(define (make-point x y)
  (cons x y))

(define (x-point x)
  (car x))

(define (y-point y)
  (cdr y))

(define (midpoint point1 point2)
  (make-point (average (x-point point1) (x-point point2))
	      (average (y-point point1) (y-point point2))))

(define(make-segment start-point end-point)
  (cons start-point end-point)
  )

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))
(define (midpoint-segment segment)
  (midpoint (start-segment segment) (end-segment segment)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define point-a (make-point 1 1))
(define point-b (make-point 3 3))
(define segment-a (make-segment point-a point-b))
