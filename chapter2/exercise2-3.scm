(define (make-point x y)
  (cons x y))

(define (x-point x)
  (car x))

(define (y-point y)
  (cdr y))

(define (x-distance point1 point2)
  (abs (- (x-point point1) (x-point point2))))

(define (y-distance point1 point2)
  (abs (- (y-point point1) (y-point point2))))

(define (make-rectangle top-left-point bottom-right-point)
  (cons top-left-point bottom-right-point))

(define (top-left rec)
  (car rec))

(define (bottom-right rec)
  (cdr rec))

(define (get-rectangle-height rec)
  (x-distance (top-left rec) (bottom-right rec))
  )

(define (get-rectangle-width rec)
  (y-distance (top-left rec) (bottom-right rec)))

(define (rectangle-area rec)
  (* (get-rectangle-height rec) (get-rectangle-width rec)))

(define (rectangle-perimeter rec)
  (+ (* 2 (get-rectangle-height rec))
     (* 2 (get-rectangle-width rec))))

(define point-a (make-point 1 1))
(define point-b (make-point 3 3))
(define rec-a (make-rectangle point-a point-b))
