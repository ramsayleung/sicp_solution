;;; vector
(define (make-vect x y)
  (cons x y))

(define (xcor-vect vec)
  (car vec))

(define (ycor-vect vec)
  (cdr vec))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s vec)
  (make-vect (* (xcor-vect vec) s)
	     (* (ycor-vect vec) s)))

;;; segment
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

;;; frame
(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

;;;
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

(define (draw-line v1 v2)
  '())

(define (segments-painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))
