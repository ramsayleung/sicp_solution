(define (make-vect x y)
  (cons x y))

(define (xcor-vect vec)
  (car vec))

(define (ycor-vect vec)
  (cdr vec))

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))
