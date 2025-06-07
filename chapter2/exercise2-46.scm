#lang racket
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

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))
(add-vect v1 v2); =>(4 . 6)
(sub-vect v1 v2); =>(-2 . -2)
(scale-vect 4 v1); =>(4 . 8)

(provide sub-vect)
