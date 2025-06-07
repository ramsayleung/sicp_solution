#lang racket
(define (cute-sqrt-iter guess x)
  (if (good-enough-improve? guess x )
      guess
      (cute-sqrt-iter (improve guess x) x)))
(define (good-enough-improve? guess x)
  (< (/ (abs (- guess (improve guess x)))
	guess) 
     0.001))
(define (improve guess x)
  (/ (+(/ x (square guess)) (* 2 guess))
     3)
  )

(define (square x)
  (* x x))

(define (cute-sqrt x)
  (cute-sqrt-iter 1.0 x))
