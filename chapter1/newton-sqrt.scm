#lang racket
(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? guess)
    (< (abs (- (square guess) x))
       0.001))

  (define (try guess)
    (if (good-enough? guess)
	guess
	(try (improve guess))))

  (try 1.0))
