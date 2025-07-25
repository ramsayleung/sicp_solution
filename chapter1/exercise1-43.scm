#lang racket
;;; compose -> f(g(x))
;;; repeat -> f(f(f(x))), 即 g(x) 变成 f(x) 这个函数本身 compose(f (compose(f f)))
(define (compose f g)
  (lambda (x) (f (g x))))
(define (square x)
  (* x x))
(define (repeated f n)
  (if (< n 1)
      (lambda (x)x)
      (compose f (repeated f (- n 1)))))
