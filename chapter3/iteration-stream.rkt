#lang racket
(require "stream.rkt")

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (λ (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(provide sqrt-stream)
