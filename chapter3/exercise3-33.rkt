#lang racket
(require "constraints.rkt")
;;; c = (a + b) * 0.5

(define (averager a b c)
  (let ((sum (make-connector))
        (half (make-connector))
        )
    (constant 0.5 half)
    (adder a b sum)
    (multiplier sum half c)
    'ok))

(module+ test
  (require rackunit)

  (test-case "Test for averager with a and b are set"
             (define a (make-connector))
             (define b (make-connector))
             (define c (make-connector))
             (averager a b c)
             (probe "value a " a)
             (probe "value b " b)
             (probe "value c " c)
             (check-false (has-value? c))
             (set-value! a 2 'user)
             (set-value! b 4 'user)
             (check-equal? (get-value c) 3.0)
             (check-exn (regexp "Contradiction") (lambda () (set-value! a 212 'user)))
             )

  (test-case "Test for averager with c and b are set"
             (define a (make-connector))
             (define b (make-connector))
             (define c (make-connector))
             (averager a b c)
             (probe "value a " a)
             (probe "value b " b)
             (probe "value c " c)
             (check-false (has-value? c))
             (set-value! b 4 'user)
             (set-value! c 0 'user)
             (check-true (has-value? a))
             (check-equal? (get-value a) -4)
             )
  )
