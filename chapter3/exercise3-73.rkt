#lang racket
(require "stream.rkt")
(require "infinite-stream.rkt")

(define (RC resistor capacitor time-step)
  (lambda (input-stream initial-voltage)
    (add-streams (integral (scale-stream input-stream (/ 1 capacitor)) initial-voltage time-step)
                 (scale-stream input-stream resistor))))
(define RC1 (RC 5 1 0.5))

(module+ test
  (require rackunit)

  (test-case "Test for resistor-capacitor(RC)"
             (define v-result (RC1 ones 0))
             (check-equal? (stream-take-n v-result 5) '(5 5.5 6.0 6.5 7.0))
  )
)
