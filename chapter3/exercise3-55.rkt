#lang racket
(require "stream.rkt")
(require "infinite-stream.rkt")

(define (partial-sums s1)
  (cons-stream (stream-car s1) (add-streams (partial-sums s1) (stream-cdr s1)))
  )

(module+ test
  (require rackunit)

  (test-case "Test for partial-sums"
             (define s1 (partial-sums integers))
             (check-equal? (stream-take-n s1 5) '(1 3 6 10 15))
  )
)
