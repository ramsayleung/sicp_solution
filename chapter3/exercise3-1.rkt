#lang racket
(define (make-accumulator total)
  (lambda (next)
    (begin (set! total (+ total next))
           total)))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  (define acc (make-accumulator 5))
  (define module-test
    (test-suite
     "Tests for make-accumulator"
     (check-equal? (acc 5) 10)
     (check-equal? (acc 10) 20)
     (check-equal? (acc -30) -10)
     ))

  (run-tests module-test))
