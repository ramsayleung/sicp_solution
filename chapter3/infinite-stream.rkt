#lang racket
(require "stream.rkt")

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (divisible? n x)
  (zero? (remainder n x)))

;;; https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(module+ test
  (require rackunit)

  (test-case "Test for divisible?"
             (check-true (divisible? 12 2))
             (check-true (divisible? 12 6))
             (check-false (divisible? 3 4))
             )

  (test-case "Test for sieve"
             (define primes (sieve (integers-starting-from 2)))
             (check-equal? (stream-ref primes 50) 233)
             )
  )
