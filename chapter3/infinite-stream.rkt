#lang racket
(require "stream.rkt")
(require "exercise3-50.rkt")

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

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs (cons-stream 0
                          (cons-stream 1
                                       (add-streams (stream-cdr fibs)
                                                    fibs))))
(define (add-streams s1 s2)
  (map-stream + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(provide add-streams scale-stream ones integers fibs)

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

  (test-case "Test for infinite stream"
             (check-equal? (stream-ref fibs 6) 8)
             )
  )
