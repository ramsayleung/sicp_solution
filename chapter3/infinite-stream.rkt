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

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (if (stream-null? s2)
          the-empty-stream
          (cons-stream (stream-car s1)
                       (interleave s2 (stream-cdr s1))))))

(define (pairs s t)
  (if (or (stream-null? s) (stream-null? t))
      the-empty-stream
      (cons-stream
       (list (stream-car s) (stream-car t))
       (interleave
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
        (pairs (stream-cdr s) (stream-cdr t))))))

(provide add-streams scale-stream ones integers fibs interleave pairs)

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
  (test-case "Test for interleave"
             (define s1 (list-to-stream '(1 2 3 4 5)))
             (define s2 (list-to-stream '(6 7 8 9 10)))
             (check-equal? (stream-to-list (interleave s1 s2) 10) '(1 6 2 7 3 8 4 9 5 10))
             )

  (test-case "Test for pairs"
             (define s1 (list-to-stream '(1 2 3)))
             (define s2 (list-to-stream '(1 2 3)))
             (define int-pairs (stream-to-list (pairs s1 s2) 4))
             (check-equal? int-pairs '((1 1) (1 2) (2 2) (1 3)))
             )
  )
