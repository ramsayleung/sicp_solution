#lang racket
(require "stream.rkt")
(require "exercise3-50.rkt")
(require "rand.rkt")

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
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (if (or (stream-null? s) (stream-null? t))
      the-empty-stream
      (cons-stream
       (list (stream-car s) (stream-car t))
       (interleave
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
        (pairs (stream-cdr s) (stream-cdr t))))))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define random-number-stream
  (cons-stream random-init
               (stream-map rand-update random-number-stream)))

(define (random-range-stream low high)
  (cons-stream (random-in-range low high)
               (random-range-stream low high))
  )

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-number-stream))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream)
      passed
      failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi-stream
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              ;; bypass division by zero problem
              (stream-filter (lambda (p) (not (= p 0)))
                             (monte-carlo cesaro-stream 0 0))))

(provide add-streams scale-stream ones integers fibs interleave pairs integral monte-carlo random-range-stream map-successive-pairs)

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
             (check-equal? (stream-take-n (interleave s1 s2) 10) '(1 6 2 7 3 8 4 9 5 10))
             )

  (test-case "Test for pairs"
             (define s1 (list-to-stream '(1 2 3)))
             (define s2 (list-to-stream '(1 2 3)))
             (define int-pairs (stream-take-n (pairs s1 s2) 4))
             (check-equal? int-pairs '((1 1) (1 2) (2 2) (1 3)))
             )

  (test-case "Test for integral"
             (define result (integral ones 0 0.5))
             (check-equal? (stream-take-n result 5) '(0 0.5 1.0 1.5 2.0))
             )
  )
