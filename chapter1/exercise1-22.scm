#lang racket
(require "time.rkt")

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (square n)
    (* n n)) 
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor)n )n)
	  ((divide? test-divisor n)test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (define (divide? a b)
    (= (remainder b a) 0))
  (= (smallest-divisor n)n))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (when (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  )

(define (search-iter current last)
  (when (<= current last) (timed-prime-test current))
  (when (<= current last) (search-iter (+ current 2)last)))

(define (search-for-primes first last)
  (search-iter (if (even? first) (+ first 1)first)
	       (if (even? last) (+ last 1)last)))
