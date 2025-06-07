#lang racket
(define (filter-accumulate condition combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (condition a)
	  (combiner (term a)
		    (filter-accumulate condition combiner null-value term (next a) next b))
	  (combiner null-value
		    (filter-accumulate condition combiner null-value term (next a) next b))
	  )))

(define (filter-sum condition term a next b)
  (filter-accumulate condition + 0 term a next b))

(define (inc a) (+ a 1))

(define (identity x)x)

(define (prime? n)
  (define (next next-divisor)
    (cond ((= next-divisor 2)3)
	  (else (+ next-divisor 2))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (square n)
    (* n n)) 
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor)n )n)
	  ((divide? test-divisor n)test-divisor)
	  (else (find-divisor n (next test-divisor)))))
  (define (divide? a b)
    (= (remainder b a) 0))
  (= (smallest-divisor n)n))

;;; a
(define (sum-even-integers a b)
  (filter-sum prime? identity a inc b))




