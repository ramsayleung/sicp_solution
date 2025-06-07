#lang racket
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
(define (prime? n)
  (= (smallest-divisor n)n))
;;; (smallest-divisor 199)=199
;;; (smallest-divisor 1999)=1999
;;; (smallest-divisor 19999)=7
