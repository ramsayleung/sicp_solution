#lang racket
(define (iter-cont-frac n d k)
  (define (loop result term)
    (if (= term 0)
	result
	(loop (/ (n term)
		 (+ (d term )result))
	      (- term 1))))
  (loop 0 k))
(define (tan-cf x k)
  (iter-cont-frac (lambda (i)
		    (if (= i 1)
			x
			(- (* x x))))
		  (lambda (i)
		    (- (* 2 i)1))
		  k))
