#lang racket
(define (iter-cont-frac n d k)
  (define (loop result term)
    (if (= term 0)
	result
	(loop (/ (n term)
		 (+ (d term )result))
	      (- term 1))))
  (loop 0 k))
(define (euler-expansion k)
  (+ 2 (iter-cont-frac
	(lambda (i) 1)
	(lambda (i) (if (= (remainder i 3)2)
			(/ (+ i 1) (/ 3 2))
			1))
	k)))
