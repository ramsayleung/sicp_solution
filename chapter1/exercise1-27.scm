#lang racket

(define (square x)
  (* x x))
(define (expmod base exp m)
  (cond ((= exp 0)1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2)m))
		    m))
	(else(remainder (* base (expmod base (- exp 1)m))
			m))))

(define (full-fermat-prime? n)
  (define (iter a n)
    (if (= a n)true
	(if (=(expmod a n n)a)
	    (iter (+ a 1) n)
	    false)))
  (iter 1 n))
(define (fermat-prime-test n expected)
  (define (report-result n result expected)
    (display n)
    (display ":")
    (display result)
    (display ": ")
    (display (if (eq? result expected)
		 "Pass"
		 "Fuck")))
  (report-result n (full-fermat-prime? n) expected))
