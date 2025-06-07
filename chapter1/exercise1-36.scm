#lang racket
(define tolerence 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))tolerence))
  (define (try guess)
    (let ((next (f guess)))
      (display "guess: ")
      (display next)
      (newline)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;;; no average
(define (find-root-by-x y)
  (fixed-point (lambda (x) (/ (log y) (log x)))
	       10.0))
(define (average x y)
  (/ (+ x y)2))
;;; average
(define (find-root-by-x-average y)
  (fixed-point (lambda (x) (average x (/ (log y) (log x))))))
