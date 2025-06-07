#lang racket
(define (equal? x1 x2)
  (if (not (pair? x1))
      (eq? x1 x2)
      (and (equal? (car x1) (car x2))
	   (equal? (cdr x1) (cdr x2)))))

(equal? '(this is a list) '(this is a list)) ;=> #t
(equal? '(this is a list) '(this (is a) list)) ;=> #f
