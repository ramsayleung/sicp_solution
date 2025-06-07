#lang racket
(define (square-list items)
  (if (null? items)
      (list)
      (cons (* (car items) (car items))
	    (square-list (cdr items)))))

(define (square-list-v2 items)
  (map (lambda (x) (* x x))
       items))
