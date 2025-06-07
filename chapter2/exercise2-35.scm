#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x)
                         (if (not (pair? x))
                             1
                             (count-leaves x)))
                       tree)))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x); => 4
