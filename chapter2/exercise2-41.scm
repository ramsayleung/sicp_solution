#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-iterval low high)
  (if (> low high)
      '()
      (cons low (enumerate-iterval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-iterval 1 (- i 1))))
           (enumerate-iterval 1 n)))

(define (make-sum-pairs pair n)
  (list (car pair) (cadr pair) (- n (+ (car pair) (cadr pair)))))

(define (sum-lte-n pair n)
  (<= (+ (car pair) (cadr pair)) n))

(define (ternary-sum-pairs n)
  (map (lambda (pair) (make-sum-pairs pair n))
       (filter (lambda (pair) (sum-lte-n pair n))
               (unique-pairs n))))

;;;
(ternary-sum-pairs 6) ;=> ((2 1 3) (3 1 2) (3 2 1) (4 1 1) (4 2 0) (5 1 0))
