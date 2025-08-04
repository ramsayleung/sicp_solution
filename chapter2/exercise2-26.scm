#lang racket
(define x (list 1 2 3))
(define y (list 4 5 6))

(module+ test
  (require rackunit)

  (test-case "Test for append, cons, and list"
             (check-equal? (append x y) '(1 2 3 4 5 6))
             (check-equal? (cons x y) '((1 2 3) 4 5 6))
             (check-equal? (list x y) '((1 2 3) (4 5 6)))
             )
  )
