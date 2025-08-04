#lang racket
(define (fringe items)
  (if (not (pair? items))
      items
      (if (pair? (car items))
          (append (fringe (car items)) (fringe (cdr items)))
          (append (list (car items)) (fringe (cdr items))))))

(define x (list (list 1 2) (list 3 4)))
(module+ test
  (require rackunit)

  (test-case "Test for fringe"
             (check-equal? (fringe x) '(1 2 3 4))
             (check-equal? (fringe (list x x)) '(1 2 3 4 1 2 3 4))
             )
  )
