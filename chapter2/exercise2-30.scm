#lang racket
(define (square a)
  (* a a))

(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else
         (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square-tree-v2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-v2 sub-tree)
             (square sub-tree)))
       tree))

(define x (list 1 (list 2 (list 3 4) 5)))

(module+ test
  (require rackunit)

  (test-case "Test for square-tree"
             (check-equal? (square-tree x) '(1 (4 (9 16) 25)))
             (check-equal? (square-tree-v2 x) '(1 (4 (9 16) 25)))
             )
  )
