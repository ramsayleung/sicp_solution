#lang racket
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square a)
  (* a a))
(define (square-tree tree) (tree-map square tree))

(define x (list 1 (list 2 (list 3 4) 5)))
(square-tree x); => (1 (4 (9 16) 25))
