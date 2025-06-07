#lang racket
(define (fringe items)
  (if (not (pair? items))
      items
      (if (pair? (car items))
	  (append (fringe (car items)) (fringe (cdr items)))
	  (append (list (car items)) (fringe (cdr items))))))

;;;
(define x (list (list 1 2) (list 3 4)))
(fringe x) ; => (1 2 3 4)
(fringe (list x x)) ; => (1 2 3 4 1 2 3 4)
