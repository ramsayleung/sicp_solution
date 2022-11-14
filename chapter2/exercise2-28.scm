(define (fringe items)
  (if (not (pair? items))
      items
      (if (pair? (car items))
	  (append (fringe (car items)) (fringe (cdr items)))
	  (append (list (car items)) (fringe (cdr items))))))

;;;
(fringe x) ; => (1 2 3 4)
(fringe (list x x)) ; => (1 2 3 4 1 2 3 4)
