(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else
	 (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 1 (list 1 2 3)); => (1 2 3)
(adjoin-set 2 (list 1 3 5)); => (1 2 3 5)
(adjoin-set 6 (list 1 3 5)); => (1 3 5 6)

