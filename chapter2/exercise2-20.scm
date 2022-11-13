(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (same-parity? a b)
  (= (remainder a 2) (remainder b 2)))


(define (same-parity x . y)
  (define (filter-same-parity a items)
    (if (null? items)
	(list a)
	(if (same-parity? a (car items))
	    (append (list a) (filter-same-parity (car items) (cdr items)))
	    (filter-same-parity a (cdr items)))))

  (if (null? y)
      (filter-same-parity x (list))
      (filter-same-parity x y))
  )

;;;
(same-parity 1 2 3 4 5 6 7); => (1 3 5 7)
(same-parity 2 3 4 5 6 7); => (2 4 6)
