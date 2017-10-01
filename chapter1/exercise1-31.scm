;;; a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))
(define (identity x)x)
(define (next n) (+ n 1))

(define (factorial a b)
  (product identity a next b))

(define (john-wallis-term a)
  (if (even? a)
      (/ (+ a 2) (+ a 1))
      (/ (+ a 1) (+ a 2))))
(define (john-wallis a b)
  (product john-wallis-term a next b)
  )
;;; b

(define (iter-product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result))))
  (iter a 1))
