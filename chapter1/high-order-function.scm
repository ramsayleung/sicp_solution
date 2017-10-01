(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a ) next b))))
(define (inc c) (+ c 1))
(define (cube n)
  (* n n n))
(define (sum-cube a b)
  (sum cube a inc b))


(define (sum-integers a b)
  (define (identity x)x)
  (sum identity a inc b)
  )

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))
