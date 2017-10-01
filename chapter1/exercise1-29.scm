(define (inc c) (+ c 1))

(define (cube n)
  (* n n n))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a ) next b))))
(define (identity x)x)

(define (simpson-rule f a b n)
  (define h (/ (- b a)n))
  (define (simpson-term k)
    (define y (f (+ a (* k h))))
    (if (= k 0)
	(* 1 y)
	(if (even? k)
	    (* 4 y)
	    (* 2 y))))
  (* (/ h 3)(sum simpson-term 0 inc n) ))

