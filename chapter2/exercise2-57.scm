#lang racket
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product-list lst)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	  ((=number? m1 1) m2)
	  ((=number? m2 1) m1)
	  ((and (number? m1) (number? m2))
	   (* m1 m2))
	  (else (list '* m1 m2))))
  (if (= (length lst)2)
      (make-product (car lst) (cadr lst))
      (make-product (car lst) (make-product-list (cdr lst)))))

(define (make-sum-list lst)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2))
	   (+ a1 a2))
	  (else
	   (list '+ a1 a2))))
  (if (= (length lst) 2)
      (make-sum (car lst) (cadr lst))
      (make-sum (car lst) (make-sum-list (cdr lst)))))

(define (make-sum . l)
  (make-sum-list l))

(define (make-product . l)
  (make-product-list l))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (let ((aug (cddr s)))
    (if (= (length aug) 1)
	(car aug)
	(make-sum-list aug))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (let ((mult (cddr p)))
    (if (= (length mult) 1)
	(car mult)
	(make-product-list mult))))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var)
	     1
	     0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (deriv (multiplier exp) var)
			(multiplicand exp))))

	(else
	 (error "unknown expression type -- DERIV" exp))))

;;;

(deriv '(+ x 3) 'x) ;=> 1
(deriv '(* x y) 'x) ;=> y 
(deriv '(* x y (+ x 3)) 'x); => (+ (* x y) (* y (+ x 3)))
