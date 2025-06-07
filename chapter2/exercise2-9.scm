#lang racket
(require "exercise2-7.scm")
(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
		(make-interval (- (upper-bound y))
			       (- (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
				 (/ 1.0 (lower-bound y)))))

(define (get-interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;;; 
(define a (make-interval 3 9))
(define b (make-interval 1 5))

(get-interval-width a) ; => 3
(get-interval-width b) ; => 2

(get-interval-width (add-interval a b)) ; => 5
(get-interval-width (sub-interval a b)) ; => 5
(get-interval-width (mul-interval a b)) ; => 21
(get-interval-width (div-interval a b)) ; => 4.2
