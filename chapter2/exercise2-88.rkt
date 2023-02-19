#lang racket
(require "generic_operation.scm")
(require "poly.rkt")
;;; 先定义一个求负操作 neg
;;; 加法的表示：a + b.
;;; 减法的表示: a - b. 或者时a + (neg b). 所以只需要定义一个求负操作，就可以复用
;;; 已有的加法过程

(define (install-scheme-number-neg)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'neg '(scheme-number) (lambda (a) (tag (- a))))
  )

(define (neg x) (apply-generic 'neg x))

(define (install-polynomial-neg)
  (define (tag x)
    (attach-tag 'polynomial x))

  (define (neg-term terms)
    (if (empty-termlist? terms)
        terms
        (let ((first (first-term terms)))
          (adjoin-term (make-term (order first) (neg (coeff first)))
                       (neg-term (rest-terms terms))))))
  (define (neg-poly p)
    (make-poly (variable p)
               (neg-term (term-list p))))

  (put 'neg '(polynomial) (lambda (x) (tag (neg-poly p))))

  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))

  (put 'sub '(polynomial polynomial) (lambda (x) (tag (sub-poly p1 p2))))
  )
