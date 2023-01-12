#lang racket
(require "generic_operation.scm")
(require "section-2-1-1.scm")
(require "complex.scm")

(define (install-scheme-number-equ?)
  (define (tag x)
    (attach-tag 'scheme-number x))
  
  (put 'equ? '(scheme-number scheme-number) (lambda (x y)(tag (= x y))))
  'done)

(define (install-rational-number-equ?)
  (define (tag x)
    (attach-tag 'rational x))

  (put 'equ? '(rational rational) (lambda (x y)(tag (equal-rat? x y))))
  'done)

(define (install-rectangular-number-equ?)
  (define (tag x) (attach-tag 'rectangular x))

  (define (equal-rectangular x y)
    (and (= (real-part-rectangular x) (real-part-rectangular y))
         (= (imag-part-rectangular x) (imag-part-rectangular y))))
  (put 'equ? '(rectangular rectangular) (lambda (x y)(tag (equal-rectangular x y))))
  'done
  )

(define (install-polar-number-equ?)
  (define (tag x)(attach-tag 'polar x))

  (define (equal-polar x y)
    (and (= (magnitude-polar x) (magnitude-polar y))
         (= (angle-polar x) (angle-polar y))))

  (put 'equ? '(polar polar) (lambda (x y) (tag (equal-polar x y))))
  'done)

(define (install-complex-number-equ?)

  (put 'equ? '(complex complex) equ?)
  'done)

(define (equ? x y) (apply-generic 'equ? x y))


(install-scheme-number-equ?)
(install-rational-number-equ?)
(install-rectangular-number-equ?)
(install-polar-number-equ?)
(install-complex-number-equ?)

(equ? (make-scheme-number 1)(make-scheme-number 1)) ; '(scheme-number . #t)
(equ? (make-scheme-number 2)(make-scheme-number 1)) ; '(scheme-number . #f)

(equ? (make-rational 1 2)(make-rational 1 2)); '(rational . #t)
(equ? (make-rational 1 2)(make-rational 3 2)); '(rational . #f)

(equ? (make-complex-from-real-imag 3 4)(make-complex-from-real-imag 3 4)); '(rectangular . #t)
(equ? (make-complex-from-real-imag 3 4)(make-complex-from-real-imag 5 6)); '(rectangular . #f)
