#lang racket

(require "generic_operation.scm")
(require "section-2-1-1.scm")
(require "complex.scm")

(define (install-scheme-number-zero?)
  (define (tag x)
    (attach-tag 'scheme-number x))
  
  (put '=zero? '(scheme-number) (lambda (x)(tag (= x 0))))
  'done)

(define (install-rational-number-zero?)
  (define (tag x)
    (attach-tag 'rational x))

  (put '=zero? '(rational) (lambda (x)(tag (zero-rat? x))))
  'done)

(define (install-rectangular-number-zero?)
  (define (tag x) (attach-tag 'rectangular x))

  (define (zero-rectangular? x)
    (and (= (real-part-rectangular x) 0)
         (= (imag-part-rectangular x) 0)))
  (put '=zero? '(rectangular) (lambda (x) (tag (zero-rectangular? x))))

  'done
  )

(define (install-polar-number-zero?)
  (define (tag x)(attach-tag 'polar x))

  (define (zero-polar? x)
    (and (= (magnitude-polar x) 0)
         (= (angle-polar x) 0)))

  (put '=zero? '(polar) (lambda (x) (tag (zero-polar? x))))
  'done)

(define (install-complex-number-zero?)

  (put '=zero? '(complex) =zero?)
  'done)

(define (=zero? x) (apply-generic '=zero? x))

(install-scheme-number-zero?)
(install-rational-number-zero?)
(install-rectangular-number-zero?)
(install-polar-number-zero?)
(install-complex-number-zero?)


;;; test case
(=zero? (make-scheme-number 1)); '(scheme-number . #f)
(=zero? (make-scheme-number 0)); '(scheme-number . #t)

(=zero? (make-rational 1 2)); '(rational . #f)
(=zero? (make-rational 0 2)); '(rational . #t)

(=zero? (make-complex-from-mag-ang 3 4)); '(rational . #t)
(=zero? (make-complex-from-mag-ang 0 0)); '(polar . #t)

(=zero? (make-complex-from-real-imag 3 4)); '(rectangular . #f)
(=zero? (make-complex-from-real-imag 0 0)); '(rectangular . #t)
