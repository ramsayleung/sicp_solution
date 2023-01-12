#lang racket
(require "coercion.rkt")
(require "generic_operation.scm")

(define (scheme-number->scheme-number n)n)
(define (complex->complex z) (display "complex->complex") z)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)

(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic-coercion 'exp x y))

;;; following added to Scheme-number package

(define (install-scheme-number-exp)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))
  'done)

(install-scheme-number-exp)

;;; a): 会死循环，一直循环调用apply-generic-coercion
;; (exp (make-complex-from-mag-ang 1 3) (make-complex-from-mag-ang 1 3))
