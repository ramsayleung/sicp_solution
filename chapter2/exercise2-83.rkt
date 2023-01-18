#lang racket
(require "generic_operation.scm")
(require "section-2-1-1.scm")

(define (install-scheme-number-raise)

  (define (scheme-number->rational n)
    (make-rational n 1))

  (put 'raise '(scheme-number) (lambda (x) (scheme-number->rational x))))


(define (install-rational-number-raise)
  (define (rational->real r)
    (make-real (/ (numer r) (denom r))))

  (put 'raise '(rational) (lambda (x) (rational->real x)))

  'done
  )

(define (install-real-number-package)
  (define (tag x)
    (attach-tag 'real x))

  (put 'make 'real (lambda (x) (tag x)))

  'done)

(define (make-real x)
  ((get 'make 'real) x))

(define (install-real-number-raise)
  (define (real->complex r)
    (make-complex-from-real-imag r 0))

  (put 'raise '(real) (lambda (x) (real->complex x)))

  'done
  )

(define (raise x) (apply-generic 'raise x))

;;; Install package
(install-real-number-package)

(install-scheme-number-raise)
(install-rational-number-raise)
(install-real-number-raise)

;;; Test
(raise (make-scheme-number 10)); => '(rational 10 . 1)
(raise (raise (make-scheme-number 10))); => '(real . 10)
(raise (raise (raise (make-scheme-number 10)))); => '(complex rectangular 10 . 0)

(provide raise install-real-number-package make-real)
