#lang racket

(require "ch2lib.scm")
(require "section-2-1-1.scm")
(require (submod "complex.scm" complex-op))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))

  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number (lambda (x) (tag x)))

  'done
  )

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  ;; numer denom make-rat add-rat sub-rat mul-rat div-rat from "section-2-1-1
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))

  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))

  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat x y))))
  'done
  )

(define (make-rational n d)
  ((get 'make 'rational)n d))


(define (install-complex-package)
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))

  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))

  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-mag-ang x y))))

  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))

  'done
  )

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex)x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex)r a))
