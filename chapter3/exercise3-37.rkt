#lang racket
(require "constraints.rkt")

;;; 有种 C++ 运算符重载的感觉了
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    ;;; x - y = z 可以转换成 x = y + z
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    ;; x / y = z 可以转换成 x = y * z
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (set-value! z x 'user)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(module+ test
  (require rackunit)

  (test-case "Test for c+"
             (define a (make-connector))
             (define b (make-connector))
             (define sum (c+ a b))
             (set-value! a 10 'user)
             (set-value! b 5 'user)
             (check-equal? (get-value sum) 15))

  (test-case "Test for c-"
             (define a (make-connector))
             (define b (make-connector))
             (define c (c- a b))
             (set-value! a 10 'user)
             (set-value! b 3 'user)
             (check-equal? (get-value c) 7))

  (test-case "Test for c*"
             (define a (make-connector))
             (define b (make-connector))
             (define product (c* a b))
             (set-value! a 10 'user)
             (set-value! b 5 'user)
             (check-equal? (get-value product) 50))

  (test-case "Test for c/"
             (define a (make-connector))
             (define b (make-connector))
             (define c (c/ a b))
             (set-value! a 6 'user)
             (set-value! b 2 'user)
             (check-equal? (get-value c) 3))

  (test-case "Test for cv"
             (define c (cv 6))
             (check-equal? (get-value c) 6))

  (test-case "Test for celsius-fahrenheit-converter"
             (define C (make-connector))
             (define F (celsius-fahrenheit-converter C))
             (set-value! C 25 'user)
             (check-equal? (get-value F) 77)
             )
  )
