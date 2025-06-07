#lang racket
;;; 欧几里得算法求最大公约数
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (standardized-make-rat n d)
  (let* ((common-divisor (gcd n d))
         (sign-factor (if (negative? d)- +))
         (g (sign-factor common-divisor)))
    (cons (/ n g) (/ d g))))

(define (number x)
  (car x))

(define (denom y)
  (cdr y))

(define (print-rat x)
  (display (number x))
  (display "/")
  (display (denom x))
  (newline))

(define negative-one-half
  (standardized-make-rat 1 -2))

(define positive-one-half
  (standardized-make-rat -1 -2))
