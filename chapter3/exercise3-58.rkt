#lang racket
(require "stream.rkt")

;;; 实际上实现了一个流式长除法算法, 用于将分数num/den转换为以radix为基数的无限小数表示
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(module+ test
  (require rackunit)

  (test-case "Test for expand"
             (define x (expand 3 8 10))
             ;; 3/8 = 0.375 (有限小数)
             (check-equal? (stream-take-n x 10) '(3 7 5 0 0 0 0 0 0 0))

             (define s (expand 1 7 10))
             ;; (expand 1 7 10) 对应 1/7 = 0.142857142857... (循环小数)
             (check-equal? (stream-take-n s 10) '(1 4 2 8 5 7 1 4 2 8))))
