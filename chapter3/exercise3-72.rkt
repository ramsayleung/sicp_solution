#lang racket
(require "stream.rkt")
(require "infinite-stream.rkt")
(require "exercise3-70.rkt")

(define square-weight (lambda (pair) (let ((i (car pair))
                                           (j (cadr pair)))
                                       (+ (* i i)
                                          (* j j)))))
(define square-pairs (pairs-weighted integers integers square-weight))

(define (find-square-sum-of-3ways s)
  (if (or (stream-null? s)
          (stream-null? (stream-cdr s))
          (stream-null? (stream-cdr (stream-cdr s))))
      (error "The given stream contains less than 3 elements")
      (let* ((first (stream-car s))
             (second (stream-car (stream-cdr s)))
             (third (stream-car (stream-cdr (stream-cdr s))))
             (first-w (square-weight first))
             (second-w (square-weight second))
             (third-w (square-weight third)))
        (if (and (= first-w second-w) (= second-w third-w))
            ;; 找到两个平方数之和的三种表达方式
            (cons-stream
             (list first-w first second third)
             ;; 将找到的有相同权重的三个序对排除
             (find-square-sum-of-3ways
              (stream-cdr (stream-cdr (stream-cdr s)))))
            ;; 没找到，递归寻找
            (find-square-sum-of-3ways (stream-cdr s) )))))

(define 3ways-square-sum-stream (find-square-sum-of-3ways square-pairs))

(module+ test
  (require rackunit)
  (test-case "Test for square sum of 3ways"
             (check-equal? (stream-take-n 3ways-square-sum-stream 3) '((325 (1 18) (6 17) (10 15))
                                                                       (425 (5 20) (8 19) (13 16))
                                                                       (650 (5 25) (11 23) (17 19))))
             )
  )
