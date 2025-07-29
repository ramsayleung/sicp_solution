#lang racket
(require "stream.rkt")
(require "exercise3-50.rkt")
(require "infinite-stream.rkt")

(define (mul-stream s1 s2)
  (map-stream * s1 s2))

(define factorials (cons-stream 1 (mul-stream (stream-cdr integers) factorials)))

(module+ test
  (require rackunit)

  (test-case "Test for mul-stream"
             (define s1 (list-to-stream '(2 3 4 5)))
             (define s2 (list-to-stream '(3 4 5 6)))
             (define s3 (mul-stream s1 s2))
             (check-equal? (stream-ref s3 2) 20)
             (check-equal? (stream-to-list s3 4) '(6 12 20 30))
             )
  (test-case "Test for factorials"
             (check-equal? (stream-to-list factorials 5) '(1 2 6 24 120))
             ;; n = 0, n+1=1 的阶乘是1
             ;; n = 1, n+1=2 的阶乘是2
             ;; n = 2, n+1=3 的阶乘是6
             )
  )
