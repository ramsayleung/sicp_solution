#lang racket
(require "stream.rkt")
(require "infinite-stream.rkt")
(require "exercise3-70.rkt")

(define cube-weight (lambda (pair) (let ((i (car pair))
                                         (j (cadr pair)))
                                     (+ (* i i i)
                                        (* j j j)))))
(define cube-sum-stream (pairs-weighted integers integers cube-weight))

(define (find-ramanujan-numbers s)
  (if (or (stream-null? s)
          (stream-null? (stream-cdr s)))
      (error "The given stream is empty")
      (let* ((first (stream-car s))
             (second (stream-car (stream-cdr s)))
             (first-w (cube-weight first))
             (second-w (cube-weight second)))
        (if (= first-w second-w)
            ;; 找到权重相同的前后相邻两个序对
            (cons-stream
             (list first-w first second)
             ;; 将有后面有相同权重的序对排除
             (find-ramanujan-numbers (stream-filter (lambda (x) (not (= (cube-weight x) first-w)))
                                                    (stream-cdr s)))
             )
            ;; 没找到，递归寻找
            (find-ramanujan-numbers (stream-cdr s) )))))

(define ramanujan-stream (find-ramanujan-numbers cube-sum-stream))

(module+ test
  (require rackunit)

  (test-case "Test for ramanujan-numbers"
             (check-equal? (stream-take-n ramanujan-stream 5) '((1729 (1 12) (9 10))
                                                                (4104 (2 16) (9 15))
                                                                (13832 (2 24) (18 20))
                                                                (20683 (10 27) (19 24))
                                                                (32832 (4 32) (18 30)))))
  )
