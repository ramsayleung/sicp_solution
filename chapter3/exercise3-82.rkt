#lang racket
(require "stream.rkt")
(require "infinite-stream.rkt")
(require "exercise3-50.rkt")
(require "exercise3-5.rkt")

(define (map-successive-pairs f s)
  (if (stream-null? s)
      the-empty-stream
      (let ((head (stream-car s)))
        (if (pair? head)
            (cons-stream
             (f (car head) (cadr head))
             (map-successive-pairs f (stream-cdr s)))
            (error "uknown parameter type " head)))))

(define (integral-experience-stream predicate x1 x2 y1 y2)
  (map-successive-pairs predicate
                        (map-stream list
                                    (random-range-stream x1 x2)
                                    (random-range-stream y1 y2))))

(define (estimate-integral-stream predicate x1 x2 y1 y2)
  (let ((rectangle-area (* (abs (- x1 x2))
                           (abs (- y1 y2)))))
    (stream-map (lambda (p) (* (exact->inexact rectangle-area) p))
                (monte-carlo (integral-experience-stream predicate x1 x2 y1 y2) 0 0)))
  )

(module+ test
  (require rackunit)
  ;; 以(5,7)为圆心，半径为3的圆，以(2,4)和(8,10)作为对角点的矩形包围着
  ;; 这个圆, 计算这个圆的面积（假设 π未知），结果应该接近 28.274
  (define circle-area 28.27431)
  (define (estimate-area-close? area-estimate tolerance)
    (< (abs (- area-estimate circle-area)) tolerance))

  (test-case "Test for estimate-integral-stream"
             (define estimated-area-stream (estimate-integral-stream in-circle? 2 8 4 10))
             (check-true (estimate-area-close? (stream-ref estimated-area-stream 10000) 2.0))
             (check-true (estimate-area-close? (stream-ref estimated-area-stream 100000) 1.5))
             (check-true (estimate-area-close? (stream-ref estimated-area-stream 1000000) 1.4))
             )
  )
