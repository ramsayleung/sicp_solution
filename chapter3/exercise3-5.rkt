#lang racket
(require "rand.rkt")
(require "monte-calro.rkt")

;; 蒙特卡罗积分的思路:
;; 在矩形内随机生成大量点 (x, y)
;; 计算满足 P(x, y) 的点的比例, 如半径为3、中心在 (5,7) 的圆，其谓词为
;; P(x, y) = (x-5) ^2 + (y-7) ^ 2 ≤ 9
;; 估算圆在矩形内的面积:
;; 圆的面积 ~= 矩形面积 * 满足p的点 / 总的点数

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  ;; 判断是否落在圆内
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (predicate x y)))

  (let ((rectangle-area (* (abs (- x1 x2))
                           (abs (- y1 y2)))))
    (* (exact->inexact rectangle-area) (monte-carlo trials experiment))))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  ;; 以(5,7)为圆心，半径为3的圆，以(2,4)和(8,10)作为对角点的矩形包围着
  ;; 这个圆, 计算这个圆的面积（假设 π未知），结果应该接近 28.274
  (define (square x)
    (* x x))

  (define (in-circle? x y)
    (<= (+ (square (- x 5)) (square (- y 7)))
        (square 3)))

  (define (estimate-area-close? trials tolerance)
    (let ((area-estimate (estimate-integral in-circle? 2 8 4 10 trials)))
      (< (abs (- area-estimate 28.27431)) tolerance)))

  (define module-test
    (test-suite
     "Tests for in-circle?"
     (check-true (in-circle? 5 6))
     (check-true (in-circle? 6 5))
     (check-false (in-circle? 10 10))

     "Tests for estimate-integral"
     (check-true (estimate-area-close? 10000 2.0))
     (check-true (estimate-area-close? 100000 1.5))
     (check-true (estimate-area-close? 10000000 1.3))
     (check-exn exn:fail? (lambda () (estimate-area-close? 0)) "Should throw exception for 0 trials")
     ))
  (run-tests module-test))
