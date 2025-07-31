#lang racket
(require "stream.rkt")

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

;;; 返回1-20的流:
;;; 返回一个 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171
;;; 190 210 的流, 因为 string-enumerate-interal 会生成 1-20 的流, 然后
;;; accum 会累加前面的流, 然后再生成一个新的流
(define seq (stream-map accum (stream-enumerate-interval 1 20)))

;;; 返回 6, 10, 28, 36, 66, 78, 120, 136, 190, 210 的流
(define y (stream-filter even? seq))

;;; 返回 10,15,45,55,105,120,190,210 的流
(define z (stream-filter (lambda (x)
                           (= (remainder x 5)
                              0))
                         seq))
;;; 返回 136
(stream-ref y 7)

;;; 输出 10,15,45,55,105,120,190,210
(display-stream z)

;;; 如果我们简单地将 (delay <expr>) 实现成 (lambda <expr>),
;;; 而不使用 memo-proc 提供的优化，响应的结果会是一样的，但是存在非常
;;; 多无效的重复计算。
;;; memo-proc 的作用就是确保已经计算过的结果，不会重复计算，但对计算结
;;; 果没有影响
