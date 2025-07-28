#lang racket
(require "stream.rkt")

(define (show x)
  (display-line x)
  x)


;;; 输出 0, 因为 (stream-enumerate-interal 0 10) 会生成一组 0 - 10 的
;;; 流, 而 stream-map 会把第一个流应用在 show 函数, 延时未执行的流是
;;; 1 - 10
(define x (stream-map show (stream-enumerate-interal 0 10)))

;;; 输出 1 2 3 4 5，因为 stream-ref 需要递归访问 0 - 5, 因为 0 已经被
;;; 处理(memo-proc), 所以只会输出 1 - 5
;;; 返回 5, show 函数不只打印，还会返回值，而5是最后一个值，所以也被返
;;; 回了.
(stream-ref x 5)

;;; 输出 6 7, 因为 stream-ref 需要递归访问 0 - 7, 而 0-5 都已经被计算
;;; 了，所以不会被打印，只会输出 6 - 7
;;; 返回 7
(stream-ref x 7)

;;; 明确输出和返回值的差别
