#lang racket
;;; peano arthimathic
(define (+ x y)
  (if (= x 0)
      y
      (+ (- x 1) (+ y 1))))
