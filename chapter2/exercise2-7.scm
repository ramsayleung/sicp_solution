#lang racket
;;; 取区间的上下界，端点小者为下界，端点大者为上界
(define (upper-bound interval)
  (let ((left (car interval))
        (right (cdr interval)))
    (if (> left right)
        left
        right))
  )

(define (lower-bound interval)
  (let ((left (car interval))
        (right (cdr interval)))
    (if (< left right)
        left
        right)))

(provide upper-bound lower-bound)
