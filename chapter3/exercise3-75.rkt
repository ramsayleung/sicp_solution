#lang racket
(require "stream.rkt")
(require "exercise3-74.rkt")

(define (make-zero-crossings
         input-stream last-value)
  (let ((avpt
         (/ (+ (stream-car input-stream)
               last-value)
            2)))
    (cons-stream
     (sign-change-detector avpt last-value)
     (make-zero-crossings
      ;; last-value 参数传入的是平均值，而非前一个值
      (stream-cdr input-stream) avpt))))

(define (make-zero-crossings-smooth input-stream last-value last-avg)
  (if (stream-null? input-stream)
      the-empty-stream
      (let ((avpt
             (/ (+ (stream-car input-stream)
                   last-value)
                2)))
        (cons-stream
         (sign-change-detector avpt last-avg)
         (make-zero-crossings-smooth
          (stream-cdr input-stream)
          (stream-car input-stream)
          avpt)))))

(module+ test
  (require rackunit)

  (test-case "Test for make-zero-crossings-smooth"
             (define sense-data (list-to-stream '(10 2 -2 -10 10 -1)))
             (define crossings (make-zero-crossings-smooth sense-data 0 0))
             (check-equal? (stream-take-n crossings 6) '(0 0 0 -1 1 0)))
  )
