#lang racket
(require "stream.rkt")
(require "exercise3-74.rkt")
(require "exercise3-50.rkt")

(define (smooth input-s)
  (if (or (stream-null? input-s)
          (stream-null? (stream-cdr input-s)))
      the-empty-stream
      (let ((prev (stream-car input-s))
            (cur (stream-car (stream-cdr input-s))))
        (cons-stream
         (/ (+ cur prev) 2)
         (smooth (stream-cdr input-s))))))


(define (make-zero-crossings-smooth input-s)
  (let ((smooth-stream (smooth input-s)))
    (map-stream sign-change-detector smooth-stream (cons-stream 0 smooth-stream))))


(module+ test
  (require rackunit)

  (test-case "Test for smooth"
             (define s1 (list-to-stream '(1 3 5 9 13 17)))
             (check-equal? (stream-take-n (smooth s1) 6) '(2 4 7 11 15))
             )

  (test-case "Test for make-zero-crossings-smooth"
             (define sense-data (list-to-stream '(10 2 -2 -10 10 -1)))
             (define crossings (make-zero-crossings-smooth sense-data))
             (check-equal? (stream-take-n crossings 6) '(0 0 -1 1 0)))
  )
