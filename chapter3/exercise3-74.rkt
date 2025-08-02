#lang racket
(require "stream.rkt")
(require "exercise3-50.rkt")

(define (sign-change-detector current prev)
  (cond ((and (< prev 0) (>= current 0)) 1)
        ((and (>= prev 0) (< current 0)) -1)
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (if (stream-null? input-stream)
      the-empty-stream
      (cons-stream
       (sign-change-detector (stream-car input-stream) last-value)
       (make-zero-crossings (stream-cdr input-stream)
                            (stream-car input-stream)))))

(provide sign-change-detector make-zero-crossings)
(define sense-data (list-to-stream '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define zero-crossings (map-stream sign-change-detector (stream-cdr sense-data) sense-data))

(module+ test
  (require rackunit)

  (test-case "Test for make-zero-crossings"
             (define zero-crossings (make-zero-crossings sense-data 0))
             (check-equal? (stream-take-n zero-crossings 13) '(0 0 0 0 0 -1 0 0 0 0 1 0 0))
             )
  (test-case "Test for zero-crossings"
             (check-equal? (stream-take-n zero-crossings 11) '(0 0 0 0 -1 0 0 0 0 1 0))
             )
  )
