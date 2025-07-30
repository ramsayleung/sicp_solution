#lang racket
(require "stream.rkt")
(require "iteration-stream.rkt")

(define (stream-limit s tolerance)
  (if (or (stream-null? s) (stream-null? (stream-cdr s)))
      (error "The given stream contains less than 2 consecutive items " s)
      (let ((first (stream-car s))
            (second (stream-car (stream-cdr s))))
        (if (<= (abs (- first second)) tolerance)
            second
            (stream-limit (stream-cdr s) tolerance)))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(module+ test
  (require rackunit)

  (test-case "Test for stream-limit"
             (check-equal? (sqrt 2 0.5) 1.5)
             (check-equal? (sqrt 2 0.1) 1.4166666666666665)
                           (check-equal? (sqrt 2 0.01) 1.4142156862745097)
                           (check-equal? (sqrt 2 0.0001) 1.4142135623746899)
                           (check-equal? (sqrt 2 0.0000000001) 1.414213562373095)
                           (check-equal? (sqrt 4 0) 2.0)
                           )
  )
