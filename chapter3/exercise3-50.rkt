#lang racket
(require "stream.rkt")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(provide (rename-out [stream-map map-stream]))

(module+ test
  (require rackunit)

  (test-case "Test for two streams mapping"
             (define s1 (list-to-stream '(1 2 3 4)))
             (define s2 (list-to-stream '(10 20 30 40)))
             (define added (stream-map + s1 s2))
             (check-equal? (stream-to-list added 4) '(11 22 33 44))
             )

  (test-case "Test for different length streams(stop at shortest)"
             (define s1 (list-to-stream '(1 2)))
             (define s2 (list-to-stream '(10 20 30 40)))
             (define added (stream-map + s1 s2))
             (check-equal? (stream-to-list added 4) '(11 22))
             )
  )
