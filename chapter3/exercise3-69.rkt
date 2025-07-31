#lang racket
(require "stream.rkt")
(require "infinite-stream.rkt")

(define (triples s t u)
  (if (or (stream-null? s)
          (stream-null? t)
          (stream-null? u))
      the-empty-stream
      (cons-stream
       (list (stream-car s) (stream-car t) (stream-car u))
       (interleave
        (stream-map (lambda (pair) (list (stream-car s) (car pair) (cadr pair)))
                    (stream-cdr (pairs t u)))
        (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))
(define (square x)
  (* x x))

(define (pythagorean triple-stream)
  (stream-filter (lambda (triple)
                   (= (+ (square (car triple))
                         (square (cadr triple)))
                      (square (caddr triple))))
                 triple-stream))
(module+ test
  (require rackunit)

  (test-case "Test for triples"
             (define s1 (list-to-stream '(1 2 3)))
             (define s2 (list-to-stream '(1 2 3)))
             (define s3 (list-to-stream '(1 2 3)))
             (check-equal? (stream-to-list (triples s1 s2 s3) 20) '((1 1 1)
                                                                    (1 1 2)
                                                                    (2 2 2)
                                                                    (1 2 2)
                                                                    (2 2 3)
                                                                    (1 1 3)
                                                                    (3 3 3)
                                                                    (1 2 3)
                                                                    (2 3 3)
                                                                    (1 3 3)))
             (test-case "Test for pythagorean"
                        (define s1 (stream-enumerate-interval 1 10))
                        (define s2 (stream-enumerate-interval 1 10))
                        (define s3 (stream-enumerate-interval 1 10))
                        (check-equal? (stream-to-list (pythagorean (triples s1 s2 s3)) 2) '((3 4 5) (6 8 10)))
                        )
             ))
