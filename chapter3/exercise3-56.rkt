#lang racket
(require "stream.rkt")
(require "infinite-stream.rkt")

;;; 合并排序
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((< s2car s1car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  ;; s1car = s2car
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))
(module+ test
  (require rackunit)

  (test-case "Test for merge"
             (define s1 (list-to-stream '(1 2 3 5 7)))
             (define s2 (list-to-stream '(2 4 5 8 9)))
             (define s3 (merge s1 s2))
             (check-equal? (stream-to-list s3 10) '(1 2 3 4 5 7 8 9))
             )
  (test-case "Test for S"
             (check-equal? (stream-to-list S 20) '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36))
             )
  )
