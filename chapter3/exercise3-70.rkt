#lang racket
(require "stream.rkt")
(require "infinite-stream.rkt")

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1-head (stream-car s1))
                (s2-head (stream-car s2))
                (w1 (weight s1-head))
                (w2 (weight s2-head)))
           (cond ((<= w1 w2)
                  (cons-stream s1-head (merge-weighted (stream-cdr s1) s2 weight)))
                 (else
                  (cons-stream s2-head (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (pairs-weighted s t weight)
  (if (or (stream-null? s)
          (stream-null? t))
      the-empty-stream
      (cons-stream
       (list (stream-car s) (stream-car t))
       (merge-weighted
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
        (pairs-weighted (stream-cdr s) (stream-cdr t) weight)
        weight))))
(provide pairs-weighted merge-weighted)

(module+ test
  (require rackunit)

  (define not-div-by-2-3-5
    (stream-filter
     (lambda (x)
       (not (or (= (remainder x 2) 0)
                (= (remainder x 3) 0)
                (= (remainder x 5) 0))))
     integers))

  (test-case "Test for merge-weighted"
             (define s1 (list-to-stream '(1 2 3 5 7)))
             (define s2 (list-to-stream '(2 -4 5 8 9)))
             (define weight (lambda (x) (abs x)))
             (define s3 (merge-weighted s1 s2 weight))
             (check-equal? (stream-take-n s3 10) '(1 2 2 3 -4 5 5 7 8 9))
             )

  (test-case "Test for pairs-weighted with weight = i+j"
             (define s1 (list-to-stream '(1 2 3)))
             (define s2 (list-to-stream '(1 2 3)))
             (define weight (lambda (pair) (+ (car pair) (cadr pair))))
             (define s3 (pairs-weighted s1 s2 weight))
             (check-equal? (stream-take-n s3 10) '((1 1) (1 2) (1 3) (2 2) (2 3) (3 3)))
             )

  (test-case "Test for pairs-weighted with weight = 2j + 3j + 5ij"
             (define weight (lambda (pair) (let ((i (car pair))
                                                 (j (cadr pair)))
                                             (+ (* 2 i)
                                                (* 3 j)
                                                (* 5 i j)))))
             (define result (pairs-weighted not-div-by-2-3-5 not-div-by-2-3-5 weight))
             (check-equal? (stream-take-n result 5) '((1 1) (1 7) (1 11) (1 13) (1 17))))
  )
