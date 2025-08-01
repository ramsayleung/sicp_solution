#lang racket
(require "stream.rkt")
(require "infinite-stream.rkt")

;;; 需要混合另一个流是: (S1,T0), (S2, T0), (S3, T0), ..

;; (stream-map (lambda (x) (list (stream-car s) x))
;;             (stream-cdr t))
;;; 是生成 (S0, T1), (S0, T2), ..

;; 那么 (S1,T0), (S2, T0), (S3, T0) 就是
;; (stream-map (lambda (x) (list x (stream-car t)))
;;             (stream-cdr s))

(define (full-pairs s t)
  (if (or (stream-null? s) (stream-null? t))
      the-empty-stream
      (cons-stream
       ;; (S0,T0)
       (list (stream-car s) (stream-car t))
       (interleave (interleave
                    ;; (S0, T1), (S0, T2), ..
                    (stream-map (lambda (x) (list (stream-car s) x))
                                (stream-cdr t))
                    ;; (S1,T0), (S2, T0), (S3, T0)
                    (stream-map (lambda (x) (list x (stream-car t)))
                                (stream-cdr s)))
                   ;; (S1,T1), (S1, T2), (S2, T2) ...
                   (full-pairs (stream-cdr s) (stream-cdr t))
                   ))))

(module+ test
  (require rackunit)

  (test-case "Test for full-pairs"
             (define s1 (list-to-stream '(1 2 3)))
             (define s2 (list-to-stream '(1 2 3)))
             (define int-pairs (stream-take-n (full-pairs s1 s2) 9))
             (check-equal? int-pairs '((1 1) (1 2) (2 2) (2 1) (2 3) (1 3) (3 3) (3 1) (3 2)))
             )
  )
