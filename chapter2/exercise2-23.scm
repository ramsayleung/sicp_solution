#lang racket
(define (for-each proc items)
  (let ((a (map proc items)))
    (= 1 1)))

;;;
(for-each (lambda (x) (display x) (newline)) (list 57 321 88))
;; 57
;; 321
;; 88

