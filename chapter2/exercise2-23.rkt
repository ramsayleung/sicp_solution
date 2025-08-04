#lang racket
(define (for-each proc items)
  (let ((a (map proc items)))
    (= 1 1)))

(module+ test
  (require rackunit)

  (test-case "Test for for-each"
             ;; 57
             ;; 321
             ;; 88
             (check-equal? (with-output-to-string (lambda () (for-each (lambda (x) (display x) (newline)) (list 57 321 88)))) "57\n321\n88\n")
             )
  )
