#lang racket
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair items)
  (list-ref items (- (length items) 1)))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  (define list-ref-tests
    (test-suite
     "Tests for list-ref"
     (check-equal? (last-pair (list 23 72 149 34)) 34)))

  (run-tests list-ref-tests))
