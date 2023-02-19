#lang racket
(require "generic_operation.scm")
(require "poly.rkt")
;;; 练习2-87
(define (=zero-poly? poly)
  (define (all-coeff-zero? term-list)
    (if (empty-termlist? term-list)
        #t
        (and (=zero? (coeff (first-term term-list)))
             (all-coeff-zero? (rest-terms term-list)))))
  (all-coeff-zero? (term-list poly)))

(put '=zero? '(polynomial) (lambda (p) (=zero-poly? p)))

