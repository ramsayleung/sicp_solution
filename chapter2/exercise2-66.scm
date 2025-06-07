#lang racket
(define (key node)
  (car node)) ; The first element of the list is the key

(define (entry node)
  (cadr node)) ; The second element of the list is the actual entry/data

(define (left-branch node)
  (caddr node)) ; The third element is the left sub-branch

(define (right-branch node)
  (cadddr node)) ; The fourth element is the right sub-branch

(define (look-up given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (entry set-of-records)))
         (entry set-of-records))
        (else (and (look-up given-key (left-branch set-of-records))
                   (look-up given-key (right-branch set-of-records))))))
