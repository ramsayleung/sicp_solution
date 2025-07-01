#lang racket
(require compatibility/mlist)

(define (make-table)
  (mlist '*table*))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcar (mcar records)))
         (mcar records))
        (else (assoc key (mcdr records)))))

;;; O(N)
(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table (mcons (mcons key value) (mcdr table)))))
  'ok)

(module+ test
  (require rackunit)

  (test-case "Test for 1 dimensional table"
             (define 1dt (make-table))
             (check-false (lookup 'no-exist-key 1dt))
             (insert! 'name 'Jon 1dt)
             (check-equal? (lookup 'name 1dt) 'Jon)
             )
  )
(provide make-table insert! lookup assoc)
