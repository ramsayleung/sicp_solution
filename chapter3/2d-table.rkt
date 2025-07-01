#lang racket
(require "1d-table.rkt")

(define (make-table)
  (let ((local-table (mcons '*table* '())))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable (mcons (mcons key-2 value)
                                             (mcdr subtable)))))
            (set-mcdr! local-table (mcons
                                    (mcons key-1
                                           (mcons (mcons key-2 value) '()))
                                    (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown Operation -- TABLE" m))))
    dispatch))


(module+ test
  (require rackunit)

  (test-case "Test for 2d table"
             (define operation-table (make-table))
             (define get (operation-table 'lookup-proc))
             (define put (operation-table 'insert-proc!))
             (check-false (get 'math '+))
             (put 'math '+ 43)
             (check-false (get 'math '*))
             (check-equal? (get 'math '+) 43)
             )
  )
