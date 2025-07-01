#lang racket

(define (make-table same-key?)
  (let ((local-table (mcons '*table* '())))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcar (mcar records)))
             (mcar records))
            (else (assoc key (mcdr records)))))

    ;;; O(N)
    (define (lookup key)
      (let ((record (assoc key (mcdr local-table))))
        (if record
            (mcdr record)
            false)))

    (define (insert! key value)
      (let ((record (assoc key (mcdr local-table))))
        (if record
            (set-mcdr! record value)
            (set-mcdr! local-table (mcons (mcons key value) (mcdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m)))
      )
    dispatch
    ))


(module+ test
  (require rackunit)

  (test-case "Test for basic table operation with equal?"
             (define op-table (make-table equal?))
             (define get (op-table 'lookup-proc))
             (define put (op-table 'insert-proc!))
             (check-false (get 'no-exist-key))
             (put 'name 'White)
             (check-equal? (get 'name) 'White)
             )

  (test-case "Table with numeric tolerance comparison"
             (define (within-tolerance? tol)
               (lambda (x y) (< (abs (- x y)) tol)))

             (define num-table (make-table (within-tolerance? 0.1)))
             (define num-get (num-table 'lookup-proc))
             (define num-put (num-table 'insert-proc!))

             (num-put 1.0 'value-at-1.0)
             (check-equal? (num-get 1.05) 'value-at-1.0)
             (check-false (num-get 1.2)))

  (test-case "Case-insensitive string keys"
             (define (case-insensitive=? a b)
               (and (string? a) (string? b)
                    (string-ci=? a b)))

             (define str-table (make-table case-insensitive=?))
             (define str-get (str-table 'lookup-proc))
             (define str-put (str-table 'insert-proc!))

             (str-put "Hello" 'value-for-hello)
             (check-equal? (str-get "HELLO") 'value-for-hello)
             (check-false (str-get "Goodbye")))
  )
