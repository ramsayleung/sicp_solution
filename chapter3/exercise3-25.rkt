#lang racket

;;; 实现与一维数组一致，差别在于 key 的表达方式，与原来使用 'a 的字符
;;; 变量作为 key 不同，现在使用 '(a b) 列表作为 key
(define (make-table)
  (let ((local-table (mcons '*table* '())))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (mcar (mcar records)))
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
  (define op-table (make-table))
  (define get (op-table 'lookup-proc))
  (define put (op-table 'insert-proc!))

  (test-case "Test for 1-dimentional table"
             (put '(name) 'Alice)
             (check-false (get '(no-exist)))
             (check-equal? (get '(name)) 'Alice)
             (put '(name age) 'Bob)
             (check-equal? (get '(name age)) 'Bob)
             (check-false (get '(age name)))
             (put '(a b c) 'val-abc)
             (check-equal? (get '(a b c)) 'val-abc)
             )
  )
