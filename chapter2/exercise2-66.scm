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
        ((= given-key (key  set-of-records))
         (entry set-of-records))
        ((< given-key (key set-of-records))
         ;; 查找左子树
         (look-up given-key (left-branch set-of-records)))
        (else
         ;; 查找右子树
         (look-up given-key (right-branch set-of-records)))))

(module+ test
  (require rackunit)
  ;;       5
  ;;     3   8
  ;;   1  4 6  9
  (define tree
    '(5 "data5"
        (3 "data3"
           (1 "data1" () ())
           (4 "data4" () ()))
        (8 "data8"
           (6 "data6" () ())
           (9 "data9" () ()))))

  (test-case "Test for look-up existing key"
             (check-equal? (look-up 3 tree) "data3")
             (check-equal? (entry '(6 "data6" () ())) "data6")
             )
  (test-case "Test for non-exist key"
             (check-false (look-up 100 tree))
             )

  (test-case "Test for edge case"
             (define empty-tree '())
             (check-false (look-up 5 empty-tree))
             (define root '(5 "root" () ()))
             (check-equal? (look-up 5 root) "root" )
             )
  )
