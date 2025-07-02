#lang racket
(require compatibility/mlist)

(define (make-bst-table [key-compare <] [key-equal equal?])
  (let ((root '()))
    ;; the format of a node, from left to right:
    ;; key, value, left-tree, right-tree
    (define (make-node key value)
      (mlist key value '() '()))

    (define (node-key node)
      (mcar node))

    (define (node-value node)
      (mcar (mcdr node)))

    (define (left-tree node)
      (mcar (mcdr (mcdr node))))

    (define (right-tree node)
      (mcar (mcdr (mcdr (mcdr node)))))

    (define (set-left-tree! node left)
      (set-mcar! (mcdr (mcdr node)) left))

    (define (set-right-tree! node right)
      (set-mcar! (mcdr (mcdr (mcdr node))) right))

    (define (lookup given-key)
      (define (iter node)
        (cond ((null? node) #f)
              ((key-equal given-key (node-key node))
               (node-value node))
              ((key-compare given-key (node-key node))
               (iter (left-tree node)))
              (else
               (iter (right-tree node)))))
      (iter root))

    (define (insert! key value)
      (define (iter node)
        (cond ((null? node)
               (make-node key value))
              ((key-equal key (node-key node))
               (set-mcar! (mcdr node) value)
               node)
              ((key-compare key (node-key node))
               (set-left-tree! node (iter (left-tree node)))
               node)
              (else
               (set-right-tree! node (iter (right-tree node)))
               node)))
      (set! root (iter root))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- BST-table" m))))
    dispatch))

(module+ test
  (require rackunit)
  (test-case "Test for bst-table of number key"
             (define bst-t (make-bst-table))
             (define get (bst-t 'lookup-proc))
             (define put (bst-t 'insert-proc!))
             (check-false (get 10086))
             (put 5 "five")
             (check-equal? (get 5) "five")
             (put 5 "5")
             (check-equal? (get 5) "5")
             )
  (test-case "Test for bst-table of symbol key"
             (define bst-t (make-bst-table symbol<? equal?))
             (define get (bst-t 'lookup-proc))
             (define put (bst-t 'insert-proc!))
             (check-false (get 'no-exist-key))
             (put 'age 18)
             (check-equal? (get 'age) 18)
             (put 'age '20)
             (check-equal? (get 'age) '20)
             )

  (test-case "Test for bst-table of string key"
             (define bst-t (make-bst-table string<? equal?))
             (define get (bst-t 'lookup-proc))
             (define put (bst-t 'insert-proc!))
             (check-false (get "no-exist-key"))
             (put "age" 18)
             (check-equal? (get "age") 18)
             (put "age" 20)
             (check-equal? (get "age") 20)
             )
  )
