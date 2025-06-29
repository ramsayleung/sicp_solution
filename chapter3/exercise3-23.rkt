#lang racket

(define (make-deque)
  (mcons '() '()))
;;; 比对单链表的队列，为了实现可以在两端进行插入和删除操作，势必需要
;;; 两个指针，一个指向前节点，一个指向后节点
(define (make-node value prev next)
  (mcons value (mcons prev next)))

(define (node-value node) (mcar node))
(define (node-prev node) (mcar (mcdr node)))
(define (node-next node) (mcdr (mcdr node)))
(define (set-node-prev! node prev) (set-mcar! (mcdr node) prev))
(define (set-node-next! node next) (set-mcdr! (mcdr node) next))

;;; 选择函数
(define (empty-deque? deque)
  (null? (mcar deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque")
      (node-value (mcar deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque")
      (node-value (mcdr deque))))

;;; 插入操作
(define (front-insert-deque! deque value)
  (let ((new-node (make-node value '() (mcar deque))))
    (cond ((empty-deque? deque)
           (set-mcar! deque new-node)
           (set-mcdr! deque new-node))
          (else
           (set-node-prev! (mcar deque) new-node)
           (set-mcar! deque new-node))))
  deque)

(define (rear-insert-deque! deque value)
  (let ((new-node (make-node value (mcdr deque) '())))
    (cond ((empty-deque? deque)
           (set-mcar! deque new-node)
           (set-mcdr! deque new-node))
          (else
           (set-node-next! (mcdr deque) new-node)
           (set-mcdr! deque new-node))))
  deque)

;;; 删除操作
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque"))
        ;; 只有一个元素
        ((null? (node-next (mcar deque)))
         (set-mcar! deque '())
         (set-mcdr! deque '()))
        (else
         (set-mcar! deque (node-next (mcar deque)))
         (set-node-prev! (mcar deque) '())))
  deque)

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque"))
        ((null? (node-prev (mcdr deque)))
         (set-mcar! deque '())
         (set-mcdr! deque '())
         )
        (else
         (set-mcdr! deque (node-prev (mcdr deque)))
         (set-node-next! (mcdr deque) '())))
  deque)

(define (print-deque deque)
  (define (iter node)
    (if (null? node)
        '()
        (mcons (node-value node) (iter (node-next node)))))
  (iter (mcar deque)))

(module+ test
  (require rackunit)

  (test-case "Test for deque"
             (define dq (make-deque))
             (check-true (empty-deque? dq))

             (front-insert-deque! dq 'a)
             (check-equal? (front-deque dq) 'a)
             (check-equal? (rear-deque dq) 'a)

             (front-insert-deque! dq 'b) ;; b -> a
             (check-equal? (front-deque dq) 'b)
             (check-equal? (rear-deque dq) 'a)

             (rear-insert-deque! dq 'c) ;; b -> a -> c
             (check-equal? (front-deque dq) 'b)
             (check-equal? (rear-deque dq) 'c)

             (front-delete-deque! dq) ;; a -> c
             (check-equal? (front-deque dq) 'a)
             (check-equal? (rear-deque dq) 'c)

             (rear-delete-deque! dq) ;; a
             (check-equal? (front-deque dq) 'a)
             (check-equal? (rear-deque dq) 'a)
             (check-false (empty-deque? dq))

             (rear-delete-deque! dq)
             (check-true (empty-deque? dq))
             )
  )
