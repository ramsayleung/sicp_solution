#lang racket
;;; 支持 mutable 与 immutable
;;; 双指针方案
;;; 时间复杂度 O(N), 空间复杂度 O(1)
;;; (这是非常经典的判断链表是否有环的做法，需要一种很聪明的想法原来只
;;; 能算是 Leetcode的 Medium)
(define (has-circle? x)
  (define (safe-cdr lst)
    (cond ((pair? lst)
           (cdr lst))
          ((mpair? lst)
           (mcdr lst))
          (else '())))

  (define (is-pair? lst)
    (or (pair? lst) (mpair? lst)))

  (let loop ((tortoise x)
             (hare (safe-cdr x)))
    (cond ((or (not (is-pair? tortoise))
               (not (is-pair? hare))
               (not (is-pair? (safe-cdr hare))))
           #f)
          ((eq? tortoise hare)
           #t)
          (else
           (loop (safe-cdr tortoise)
                 (safe-cdr (safe-cdr hare))))
          ))
  )

(module+ test
  (require rackunit)
  (test-case "Test for circular? with circular mutable list"
             (define cycle (mcons 1 (mcons 2 null)))
             (set-mcdr! (mcdr cycle) cycle)
             (check-true (has-circle? cycle)))
  (test-case "Test for circular? with non-circular mutable list"
             (define non-cycle (mcons 1 (mcons 2 null)))
             (check-false (has-circle? non-cycle)))

  (test-case "Test for circular? with immutable list"
             (define imm-list (list 1 2 3))
             (check-false (has-circle? imm-list)))
  )
