#lang racket
;;; 支持 mutable 与 immutable

;;; 时间复杂度 O(N), 空间复杂度 O(N)
(define (memo-has-circle? x)
  (let ((seen '()))
    (define (iter lst)
      (cond
        ((and (not (pair? lst)) (not (mpair? lst))
              #f))
        ((memq lst seen)
         #t)
        (else
         (set! seen (cons lst seen))
         (match lst
           [ (cons a b) (iter b)]
           [ (mcons a b) (iter b)]
           [_ #f])
         )))
    (iter x)))

;;; 双指针方案
;;; 时间复杂度 O(N), 空间复杂度 O(1)
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
             (check-true (memo-has-circle? cycle))
             (check-true (has-circle? cycle)))
  (test-case "Test for circular? with non-circular mutable list"
             (define non-cycle (mcons 1 (mcons 2 null)))
             (check-false (memo-has-circle? non-cycle))
             (check-false (has-circle? non-cycle)))

  (test-case "Test for circular? with immutable list"
             (define imm-list (list 1 2 3))
             (check-false (memo-has-circle? imm-list))
             (check-false (has-circle? imm-list)))
  )
