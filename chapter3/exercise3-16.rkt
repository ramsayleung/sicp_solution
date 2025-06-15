#lang racket

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; 无法处理共享或循环结构
(module+ test
  (require rackunit)

  (test-case "Test for count-pairs return 3, without shared"
             (define a (list '1 '2 '3))
             (check-equal? (count-pairs a) 3))

  (test-case "Test for count-pairs return 4, with shared cons"
             (define shared (cons 1 2))
             (define b (cons shared (cons shared '())))
             (check-equal? (count-pairs b) 4))

  (test-case "Test for count-pairs return 7, with multiple shared cons"
             (define shared (cons 1 2))
             (define c (cons shared shared))
             (define d (cons c c))
             (check-equal? (count-pairs d) 7)
             )
  (test-case "Test for not return at all, with circular reference"
             ;; (define cycle (cons 1 (cons 2 '())))
             ;; (set-cdr! (cdr cycle) cycle)
             ;; (count-pairs cycle) ; 无限循环
             ;; racket 不支持 set-cdr!, 如果使用 =set-mcdr!= 可变的数
             ;; 据结构, =cons=, =pair?= 都需要作出相应的修改
             )
  )

