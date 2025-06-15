#lang racket

(define (count-pairs x)
  (let ((seen '()))
    (define (helper v)
      (cond ((not (pair? v))
             0)
            ((memq v seen)
             0)
            (else (set! seen (cons v seen))
                  (+ (helper (car v))
                     (helper (cdr v))
                     1))))
    (helper x)))

(module+ test
  (require rackunit)

  (test-case "Test for count-pairs return 3, without shared"
             (define a (list '1 '2 '3))
             (check-equal? (count-pairs a) 3))

  (test-case "Test for count-pairs return 3, with shared cons"
             (define shared (cons 1 2))
             (define b (cons shared (cons shared '())))
             (check-equal? (count-pairs b) 3))

  (test-case "Test for count-pairs return 3, with multiple shared cons"
             (define shared (cons 1 2))
             (define c (cons shared shared))
             (define d (cons c c))
             (check-equal? (count-pairs d) 3)
             )

  (test-case "Test for circular reference - mutable version"
             ;; racket 不支持 Scheme set-cdr! 操作, 只支持针对 mutable 对象的
             ;; set-mcdr!, 但如果要使用 set-mcdr!, 就需要使用相应的 mpair?,
             ;; mcdr, mcar 函数, 那干脆在定义一个 mutable 版本的 count-pairs
             (define (mcount-pairs x)
               (let ((seen '()))
                 (define (helper v)
                   (cond ((not (mpair? v))
                          0)
                         ((memq v seen)
                          0)
                         (else (set! seen (cons v seen))
                               (+ (helper (mcar v))
                                  (helper (mcdr v))
                                  1))))
                 (helper x)))

             (define cycle (mcons 1 (mcons 2 null)))
             (set-mcdr! (mcdr cycle) cycle)
             (check-equal? (mcount-pairs cycle) 2))
  )



