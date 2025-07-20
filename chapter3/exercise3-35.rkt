#lang racket
(require "constraints.rkt")

(define (squarer a b)
  (define (process-new-value)
    ;; 原书的实现有缺陷，它假定 b 有值的时候 a 没有值，a 有值的时候 b
    ;; 没有, 但是也存在一种可能是 a 和 b 都没有值, 比如在调用
    ;; =(process-forget-value)= 时
    (cond ((and (has-value? a) (not (has-value? b)))
           (set-value! b (* (get-value a) (get-value a))
                       me))
          ((and (has-value? b) (not (has-value? a)))
           (if (< (get-value b) 0)
               (error "square less than 0 -- SQUARER" (get-value b))
               ;; 还有一种可能就是有 (- (sqrt b)
               (set-value! a (sqrt (get-value b))
                           me)))))

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))

          ((eq? request 'I-lost-my-value)
           (process-forget-value))

          (else
           (error "Unknown request -- MULTIPLIER" request))))

  (connect a me)
  (connect b me)
  me)

(module+ test
  (require rackunit)

  (test-case "Test for squarer with a set"
             (define a (make-connector))
             (define b (make-connector))
             (squarer a b)
             (probe "value a" a)
             (probe "value b" b)
             (check-false (has-value? a))
             (check-false (has-value? b))
             (set-value! a 5 'user)
             (check-equal? (get-value b) 25)

             (forget-value! a 'user)
             (check-false (has-value? a))
             (set-value! a -5 'user)
             (check-equal? (get-value b) 25)
             )

  (test-case "Test for squarer with b set"
             (define a (make-connector))
             (define b (make-connector))
             (squarer a b)
             (probe "value a" a)
             (probe "value b" b)
             (check-false (has-value? a))
             (check-false (has-value? b))
             (set-value! b 25 'user)
             (check-equal? (get-value a) 5)

             (forget-value! b 'user)
             (check-false (has-value? a))
             (set-value! b 16 'user)
             (check-equal? (get-value a) 4)
             )
  )
