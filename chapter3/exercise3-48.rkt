#lang racket
(require "mutex.rkt")

;;; 设想 Peter 企图去交换 a1 和 a2，同时 Paul 并发地企图交换 a2 和 a1
;;; 如果对账户编号，并使进程先试图获取编号较小的账户，那么意味着账户的
;;; 顺序是确定的；即 Peter 和 Paul 都会先获取 a1 （假设 a1 编号更小），
;;; 那么只有一个进程会获取成功，那么它就可以继续获取 a2, 从而避免死锁

(define (make-account-and-serializer id balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) id)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serializer-exchange account1 account2)
  (let* ((id1 (account1 'id))
         (id2 (account2 'id))
         (small-account (if (< id1 id2) account1 account2))
         (large-account (if (< id1 id2) account2 account1))
         (s-serializer (small-account 'serializer))
         (l-serializer (large-account 'serializer)))
    ;; always lock smaller Id first, then large Id
    ((s-serializer (l-serializer exchange)) account1 account2)))

(module+ test
  (require rackunit)

  (test-case "Test for no-dead lock version exchange"
             (define account1 (make-account-and-serializer 1 40))
             (define account2 (make-account-and-serializer 2 20))
             (check-equal? (account1 'id) 1)
             (check-equal? (account2 'id) 2)

             (define t1 (thread (lambda ()(serializer-exchange account1 account2))))
             (define t2 (thread (lambda ()(serializer-exchange account2 account1))))

             ;; should avoid deadlock
             (thread-wait t1)
             (thread-wait t2)

             ;; exchange twice means each account has the original balance
             (check-equal? (account1 'balance) 40)
             (check-equal? (account2 'balance) 20)
             )
  )
