#lang racket
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch pwd m)
    (if (eq? pwd password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request --MAKE_ACCOUNT" m)))
        (error "Incorrect password")
        ))
  dispatch)

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  (define acc (make-account 100 'secret-password))

  (define module-test
    (test-suite
     "Tests for make-account with password"
     (check-equal? ((acc 'secret-password 'withdraw) 40) 60)
     (check-equal? ((acc 'secret-password 'withdraw) 40) 20)
     ;; Test that an exception is thrown with the correct message
     (check-exn exn:fail? (lambda () ((acc 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc 'invalid-password 'deposit) 40)))
     ))

  (run-tests module-test))

(provide make-account)
