#lang racket
(define (make-account balance password)
  (let ((failed-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (call-the-cops)
      (error "Calling 911"))

    (define (dispatch pwd m)
      (if (eq? pwd password)
          ;; reset the failed count once the password is correct
          (begin (set! failed-count 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request --MAKE_ACCOUNT" m))))
          (begin (set! failed-count (+ failed-count 1))
                 (if (> failed-count 7)
                     (call-the-cops)
                     (error "Incorrect password")))
          ))
    dispatch))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  (define acc (make-account 100 'secret-password))
  (define acc2 (make-account 100 'secret-password))
  (define acc3 (make-account 100 'secret-password))

  (define module-test
    (test-suite
     "Tests for make-account with password and limited attempt"
     (check-equal? ((acc 'secret-password 'withdraw) 40) 60)
     (check-equal? ((acc 'secret-password 'withdraw) 40) 20)
     ;; Test that an exception is thrown with the correct message
     (check-exn exn:fail? (lambda () ((acc 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc 'invalid-password 'deposit) 40)))

     "Tests for make-account, 7 consecutive attmepts failed, call cops eventually"
     (check-exn (regexp "Incorrect password") (lambda () ((acc2 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc2 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc2 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc2 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc2 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc2 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc2 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Calling 911") (lambda () ((acc2 'invalid-password 'deposit) 40)))

     "Tests for make-account, 1 attempt succeeded, but subsequence attempt failed, call cops eventually"
     (check-exn (regexp "Incorrect password") (lambda () ((acc3 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc3 'invalid-password 'deposit) 40)))
     ;; successful attempt reset the failed count
     (check-equal? ((acc3 'secret-password 'withdraw) 40) 60)
     (check-exn (regexp "Incorrect password") (lambda () ((acc3 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc3 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc3 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc3 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc3 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc3 'invalid-password 'deposit) 40)))
     (check-exn (regexp "Incorrect password") (lambda () ((acc3 'invalid-password 'deposit) 40)))
     ;; call-the-cops after consecutive 7 failed attempts
     (check-exn (regexp "Calling 911") (lambda () ((acc3 'invalid-password 'deposit) 40)))
     )
    )

  (run-tests module-test))
