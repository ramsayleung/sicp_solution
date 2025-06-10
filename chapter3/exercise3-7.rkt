#lang racket
(require "exercise3-3.rkt")
(define (make-joint account old-password new-password)
  (lambda (pwd m)
    (if (eq? pwd new-password)
        (account old-password m)
        (error "Incorrect joint account password"))))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  (define peter-acc (make-account 100 'open-sesame))
  (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

  (define module-test
    (test-suite
     "Tests for make-joint"
     (check-equal? ((peter-acc 'open-sesame 'withdraw) 40) 60 "Test original account with old password")
     (check-equal? ((paul-acc 'rosebud 'withdraw) 40) 20 "Test joint account with new password")
     (check-exn (regexp "Incorrect joint account password")
                (lambda () ((paul-acc 'wrong-password 'deposit) 50)))
     (check-equal? ((peter-acc 'open-sesame 'deposit) 30) 50 "Test original account with old password still works")
     ))

  (run-tests module-test))
