#lang racket

(define (make-monitored func)
  (let ((count 0))
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) count)
            ((eq? input 'reset-count) (set! count 0) 'reset)
            (else
             (set! count (+ count 1))
             (func input))))))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  (define s (make-monitored sqrt))
  (define module-test
    (test-suite
     "Tests for make-monitored"
     (check-equal? (s 100) 10)
     (check-equal? (s 100) 10)
     (check-equal? (s 'how-many-calls?) 2)
     (s 'reset-count)
     (check-equal? (s 'how-many-calls?) 0)
     ))

  (run-tests module-test))
