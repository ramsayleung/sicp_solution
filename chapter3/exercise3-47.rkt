#lang racket

(require "mutex.rkt")

(define (make-semaphore n)
  (let ((count n)
        (mutex (make-mutex)))
    (define (acquire)
      (let loop ()
        (mutex 'acquire)
        (if (> count 0)
            (begin
              (set! count (- count 1))
              (mutex 'release))
            (begin
              (mutex 'release)
              (sleep 0.01)
              (loop)))))

    (define (release)
      (mutex 'acquire)
      (set (count (+ count  1)))
      (mutex 'release))

    (lambda (m)
      (cond ((eq? m 'acquire) acquire)
            ((eq? m 'release) release)
            (else (error "Unknown operation " m))))))

(module+ test
  (require rackunit)

  (test-case "Basic semaphore operations"
             (define sem (make-semaphore 2))

             (check-not-exn (lambda () (sem 'acquire)))
             (check-not-exn (lambda () (sem 'acquire)))

             (check-not-exn (lambda () (sem 'release))))

  (test-case "Threading test"
             (define sem (make-semaphore 1))
             (define results '())
             (define result-mutex (make-mutex))

             (define (add-result x)
               (result-mutex 'acquire)
               (set! results (cons x results))
               (result-mutex 'release))

             (sem 'acquire)

             (thread (lambda ()
                       (sleep 0.05)
                       (add-result 'thread-started)
                       (sem 'acquire)
                       (add-result 'thread-acquired)
                       (sem 'release)))

             (add-result 'main-holding)
             (add-result 'main-releasing)
             (sem 'release)

             ;; Give thread time to complete
             (sleep 0.2)
             (check-equal? (car results) 'thread-acquired)
             (check-equal? (car (cdr results))  'thread-started)
             )
  )
