#lang racket
(require "queue.rkt")

(define (print-queue queue)
  (displayln (front-ptr queue)))

(module+ test
  (require rackunit)

  (test-case "Test for empty queue"
             (define q (make-queue))
             (check-equal? (with-output-to-string (lambda () (print-queue q))) "()\n"))

  (test-case "Test for queue with 2 elements"
             (define q (make-queue))
             (insert-queue! q 'a)
             (insert-queue! q 'b)
             (check-equal? (with-output-to-string (lambda () (print-queue q))) "{a b}\n"))

  (test-case "Test for queue after deleting elements"
             (define q (make-queue))
             (insert-queue! q 'a)
             (insert-queue! q 'b)
             (check-equal? (with-output-to-string (lambda () (print-queue q))) "{a b}\n")
             (delete-queue! q)
             (check-equal? (with-output-to-string (lambda () (print-queue q))) "{b}\n")
             )
  )
