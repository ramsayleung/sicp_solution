#lang racket
(require "stream.rkt")
(require "rand.rkt")

(define (random-stream requests seed)
  (if (stream-null? requests)
      the-empty-stream
      (let ((request (stream-car requests))
            (rest-requests (stream-cdr requests)))

        (cond ((eq? 'generate request)
               (let ((next-random (rand-update seed)))
                 (cons-stream next-random
                              (random-stream rest-requests next-random))))

              ((and (pair? request) (eq? 'reset (car request)))
               (let ((new-seed (cadr request)))
                 (random-stream rest-requests new-seed)))

              (else
               (random-stream rest-requests seed))))))

(module+ test
  (require rackunit)

  (test-case "Test for random-stream"
             (define requests (list-to-stream '(generate
                                                generate
                                                generate
                                                (reset 42)
                                                generate
                                                generate
                                                generate
                                                generate)))
             (define random-numbers (random-stream requests 42))
             (check-equal? (stream-take-n random-numbers 8) '(2027382 1226992407 551494037 2027382 1226992407 551494037 961371815))
             )
  )
