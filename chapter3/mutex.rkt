#lang racket
(define (test-and-set! cell)
  ;; box-cas! is atomic operation
  (not (box-cas! cell #f #t)))

(define (make-mutex)
  (let ((cell (box #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (when (test-and-set! cell)
               ;; retry
               (the-mutex 'acquire)))
            ((eq? m 'release)
             (clear! cell))))
    the-mutex))

(define (clear! cell)
  (box-cas! cell #t #f))

(provide make-mutex)
