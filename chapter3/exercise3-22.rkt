#lang racket
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))

    (define (insert-queue! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (mcdr front-ptr)))))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (mcar front-ptr)))

    (define (print-queue)
      (displayln front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'empty?) empty-queue?)
            ((eq? m 'front) front-queue)
            ((eq? m 'insert!)insert-queue!)
            ((eq? m 'delete!)delete-queue!)
            ((eq? m 'print)print-queue)))
    dispatch))

(module+ test
  (require rackunit)

  (test-case "Test for make-queue success"
             (define q (make-queue))
             (check-true ((q 'empty?)))
             ((q 'insert!) 'a)
             (check-equal? (with-output-to-string (lambda () ((q 'print)))) "{a}\n")
             ((q 'insert!) 'b)
             (check-equal? (with-output-to-string (lambda () ((q 'print)))) "{a b}\n")
             (check-equal? ((q 'front)) 'a)
             ((q 'delete!))
             (check-equal? (with-output-to-string (lambda () ((q 'print)))) "{b}\n")
             (check-equal? ((q 'front)) 'b)
             (check-false ((q 'empty?)))
             )

  (test-case "Test for make-queue unhappy path"
             (define q (make-queue))
             (check-exn (regexp "DELETE! called with an empty queue") (lambda () ((q 'delete!))))
             (check-exn (regexp "FRONT called with an empty queue") (lambda () ((q 'front))))
             )

  )
