#lang racket

(provide (all-defined-out))
(define (stream-null? s)
  (null? s))

(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream a b)
     (cons a (delay b))]))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (force delayed-object)
  (delayed-object))

(define-syntax delay
  (syntax-rules ()
    [(delay expr) (memo-proc (lambda () expr))]))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interal low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interal (+ low 1) high))))

;; Testing utility functions
(define (stream-to-list stream n)
  (if (or (= n 0)
          (stream-null? stream))
      '()
      (cons (stream-car stream)
            (stream-to-list (stream-cdr stream)
                            (- n 1)))))
(define (list-to-stream lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst)
                   (list-to-stream (cdr lst)))))
(module+ test
  (require rackunit)

  (test-case "Test for basic stream operation"
             (define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))

             (check-false (stream-null? s1))
             (check-true (stream-null? the-empty-stream))
             (check-equal? (stream-car s1) 1)
             (check-equal? (stream-car (stream-cdr s1)) 2)
             (check-equal? (stream-car (stream-cdr (stream-cdr s1))) 3)
             (check-true (stream-null? (stream-cdr (stream-cdr (stream-cdr s1)))))
             )

  (test-case "Test for stream-ref"
             (define s1 (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))
             (check-equal? (stream-ref s1 0) 1)
             (check-equal? (stream-ref s1 1) 2)
             (check-equal? (stream-ref s1 2) 3)
             )
  (test-case "Test for lazy-evaluation"
             (define eval-count 0)
             (define (expensive-computation x)
               (set! eval-count (+ eval-count 1))
               (* x x))
             (define lazy-stream
               (cons-stream 1
                            (cons-stream (expensive-computation 2)
                                         (cons-stream (expensive-computation 3)
                                                      the-empty-stream))))

             ;; no computation is called
             (check-equal? eval-count 0)

             ;; access first element, still no computation
             (check-equal? (stream-car lazy-stream) 1)
             (check-equal? eval-count 0)

             ;; access second element, first computation (triggered by
             ;; stream-cdr)
             (check-equal? (stream-car (stream-cdr lazy-stream)) 4)
             (check-equal? eval-count 1)

             ;; access third element, second computation happens
             (check-equal? (stream-car (stream-cdr (stream-cdr lazy-stream))) 9)
             (check-equal? eval-count 2)
             )

  (test-case "Test for memo-proc"
             (define memo-count 0)
             (define (counted-computation)
               (set! memo-count (+ memo-count 1))
               42)

             (define memo-stream (cons-stream 1 (counted-computation)))

             (stream-cdr memo-stream)
             (check-equal? memo-count 1)

             ;; won't eval the stream twice for the same proc
             (stream-cdr memo-stream)
             (check-equal? memo-count 1)
             )

  (test-case "Test for stream-map"
             (define s3 (list-to-stream '(1 2 3 4 5)))
             (define squared (stream-map (lambda (x) (* x x)) s3))
             (check-equal? (stream-to-list squared 5) '(1 4 9 16 25))
             )

  (test-case "Test for stream-filter"
             (define s3 (list-to-stream '(1 2 3 4 5)))
             (define even (stream-filter even? s3))
             (check-equal? (stream-to-list even 5) '(2 4))
             )

  (test-case "Test for stream-for-each"
             (define results '())
             (define s (list-to-stream '(1 2 3 4 5)))
             (define result (stream-for-each
                             (lambda (x)
                               (set! results (cons x results)))
                             s))
             (display-stream s)
             (check-equal? result 'done)
             (check-equal? results '(5 4 3 2 1))
             )

  (test-case "Test for stream-enumerate-interal"
             (define s (stream-enumerate-interal 5 10))
             (check-equal? (stream-to-list s 10) '(5 6 7 8 9 10))
             )
  )
