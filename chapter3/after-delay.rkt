#lang racket
(require compatibility/mlist)
(require "queue.rkt")
(require "sicp-compat.rkt")
;;; segment: (time queue)
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

;;; The agenda is a sorted list of time segments:
;;; agenda: [current-time, (time1, queue1), (time2, queue2), (time3, queue3), ...]
;;; agenda: [0, (5, [action-A]), (10, [action-B, action-C]), (15, [action-D])]
(define (make-agenda) (mlist 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time)
  )

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    ;; Time slot already exists
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        ;; New time slot needed
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))

        (add-to-segments! segments)))
  )

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (when (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))

;;; The after-delay function implements discrete event simulation for
;;; digital circuits.
;;; It schedules actions to execute at future simulation times without
;;; blocking
;;; - after-delay calculates current-time + delay, adds the action to a time-ordered agenda, and returns immediately.
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

;;; The propagate function serves as the event loop, processing
;;; scheduled events chronologically.
;;; Time advances in discrete jumps only when first-agenda-item calls
;;; set-current-time!, jumping directly to the next event time (e.g.,
;;; 0 -> 3 -> 5 -> 8, skipping intermediate times).
;;; This design efficiently models digital circuits where signals change at specific moments, avoiding continuous time checking.
(define (propagate-agenda! agenda)
  (if (empty-agenda? agenda)
      'done
      (let ((first-item (first-agenda-item agenda)))
        (first-item)
        (remove-first-agenda-item! agenda)
        (propagate-agenda! agenda))))

(define (propagate)
  (propagate-agenda! the-agenda))

(provide after-delay propagate)
(module+ test
  (require rackunit)
  (define test-output '())
  (define (reset-test-output!) (set! test-output '()))
  (define (add-test-output! msg) (set! test-output (cons msg test-output)))
  (define (get-test-output) (reverse test-output))

  (test-case "Test for time advances correctly"
             (define test-agenda (make-agenda))
             (check-equal? (current-time test-agenda) 0)

             (add-to-agenda! 3 (λ () 'dummy) test-agenda)
             (add-to-agenda! 8 (λ () 'dummy) test-agenda)
             (add-to-agenda! 5 (λ () 'dummy) test-agenda)

             ;; 0 -> 3
             (first-agenda-item test-agenda)
             (check-equal? (current-time test-agenda) 3)
             (remove-first-agenda-item! test-agenda)

             ;; 3 -> 5
             (first-agenda-item test-agenda)
             (check-equal? (current-time test-agenda) 5)
             (remove-first-agenda-item! test-agenda)

             ;; 5 -> 8
             (first-agenda-item test-agenda)
             (check-equal? (current-time test-agenda) 8)
             (remove-first-agenda-item! test-agenda)
             )

  (test-case "Single event scheduling and execution"
             (reset-test-output!)
             (let ((agenda (make-agenda)))
               (add-to-agenda! 5 (λ () (add-test-output! "Event at 5")) agenda)
               (check-equal? (current-time agenda) 0)
               (check-equal? (empty-agenda? agenda) #f)

               ;; Execute events
               (propagate-agenda! agenda)
               (check-equal? (current-time agenda) 5)
               (check-equal? (get-test-output) (list "Event at 5"))
               (check-equal? (empty-agenda? agenda) #t)
               ))

  (test-case "Multiple events in chronological order"
             (reset-test-output!)
             (let ((agenda (make-agenda)))
               (add-to-agenda! 10 (λ () (add-test-output! "Event at 10")) agenda)
               (add-to-agenda! 3 (λ () (add-test-output! "Event at 3")) agenda)
               (add-to-agenda! 7 (λ () (add-test-output! "Event at 7")) agenda)

               (propagate-agenda! agenda)
               (check-equal? (get-test-output) (list "Event at 3" "Event at 7" "Event at 10"))
               (check-equal? (empty-agenda? agenda) #t)
               ))

  (test-case "Multiple events at same time"
             (reset-test-output!)
             (let ((agenda (make-agenda)))
               ;; Schedule multiple events at same time
               (add-to-agenda! 5 (lambda () (add-test-output! "First at 5")) agenda)
               (add-to-agenda! 5 (lambda () (add-test-output! "Second at 5")) agenda)
               (add-to-agenda! 5 (lambda () (add-test-output! "Third at 5")) agenda)

               (propagate-agenda! agenda)
               (check-equal? (length (get-test-output)) 3)
               (check-equal? (get-test-output) (list "First at 5" "Second at 5" "Third at 5"))
               (check-equal? (empty-agenda? agenda) #t)
               )
             )
  )
