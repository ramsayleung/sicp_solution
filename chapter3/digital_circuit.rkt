#lang racket

;;; Helper procedure to call all procedures in a list
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      ;; Call the procedure immediately
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch)
  )

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

(module+ test
  (require rackunit)

  (test-case "Test for make-wire"
             (define a (make-wire))
             (define b (make-wire))
             (check-eq? (get-signal a) 0)
             (set-signal! a 1)
             (check-eq? (get-signal a) 1)
             )
  (test-case "Test for add-action!"
             (define w (make-wire))
             (define counter 0)
             (define last-signal #f)

             ; Add an action that increments counter and records the signal
             (add-action! w (lambda ()
                              (set! counter (+ counter 1))
                              (set! last-signal (get-signal w))))

             ; The action should be called immediately when added
             (check-eq? counter 1)
             (check-eq? last-signal 0)

             ; Change the signal - action should be called again
             (set-signal! w 1)
             (check-eq? counter 2)
             (check-eq? last-signal 1)

             ; Change signal back - action should be called again
             (set-signal! w 0)
             (check-eq? counter 3)
             (check-eq? last-signal 0)

             ; Setting same signal should NOT call action
             (set-signal! w 0)
             (check-eq? counter 3)  ; Should remain 3
             (check-eq? last-signal 0)
             )
  )
