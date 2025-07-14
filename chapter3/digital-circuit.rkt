#lang racket
(require "after-delay.rkt")

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

(define (valid-signal? s)
  (unless (or (= s 0) (= s 1))
    (error "Invalid signal: expected 0 or 1, got" s))
  )

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (valid-signal? s1)
  (valid-signal? s2)
  (cond  ((and (= s1 1)
               (= s2 1)) 1)
         (else 0)))

(define (logical-or s1 s2)
  (valid-signal? s1)
  (valid-signal? s2)
  (cond ((or (= s1 1)
             (= s2 1)) 1)
        (else 0)))

(define inverter-delay 2)
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay (lambda ()
                                    (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define and-gate-delay 3)
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay (lambda ()
                                    (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define or-gate-delay 5)
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay (lambda ()
                                   (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(provide add-action! get-signal set-signal! make-wire or-gate and-gate inverter)

(module+ test
  (require rackunit)
  (test-case "Test for logical-and"
             (check-eq? (logical-and 1 0) 0)
             (check-eq? (logical-and 0 1) 0)
             (check-eq? (logical-and 0 0) 0)
             (check-eq? (logical-and 1 1) 1)
             (check-exn (regexp "Invalid signal: expected 0 or 1, got 2") (lambda () (logical-and 2 1)))
             )
  (test-case "Test for logical-or"
             (check-eq? (logical-or 1 0) 1)
             (check-eq? (logical-or 0 1) 1)
             (check-eq? (logical-or 0 0) 0)
             (check-eq? (logical-or 1 1) 1)
             (check-exn (regexp "Invalid signal: expected 0 or 1, got 4") (lambda () (logical-and 4 1)))
             )

  (test-case "Test for logical-not"
             (define a (make-wire))
             (check-eq? (logical-not (get-signal a)) 1)
             (set-signal! a 1)
             (check-eq? (logical-not (get-signal a)) 0)
             )

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

  (test-case "Test for inverter gate"
             (define input (make-wire))
             (define output (make-wire))

             (inverter input output)

             (check-eq? (get-signal input) 0)
             (check-eq? (get-signal output) 0)
             (propagate)
             (check-equal? (get-signal output) 1)

             (set-signal! input 1)
             (propagate)
             (check-equal? (get-signal output) 0)

             (set-signal! input 0)
             (propagate)
             (check-equal? (get-signal output) 1))

  (test-case "Test for and-gate"
             (define a1 (make-wire))
             (define a2 (make-wire))
             (define output (make-wire))

             (and-gate a1 a2 output)

             ;; 1 AND 1 = 1
             (set-signal! a1 1)
             (set-signal! a2 1)
             (propagate)
             (check-equal? (get-signal output)1)

             ;; 0 AND 0 = 0
             (set-signal! a1 0)
             (set-signal! a2 0)
             (propagate)
             (check-equal? (get-signal output)0)

             ;; 0 AND 1 = 0
             (set-signal! a1 0)
             (set-signal! a2 1)
             (propagate)
             (check-equal? (get-signal output)0)

             ;; 1 AND 0 = 0
             (set-signal! a1 1)
             (set-signal! a2 0)
             (propagate)
             (check-equal? (get-signal output)0)
             )

  (test-case "Test gate timing and action propagation"
             (define input (make-wire))
             (define output (make-wire))
             (define change-count 0)

             ;; Track output changes
             (add-action! output (lambda ()
                                   (set! change-count (+ change-count 1))))
             ;; Initial action call should have incremented counter
             (check-eq? change-count 1)

             ;; Set up inverter
             (inverter input output)

             (propagate)
             (check-equal? (get-signal output)1)
             (check-eq? change-count 2)
             )

  (test-case "Test compound circuit (NAND gate using AND + NOT)"
             (define a1 (make-wire))
             (define a2 (make-wire))
             (define and-out (make-wire))
             (define nand-out (make-wire))

             (and-gate a1 a2 and-out)
             (inverter and-out nand-out)

             ;; 0 NAND 0 = 1
             (set-signal! a1 0)
             (set-signal! a2 0)
             (propagate)
             (check-equal? (get-signal nand-out) 1)

             ;; 0 NAND 1 = 1
             (set-signal! a1 0)
             (set-signal! a2 1)
             (propagate)
             (check-equal? (get-signal nand-out)1)

             ;; 1 NAND 0 = 1
             (set-signal! a1 1)
             (set-signal! a2 0)
             (propagate)
             (check-equal? (get-signal nand-out)1)

             ;; 1 NAND 1 = 0
             (set-signal! a1 1)
             (set-signal! a2 1)
             (propagate)
             (check-equal? (get-signal nand-out)0))
  )
