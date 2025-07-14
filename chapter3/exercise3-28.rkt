#lang racket

(require "digital-circuit.rkt")
(require "after-delay.rkt")
;;; The implementation of `or-gate` is in `digital-circuit.rkt`
(module+ test
  (require rackunit)


  (test-case "Test for or-gate"
             (define a1 (make-wire))
             (define a2 (make-wire))
             (define output (make-wire))

             (or-gate a1 a2 output)

             ;; 0 OR 0 = 0
             (set-signal! a1 0)
             (set-signal! a2 0)
             (propagate)
             (check-equal? (get-signal output)0)

             ;; 0 OR 1 = 1
             (set-signal! a1 0)
             (set-signal! a2 1)
             (propagate)
             (check-equal? (get-signal output)1)

             ;; 1 OR 0 = 1
             (set-signal! a1 1)
             (set-signal! a2 0)
             (propagate)
             (check-equal? (get-signal output) 1)

             ;; 1 OR 1 = 1
             (set-signal! a1 1)
             (set-signal! a2 1)
             (propagate)
             (check-equal? (get-signal output )1))

  )
