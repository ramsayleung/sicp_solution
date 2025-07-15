#lang racket

(require "digital-circuit.rkt")
(require "after-delay.rkt")

;;; 根据德摩根定律(De Morgan's Laws)
;;; A OR B = NOT((NOT A) AND (NOT B))
(define (demorgan-or-gate a b output)
  (let ((not-a (make-wire))
        (not-b (make-wire))
        (and-output (make-wire)))
    (inverter a not-a)
    (inverter b not-b)
    (and-gate not-a not-b and-output)
    (inverter and-output output))
  'ok)

;;; (inverter a not-a): 需要 1 inverter-delay
;;; (inverter b not-b): 需要 1 inverter-delay
;;; (and-gate not-a not-b and-output): 需要 1 and-gate-delay
;;; (inverter and-output output): 需要 1 inverter-delay
;;; 所以最多需要 3 * inverter-delay + and-gate-delay
;;; 但是因为 (inverter a not-a) + (inverter b not-b) 可以同时执行, 所
;;; 以最少需要 2 * inverter-delay + and-gate-delay
(module+ test
  (require rackunit)

  (test-case "Test for demorgan-or-gate"
             (define a1 (make-wire))
             (define a2 (make-wire))
             (define output (make-wire))

             (demorgan-or-gate a1 a2 output)

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
