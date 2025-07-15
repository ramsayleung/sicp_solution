#lang racket
(require "digital-circuit.rkt")
(require "after-delay.rkt")

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s))
  'ok)

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out))
  'ok)

(provide half-adder full-adder)

(module+ test
  (require rackunit)

  (test-case "Test for half-adder"
             (define a (make-wire))
             (define b (make-wire))
             (define s (make-wire))
             (define c (make-wire))
             ;; A	B	Sum (S)	Carry (C)
             ;; 0	0	  0	    0
             ;; 0	1	  1	    0
             ;; 1	0	  1	    0
             ;; 1	1	  0	    1
             (half-adder a b s c)
             (propagate)
             (check-equal? (get-signal s) 0)
             (check-equal? (get-signal c) 0)

             (set-signal! b 1)
             (propagate)
             (check-equal? (get-signal s) 1)
             (check-equal? (get-signal c) 0)

             (set-signal! b 0)
             (set-signal! a 1)
             (propagate)
             (check-equal? (get-signal s) 1)
             (check-equal? (get-signal c) 0)

             (set-signal! b 1)
             (propagate)
             (check-equal? (get-signal s) 0)
             (check-equal? (get-signal c) 1)
             )

  (test-case "Test-for full-adder"
             ;; A	B	Cin S	Cout
             ;; 0	0	0	0	0
             ;; 0	0	1	1	0
             ;; 0	1	0	1	0
             ;; 0	1	1	0	1
             ;; 1	0	0	1	0
             ;; 1	0	1	0	1
             ;; 1	1	0	0	1
             ;; 1	1	1	1	1
             (define a (make-wire))
             (define b (make-wire))
             (define c-in (make-wire))
             (define s (make-wire))
             (define c-out (make-wire))
             (full-adder a b c-in s c-out)

             (propagate)
             (check-equal? (get-signal s) 0)
             (check-equal? (get-signal c-out) 0)

             (set-signal! b 1)
             (set-signal! c-in 1)
             (propagate)
             (check-equal? (get-signal s) 0)
             (check-equal? (get-signal c-out) 1)

             (set-signal! a 1)
             (propagate)
             (check-equal? (get-signal s) 1)
             (check-equal? (get-signal c-out) 1)
             )
  )
