#lang racket
(require "digital-circuit.rkt")
(require "sicp-compat.rkt")
(require "adder.rkt")
(require "after-delay.rkt")

;;; 延迟分析
;;; 全加器延迟是 2 and-gate-delay + 1 or-gate delay
;;; n位的级联进位加法器就是 n * (2 * and-gate-delay + or-gate-delay)
(define (ripple-carry-adder a-list b-list s-list c-out)
  (define (iter a-wires b-wires s-wires c-in)
    (if (null? a-wires)
        'done
        (let ((a (car a-wires))
              (b (car b-wires))
              (s (car s-wires))
              (next-carry (if (null? (cdr a-wires))
                              c-out
                              (make-wire))))
          (full-adder a b c-in s next-carry)
          (iter (cdr a-wires) (cdr b-wires) (cdr s-wires) next-carry))))
  (let ((c-in (make-wire)))
    (set-signal! c-in 0)
    (iter a-list b-list s-list c-in)))

(module+ test
  (require rackunit)

  (define (make-wire-list n)
    (if (= n 0)
        '()
        (cons (make-wire) (make-wire-list (- n 1)))))

  (define (get-wire-list-signal wires)
    (if (null? wires)
        '()
        (cons (get-signal (car wires))
              (get-wire-list-signal (cdr wires)))))

  (define (set-wire-signals! wires values)
    (if (or (null? wires) (null? values))
        'done
        (begin
          (set-signal! (car wires) (car values))
          (set-wire-signals! (cdr wires) (cdr values)))))

  (test-case "Test for ripple-carry-adder, 5 + 3 = 8"
             (let ((a-wires (make-wire-list 4))
                   (b-wires (make-wire-list 4))
                   (s-wires (make-wire-list 4))
                   (c-wire (make-wire)))
               (ripple-carry-adder a-wires b-wires s-wires c-wire)

               ;; Using little-endian bit order (LSB first)
               ;; 5 = 0101 in binary -> (1 0 1 0) in little-endian
               ;; 3 = 0011 in binary -> (1 1 0 0) in little-endian
               ;; 8 = 1000 in binary -> (0 0 0 1) in little-endian
               (set-wire-signals! a-wires (list 1 0 1 0))
               (set-wire-signals! b-wires (list 1 1 0 0))
               (propagate)
               (check-equal? (get-wire-list-signal s-wires) (list 0 0 0 1))
               (check-equal? (get-signal c-wire) 0)
               ))

  (test-case "Test for ripple-carry-adder, 9 + 10 = 19"
             (let ((a-wires (make-wire-list 4))
                   (b-wires (make-wire-list 4))
                   (s-wires (make-wire-list 4))
                   (c-wire (make-wire)))
               (ripple-carry-adder a-wires b-wires s-wires c-wire)

               ;; Using little-endian bit order (LSB first)
               ;; 9  = 1001 in binary -> (1 0 0 1) in little-endian
               ;; 10 = 1010 in binary -> (0 1 0 1) in little-endian
               ;; 19 = 10011 in binary -> (1 1 0 0 1) in little-endian
               ;; 19 = 16 + 3 = 0b10000 + 0b0011
               (set-wire-signals! a-wires (list 1 0 0 1))
               (set-wire-signals! b-wires (list 0 1 0 1))
               (propagate)
               ;; 1 1 0 0 in little-endian
               (check-equal? (get-wire-list-signal s-wires) (list 1 1 0 0))
               ;; 0b10000
               (check-equal? (get-signal c-wire) 1)
               ))
  )
