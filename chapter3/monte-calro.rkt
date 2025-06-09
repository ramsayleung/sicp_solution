#lang racket
(require "rand.rkt")
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trial-passed)
    (cond ((= trials-remaining 0)
           (/ trial-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trial-passed 1)))
          (else
           (iter (- trials-remaining 1) trial-passed))))
  (iter trials 0))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)
  (define (estimate-pi-close? trials tolerance)
    (let ((pi-estimate (estimate-pi trials)))
      (< (abs (- pi-estimate 3.14159)) tolerance)))

  (define module-test
    (test-suite
     "Tests for estimate-pi"
     (check-true (estimate-pi-close? 10000 0.1))
     (check-true (estimate-pi-close? 100000 0.01))
     (check-true (estimate-pi-close? 10000000 0.001))
     (check-exn exn:fail? (lambda () (estimate-pi 0)) "Should throw exception for 0 trials")
     ))
  (run-tests module-test))

(provide monte-carlo)
