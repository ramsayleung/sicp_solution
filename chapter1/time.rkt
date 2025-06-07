#lang racket

(define (runtime)
  (/ (current-process-milliseconds) 1000.0))

(provide runtime)
