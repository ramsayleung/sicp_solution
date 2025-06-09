#lang racket
(require "rand.rkt")

(define random-init 42)
(define rand
  (let ((x random-init))
    (lambda (param)
      (cond ((eq? param 'generate)
             (set! x (rand-update x))
             x)
            ((eq? param 'reset)
             (lambda (new-value)
               (set! x new-value)))
            (else (error "Unknown operation -- RAND" param))))))

(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  (define module-test
    (test-suite
     "Tests for resetable random generater"
     (let ((num1 (rand 'generate))
           (num2 (rand 'generate)))
       (check-false (= num1 num2)
                    "Sequential calls should produce different numbers"))

     "Reset functionality"
     (let ((reset-proc (rand 'reset)))
       ;; Generate first sequence
       (reset-proc 42)
       (let ((seq1 (list (rand 'generate) (rand 'generate) (rand 'generate))))

         ;; Reset and generate second sequence
         (reset-proc 42)
         (let ((seq2 (list (rand 'generate) (rand 'generate) (rand 'generate))))
           (check-equal? seq1 seq2 "Sequences should be identical after same reset"))))
     ))

  (run-tests module-test))
