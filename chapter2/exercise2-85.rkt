#lang racket
(require "generic_operation.scm")
(require "ch2lib.scm")
(require "exercise2-83.rkt")
(require "section-2-1-1.scm")

(define (install-complex-project)

  (put 'drop '(complex) (lambda (c)
                          (make-real (real-part c))))

  'done)

(define (install-real-project)

  (define (real-project r)
    (let ((scheme-rat (inexact->exact r)))
      (make-rational (numerator scheme-rat) (denominator scheme-rat))))

  (put 'drop '(real) (lambda (c) (real-project c)))
  'done)

(define (install-rational-project)

  (define (rational-project r)
    (make-scheme-number (/ (numer r) (denom r))))

  (put 'drop '(rational) (lambda (c) (rational-project c)))

  'done
  )

(define (drop x) (apply-generic 'drop x))

(define (drop-if-possible x)
  (let ((project-proc (get 'project (list (type-tag x)))))
    (if project-proc
        (let ((dropped (project-proc (contents x))))
          (if (eq? x (raise dropped))
              dropped
              x))
        x)))

(define (apply-generic-v4 op . args)
  (define (raise-to lower higher)
    ;; Raise lower to higher
    ;; Return higher if it's successful, otherwise return #f
    (let ((lower-type (type-tag lower))
          (higher-type (type-tag higher)))
      (cond ((eq? higher-type lower-type)
             lower)
            ((get 'raise (list lower-type))
             (raise-to ((get 'raise (list lower-type)) (contents lower)) higher))
            (else #f)))
    )

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop-if-possible (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((o1 (car args))
                    (o2 (cadr args)))
                (cond ((raise-to o1 o2)
                       (apply-generic-v4 op (raise-to o1 o2) o2))
                      ((raise-to o2 o1)
                       (apply-generic-v4 op o2 (raise-to o1 o2)))
                      (else (error "No raise methods for these types: " (list o1 o2)))))
              (error "No methods for theses types: " type-tag)))))
  )

(define (add x y) (apply-generic-v4 'add x y))

;;; Install package
(install-real-number-package)
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(install-complex-project)
(install-real-project)
(install-rational-project)

;;; Test
;; (drop (raise (make-scheme-number 10))); '(scheme-number . 10)
;; (drop (raise (raise (make-scheme-number 10)))) ; '(rational 10 . 1)
;; (drop (raise (raise (raise (make-scheme-number 10))))) ; '(real . 10)

;; (add (make-scheme-number 10) (make-rational 5 1)); '(scheme-number . 15)
