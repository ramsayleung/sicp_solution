#lang racket
(require "generic_operation.scm")

(define *coercion-table* (make-hash))

(define (put-coercion type1 type2 proc)
  (hash-set! *coercion-table* (list type1 type2) proc))

(define (get-coercion type1 type2)
  (hash-ref! *coercion-table* (list type1 type2) #f))

(define (apply-generic-coercion op . args)
  (display "apply-generic-coercion: op = ")
  (display op)
  (display ", args =")
  (display args)
  (newline)

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic-coercion op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic-coercion op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types" (list op type-tags type1 type2))))))
              (error "No method for these two arguments types" (list op type-tags)))))))

(provide (all-defined-out))
