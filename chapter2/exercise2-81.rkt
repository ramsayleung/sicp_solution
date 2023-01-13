#lang racket
(require "coercion.rkt")
(require "generic_operation.scm")

(define (scheme-number->scheme-number n)n)
(define (complex->complex z) (display "complex->complex") z)

(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)

(put-coercion 'complex 'complex complex->complex)

;; (define (exp x y) (apply-generic-coercion 'exp x y))

;;; following added to Scheme-number package

(define (install-scheme-number-exp)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))
  'done)

(install-scheme-number-exp)

;;; a): 会死循环，一直循环调用apply-generic-coercion
;; (exp (make-complex-from-mag-ang 1 3) (make-complex-from-mag-ang 1 3))

;;; b): 没有纠正同样类型参数的强制问题。
;;; 修改前，如果是同样类型参数，会报错`"No method for these types"`
;;; 修改后，反而引入了Bug，如果对应的数类型，指定的过程没有安装，会出现死循环

;;; c):
(define (apply-generic-v3 op . args)
  (display "apply-generic-v3: op = ")
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
                (if (eq? type1 type2)
                    (error "Can't convert between two same types " (list type1 type2))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic-v3 op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic-v3 op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types" (list op type-tags type1 type2)))))))
              (error "No method for these two arguments types" (list op type-tags)))))))
