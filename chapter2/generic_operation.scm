#lang racket

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error "No method for these types -- APPLY-GENERIC"
		 (list op type-tags))))))

(define (square x) (* x x))

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref! *op-table* (list op type) #f))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
;;; 

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;; regular number

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))

  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number (lambda (x) (tag x)))

  'done
  )

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;;; rational number
(define (install-rational-package)
  ;; internal procedures
  (define (make-rat n d)
    (cons n d))

  (define (numer x)
    (car x))
  (define (denom y)
    (cdr y))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))

  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))

  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))

  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))

  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))

  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))

  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done
  )

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;; complex number
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))

  (define (imag-part z) (cdr z))

  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)

  (put 'imag-part '(rectangular) imag-part)

  (put 'magnitude '(rectangular) magnitude)

  (put 'angle '(rectangular) angle)

  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))

  'done
  ) 

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z ))

  (define (angle z) (cdr z))

  (define (make-from-mag-ang r a) (cons r a))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))

  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))

  ;; interface to the rest of the system
  (define (tag x)(attach-tag 'polar x))

  (put 'real-part '(polar) real-part)

  (put 'imag-part '(polar) imag-part)

  (put 'magnitude '(polar) magnitude)

  (put 'angle '(polar) angle)

  (put 'make-from-real-imag 'polar (lambda (x y)(tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'polar (lambda (r a)(tag (make-from-mag-ang r a))))

  'done
  )

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z)     (apply-generic 'angle z))

(define (install-complex-package)
  ;; internal procedure
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular)x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))

  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))

  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))

  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)

  (put 'imag-part '(complex) imag-part)

  (put 'magnitude '(complex) magnitude)

  (put 'angle '(complex) angle)

  'done
  )

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex)x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex)r a))

;;; test case
(install-scheme-number-package)
(install-complex-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package)


(add (make-scheme-number 10) (make-scheme-number 5)) ;=> (scheme-number . 15)
(sub (make-scheme-number 10) (make-scheme-number 5)) ;=> (scheme-number . 5)
(mul (make-scheme-number 10) (make-scheme-number 5)) ;=> (scheme-number . 50)
(div (make-scheme-number 10) (make-scheme-number 5)) ;=> (scheme-number . 2)

(add (make-rational 1 2) (make-rational 1 3)) ;=> (rational 5 . 6)
(sub (make-rational 1 2) (make-rational 1 3)) ;=> (rational 1 . 6)
(mul (make-rational 1 2) (make-rational 1 3)) ;=> (rational 1 . 6)
(div (make-rational 1 2) (make-rational 1 3)) ;=> (rational 3 . 2)

(add (make-complex-from-real-imag 3 4) (make-complex-from-mag-ang 10 1)) ;=> (complex rectangular -3.921861725181672 . -4.540814971847569)
(sub (make-complex-from-real-imag 3 4) (make-complex-from-mag-ang 10 1)) ;=> (complex rectangular 0.0 . 0.0)
(mul (make-complex-from-real-imag 3 4) (make-complex-from-mag-ang 10 1)) ;=> (complex polar 9 . 8)
(div (make-complex-from-real-imag 3 4) (make-complex-from-mag-ang 10 1)) ;=> (complex polar 1 . 0)

(provide install-scheme-number-package install-complex-package install-rational-package install-rectangular-package install-polar-package make-scheme-number make-rational make-complex-from-mag-ang make-complex-from-real-imag)
(provide attach-tag type-tag contents get put apply-generic)
(provide magnitude angle real-part imag-part)
(provide add sub mul div)
