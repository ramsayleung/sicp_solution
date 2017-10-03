(define (make-rat n d)
  (cons n d))

(define (number x)
  (car x))
(define (denom y)
  (cdr y))

(define (print-rat x)
  (display (number x))
  (display "/")
  (display (denom x))
  (newline))

(define (add-rat x y)
  (make-rat (+ (* (number x) (denom y))
	       (* (number y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (number x) (denom y))
	       (* (number y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (number x) (number y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (number x) (denom y))
	    (* (denom x) (number y))))

(define (equal-rat? x y)
  (= (* (number x) (denom y))
     (* (number y) (denom x))))
