(define (standardized-make-rat n d)
  (let ((g (if (negative? d)- +) (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (number x)
  (car x))
(define (denom y)
  (cdr y))
(define (print-rat x)
  (display (number x))
  (display "/")
  (display (denom x))
  (newline))

(define negative-one-half
  (standardized-make-rat 1 -2))
(define positive-one-half
  (standardized-make-rat -1 -2))
