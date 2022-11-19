;;; prime
(define (smallest-divisor n)
  (find-divisor n 2))

(define (square n)
  (* n n)) 

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor)n )n)
	((divide? test-divisor n)test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divide? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n)n))

;;; accumulate
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (enumerate-iterval low high)
  (if (> low high)
      '()
      (cons low (enumerate-iterval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

;;; 
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
			    (enumerate-iterval 1 (- i 1))))
	   (enumerate-iterval 1 n)))

;;;
(prime-sum-pairs 6); => ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))
