
(define (prime? n)
  (define (next next-divisor)
    (cond ((= next-divisor 2)3)
	  (else (+ next-divisor 2))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (square n)
    (* n n)) 
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor)n )n)
	  ((divide? test-divisor n)test-divisor)
	  (else (find-divisor n (next test-divisor)))))
  (define (divide? a b)
    (= (remainder b a) 0))
  (= (smallest-divisor n)n))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  )

;;; 两个算法速度的比值不是 2:1, 大概是3:2
