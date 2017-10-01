(define (square n)
  (* n n))
(define (expmod base exp m)
  (cond ((= exp 0)1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2)m))
		    m))
	(else(remainder (* base (expmod base (- exp 1)m))
			m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n)a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0)true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  )

(define (search-iter current last)
  (if (<= current last) (timed-prime-test current))
  (if (<= current last) (search-iter (+ current 2)last)))

(define (search-for-primes first last)
  (search-iter (if (even? first) (+ first 1)first)
	       (if (even? last) (+ last 1)last)))

;;; 算法速度的比值，大概是每增加一倍长的数字，速度就慢一倍，例如 10000 速度为 x,
;;; 100000000 速度就是 2x. 但是这个如果计算的数值非常大的，例如从 60个数字，100
;;; 个数字，速度就不是之前的线性增长的形式。导致这个原因是数字太大的时候，超出了
;;; 计算机可以储存的 64位之后，变成大数之后，CPU需要耗费额外的资源来计算大数

