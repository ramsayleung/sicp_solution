(define(largest-two-sum x y z)
  (if (= x (larger x y))
      (sum x (larger y z))
      (sum y (larger z x))))

(define (larger x y)
  (if (< x y)y x))

(define (sum x y)
  (+ x y))
