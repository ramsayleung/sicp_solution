#lang racket

;; 线性同余生成器（Linear Congruential Generator, LCG）
;; latex 公式: X_{n+1} = (a \cdot X_n + c) \pmod{m}
;; https://en.wikipedia.org/wiki/Linear_congruential_generator
;; 使用 C++11 minstd_rand 实现的参数
(define (rand-update x)
  (let ((a 48271)
        (c 0)
        (m (- (expt 2 31) 1)))
    (modulo (+ (* a x) c) m)))

(define random-init 42)
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(provide random-init rand-update)

(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  (define module-test
    (test-suite
     "Tests for random-in-range"
     (check-true (andmap (lambda (_) (let ((a (random-in-range 1 10)))
                                       (and (<= a 10)
                                            (>= a 1))))
                         (range 3000)))
     ))

  (run-tests module-test))

(provide rand random-in-range rand-update)
