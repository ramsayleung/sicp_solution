#lang racket

;;; 相当于总是返回第一个函数的参数，并且第二个函数的调用结果为0
;;; (+ (f 0) (f 1)
;;; 因为从左到右运算，第一个函数是 (f 0), 参数是 0, 第二个函数的结果总
;;; 是为0, 即 (+ 0 0)
;;; 从右到左运算，第一个函数是 (f 1), 参数是 1, 第二个函数的结果为0，
;;; 即 (+ 0 1)
(define (make-f)
  (let ((called #f))
    (lambda (param)
      (if called
          0
          (begin (set! called #t)
                 param)))))

(module+ test
  (require rackunit)

  (test-case "Left-to-right evaluation"
             (let ((f (make-f)))
               (check-equal? (+ (f 2) (f 3)) 2)))

  (test-case "Right-to-left evaluation"
             ; 手动模拟从右到左
             (let ((f (make-f)))
               (let* ((right (f 3))
                      (left (f 2)))
                 (check-equal? (+ right left) 3)))))
