#lang racket
(require sicp-pict)

(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-up
           (transform-painter painter1
                              (make-vect 0 0)
                              (make-vect 1 0)
                              split-point
                              
                              ))
          (paint-down
           (transform-painter painter2
                              split-point
                              (make-vect 1 0.5)
                              (make-vect 0 1))))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))

(define (below-v2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
