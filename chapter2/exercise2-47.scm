(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (car (cdr (cdr frame))))

(define (make-frame-v2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-v2 frame)
  (car frame))

(define (edge1-frame-v2 frame)
  (cadr frame))

(define (edge2-frame-v2 frame)
  (cdr (cdr frame)))
