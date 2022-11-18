(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (accumuate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
	    (accumuate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

;;;
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (v1) (dot-product v1 v)) m))

(define (transpose mat)
  (accumuate-n cons (list) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;;; [1 -1 2]
;;; [0 -3 1]
(define m (list (list 1 -1 2) (list 0 -3 1)))
(define x (list 2 1 0))
(matrix-*-vector m x); => [1 -3]

;;; [1   0]
;;; [-1 -3]
;;; [2   1]
(transpose m)

;;; [0  4  -2]
;;; [-4 -3  0]
(define a (list (list 0 4 -2) (list -4 -3 0)))
;;; [0  1]
;;; [1 -1]
;;; [2  3]
(define b (list (list 0 1) (list 1 -1) (list 2 3)))

;;; [ 0  -10]
;;; [-3   -1]
(matrix-*-matrix a b)


