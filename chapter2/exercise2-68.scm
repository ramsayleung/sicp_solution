(load "exercise2-67.scm")

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (element-of-set? x set)
  (not (not (memq x set))))

(define (encode-symbol symbol tree)
  "根据给定的树产生给定symbol 的二进制表"
  (cond ((null? tree) (error "bad tree -- ENCODE-SYMBOL" tree))
	((leaf? tree) '())
	(else
	 (let ((left-symbols (symbols (left-branch tree)))
	       (right-symbols (symbols (right-branch tree))))
	   (cond ((element-of-set? symbol left-symbols)
		  (cons 0 (encode-symbol symbol (left-branch tree))))
		 ((element-of-set? symbol right-symbols)
		  (cons 1 (encode-symbol symbol (right-branch tree))))
		 (else (error "Symbol not in the tree" tree)))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define sample-message '(A D A B B C A))
(encode sample-message sample-tree)	; => (0 1 1 0 0 1 0 1 0 1 1 1 0)
;;; A - 0  B 10 C 111 D 110

