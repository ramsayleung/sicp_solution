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
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumuate-n + 0 s); => (22 26 30)
