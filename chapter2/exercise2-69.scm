(load "huffman-tree.scm")
(load "exercise2-68.scm")

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge 
       (adjoin-set (make-code-tree (car leaf-set)
				   (cadr leaf-set))
		   (cddr leaf-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define sample-tree (generate-huffman-tree '((A 4)
					     (B 2)
					     (C 1)
					     (D 1))))

(define sample-symbol '(A D A B B C A))
(define sample-bits (encode sample-symbol sample-tree)); => (0 1 1 0 0 1 0 1 0 1 1 1 0)
(decode sample-bits sample-tree); => (A D A B B C A)
