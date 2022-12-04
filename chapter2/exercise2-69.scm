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
