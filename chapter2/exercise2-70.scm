#lang racket
(require "huffman-tree.scm")
(require "exercise2-68.scm")
(require "exercise2-69.scm")

(define rock-tree (generate-huffman-tree '((A 2)
					   (NA 16)
					   (BOOM 1)
					   (SHA 3)
					   (GET 2)
					   (YIP 9)
					   (JOB 2)
					   (WAH 1))))

(define rock-symbol '(GET A JOB))
(define sample-bits (encode rock-symbol rock-tree)); => (1 1 1 1 1 1 1 0 0 1 1 1 1 0)
(decode sample-bits rock-tree); => (GET A JOB)
(encode '(SHA NA NA NA NA NA NA NA NA) rock-tree) ; => (1 1 1 0 0 0 0 0 0 0 0 0)
(encode '(GET A JOB) rock-tree); => (1 1 1 1 1 1 1 0 0 1 1 1 1 0)
(encode '(SHA NA NA NA NA NA NA NA NA) rock-tree) ; => (1 1 1 0 0 0 0 0 0 0 0 0)
(encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) rock-tree); => (1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)
(encode '(SHA BOOM) rock-tree); => (1 1 1 0 1 1 0 1 1)

;;; 这一编码需要5位二进制。使用定长编码，每个编码需要3个二进制位；完成这首歌曲，
;;; 最少需要3 * (2 + 16 + 1 + 3 + 2 + 2 + 9 + 1) = 108 位二进制
