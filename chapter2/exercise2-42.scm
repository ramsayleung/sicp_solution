#lang racket
;;; 棋盘由若干个position 组成，而position 由row 与col 组成
(define (make-position row col)
  (cons row col))

(define (position-row pos)
  (car pos))

(define (position-col pos)
  (cdr pos))

(define (same-position? p1 p2)
  (and (= (position-col p1) (position-col p2))
       (= (position-row p1) (position-row p2))))

(define empty-board '())

(define (adjoin-position row col positions)
  (append positions (list (make-position row col))))

(define (safe? k positions)
  "在第k列的queen 是否不会被第1..k-1列的queen 攻击"
  (define (attack? p1 p2)
    "判断p1位置的queen能否攻击p2 位置的queen"
    (let ((col1 (position-col p1))
          (row1 (position-row p1))
          (col2 (position-col p2))
          (row2 (position-row p2)))
      (or (= col1 col2) ; 同一列攻击
          (= row1 row2) ; 同一行攻击
          (= (abs (- col1 col2)) ;对角线攻击
             (abs (- row1 row2))))))
  ;; 遍历kth-pos 与1..k-1列，如果可攻击列表为空，则第k 列的queen 是安全的
  (let ((kth-pos (list-ref positions (- k 1))))
    (null? (filter (lambda (pos)
                     (and (not (same-position? pos kth-pos))
                          (attack? pos kth-pos)))
                   positions))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-iterval low high)
  (if (> low high)
      '()
      (cons low (enumerate-iterval (+ low 1) high))))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap
                 (lambda (rest-of-queens)
                   (map (lambda (new-row)
                          (adjoin-position new-row k rest-of-queens))
                        (enumerate-iterval 1 board-size)))
                 (queen-cols (- k 1))))))
  (queen-cols board-size))
;;;

(queens 4);=> (((2 . 1) (4 . 2) (1 . 3) (3 . 4)) ((3 . 1) (1 . 2) (4 . 3) (2 . 4)))
