;;; 这算是个简单的System Design 问题，以便在维持各个分支机构现在独立工作方式的同
;;; 时，又能满足公司总部管理的需要。那就定义一个公司总部的通用的数据结构体，每个
;;; 分支机构都需要实现一个自身独立数据文件到通用结构体的转换函数。
;;; 例如现在有两个分支机构，需要存储的数据为name, address, salary。
;;; 机构1的数据组织方式是(name address salary)； 机构2的数据组织方式是(name
;;; salary addree);  总部定义的数据结构是：(name (address salaray))

;;; a.

#lang racket
(require "generic_operation.scm")
(require "ch2lib.scm")

(define (org1-parse-record record)
  (let ((name (car record))
	(addree (cadr record))
	(salary (caddr record)))
    (list name (list addree salary))))

(define (org2-parse-record record)
  (let ((name (car record))
	(salary (cadr record))
	(addree (caddr record)))
    (list name (list addree salary))))

(define (install-records)
  ;; 存储不同分支机构解析自己独立数据文件的过程
  (put 'record-parser 'org1 org1-parse-record)
  (put 'record-parser 'org2 org2-parse-record)

  ;;; 省去解析文件的步骤
  ;; 不同分支机构存储的数据文件
  (put 'org1 'Dosi '(Earth 1000))
  (put 'org2 'Bob '(2000 Mars))
  )

(install-records)

(define (get-record org-file-name employee-name)
  ((get 'record-parser org-file-name) (append  (list employee-name) (get org-file-name employee-name))))

(get-record 'org1 'Dosi) ;;; => (Dosi (Earth 1000))
(get-record 'org2 'Bob) ;;; => (Bob (Mars 2000))

;;; b.

(define (get-salary-from-comm-record record)
  "从总部定义的数据结构中解析salary"
  (cadr (cadr record)))

(define (get-salary org-file-name employee-name)
  (get-salary-from-comm-record (get-record org-file-name employee-name)))

(get-salary 'org1 'Dosi) ;;; => 1000
(get-salary 'org2 'Bob) ;;; => 2000

;;; c.
(define (contains op type)
  (hash-has-key? *op-table* (list op type)))

(define (find-employee-record org-list employee-name)
  "先获取所有的分支机构，然后遍历存储数据文件，判断employee-name 是否在数据文件内"
  (map (lambda (org-name) (get-record org-name employee-name))
       (filter (lambda (org-name) (contains org-name employee-name)) org-list))
  )

(find-employee-record '(org1 org2) 'Dosi);;; => ((Dosi (Earth 1000)))
(find-employee-record '(org1 org2) 'Dosiss) ;;; => ()

;;; d.
;;; 只需要实现一个parse 过程，将新的人事文件解析成总部通用文件，并安装到 *op-table*
