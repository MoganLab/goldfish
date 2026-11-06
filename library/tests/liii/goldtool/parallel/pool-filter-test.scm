;; pool-filter
;; 以 worker 池并发执行过滤判断，返回使判断结果为真的文件列表，
;; 结果顺序与输入顺序一致。
;;
;; 语法
;; ----
;; (pool-filter pred files)
;;
;; 参数
;; ----
;; pred : procedure?
;; 判断过程，接收文件路径，返回布尔值。
;; files : list
;; 待过滤的文件列表。
;;
;; 返回值
;; ------
;; list
;; 满足判断条件的文件列表；files 为空时返回空列表。

(import (scheme base) (liii check) (liii list) (liii goldtool parallel))

(check-set-mode! 'report-failed)

(define original-jobs (goldtool-jobs))

;; 串行执行：只保留判断为真的文件
(set-goldtool-jobs! 1)
(check
  (pool-filter (lambda (f) (string-ends? f ".scm")) '("a.scm" "b.txt" "c.scm"))
  =>
  '("a.scm" "c.scm")
) ;check

;; 空文件列表返回空列表
(check (pool-filter (lambda (f) #t) '()) => '())

;; 没有文件满足条件时返回空列表
(check (pool-filter (lambda (f) #f) '("a.scm" "b.txt")) => '())

;; 并发执行时结果顺序与输入顺序一致
(set-goldtool-jobs! 4)
(check
  (pool-filter (lambda (f) (not (string-ends? f ".txt"))) '("a.scm" "b.txt" "c.scm"))
  =>
  '("a.scm" "c.scm")
) ;check

;; 恢复原值，避免影响其它测试
(set-goldtool-jobs! original-jobs)

(check-report)
