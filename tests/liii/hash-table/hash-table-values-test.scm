(import (liii check)
        (liii hash-table)
) ;import

(check-set-mode! 'report-failed)

;; hash-table-values
;; 返回哈希表中所有值的列表。
;;
;; 语法
;; ----
;; (hash-table-values ht)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; 返回值
;; ----
;; list
;; 由哈希表中的值组成的列表。
;;
;; 注意
;; ----
;; 结果列表的顺序由底层遍历顺序决定。
;;
;; 示例
;; ----
;; (hash-table-values ht) => '(v1)
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。

(check (hash-table-values (make-hash-table)) => '())

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'k1 'v1)
  (check (hash-table-values ht) => '(v1))
) ;let

(check-report)
