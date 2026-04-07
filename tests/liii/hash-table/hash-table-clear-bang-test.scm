(import (liii check)
        (liii hash-table)
) ;import

(check-set-mode! 'report-failed)

;; hash-table-clear!
;; 清空哈希表中的所有条目。
;;
;; 语法
;; ----
;; (hash-table-clear! ht)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; 返回值
;; ----
;; 未指定
;; 本函数主要用于副作用。
;;
;; 注意
;; ----
;; 清空后，原表中的键都应视为不存在。
;;
;; 示例
;; ----
;; (hash-table-clear! ht) => 清空哈希表
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'key 'value)
  (hash-table-set! ht 'key1 'value1)
  (hash-table-clear! ht)
  (check (hash-table-ref ht 'key) => #f)
  (check (hash-table-ref ht 'key1) => #f)
) ;let

(check-report)
