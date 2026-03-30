(import (liii check)
        (liii hash-table))

(check-set-mode! 'report-failed)

;; hash-table-keys
;; 返回哈希表中所有键的列表。
;;
;; 语法
;; ----
;; (hash-table-keys ht)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; 返回值
;; ----
;; list
;; 由哈希表中的键组成的列表。
;;
;; 注意
;; ----
;; 结果列表的顺序由底层遍历顺序决定。
;;
;; 示例
;; ----
;; (hash-table-keys ht) => '(k1)
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。

(check (hash-table-keys (make-hash-table)) => '())

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'k1 'v1)
  (check (hash-table-keys ht) => '(k1))
) ;let

(check-report)
