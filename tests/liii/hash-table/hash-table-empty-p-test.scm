(import (liii check)
        (liii hash-table)
) ;import

(check-set-mode! 'report-failed)

;; hash-table-empty?
;; 判断哈希表是否为空。
;;
;; 语法
;; ----
;; (hash-table-empty? ht)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; 返回值
;; ----
;; boolean
;; 若表中没有任何条目则返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 它本质上是对哈希表大小是否为 0 的判断。
;;
;; 示例
;; ----
;; (hash-table-empty? (make-hash-table)) => #t
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。

(check-true (hash-table-empty? (make-hash-table)))

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'key 'value)
  (check-false (hash-table-empty? ht))
) ;let

(check-report)
