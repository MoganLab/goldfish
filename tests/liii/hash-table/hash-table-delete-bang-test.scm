(import (liii check)
        (liii error)
        (liii hash-table))

(check-set-mode! 'report-failed)

;; hash-table-delete!
;; 从哈希表中删除一个或多个键。
;;
;; 语法
;; ----
;; (hash-table-delete! ht key ...)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; key ... : any
;; 一个或多个要删除的键。
;;
;; 返回值
;; ----
;; integer
;; 实际删除的键数量。
;;
;; 注意
;; ----
;; 不存在的键会被忽略，不计入返回值。
;;
;; 示例
;; ----
;; (hash-table-delete! ht 'key) => 1
;;
;; 错误处理
;; ----
;; type-error
;; 当 ht 不是哈希表时抛出错误。

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'key 'value)
  (check (hash-table-delete! ht 'key) => 1)
  (check (hash-table-ref ht 'key) => #f)
  (hash-table-set! ht 'key1 'value1)
  (hash-table-set! ht 'key2 'value2)
  (hash-table-set! ht 'key3 'value3)
  (hash-table-set! ht 'key4 'value4)
  (check (hash-table-delete! ht 'key1 'key2 'key3) => 3)
) ;let

(check-catch 'type-error (hash-table-delete! "not-a-table" 'key))

(check-report)
