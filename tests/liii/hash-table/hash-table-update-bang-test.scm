(import (liii check)
        (liii error)
        (liii hash-table)
) ;import

(check-set-mode! 'report-failed)

;; hash-table-update!
;; 直接将哈希表中指定键更新为新值。
;;
;; 语法
;; ----
;; (hash-table-update! ht key value)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; key : any
;; 要更新的键。
;;
;; value : any
;; 要写入的新值。
;;
;; 返回值
;; ----
;; 未指定
;; 本函数主要用于副作用，语义上等同于对单个键执行写入。
;;
;; 注意
;; ----
;; 该函数不会保留旧值，而是直接覆盖为新值。
;;
;; 示例
;; ----
;; (hash-table-update! ht 'key 'value) => 通过副作用写入
;;
;; 错误处理
;; ----
;; type-error
;; 当 ht 不是哈希表时抛出错误。

(let ((ht (make-hash-table)))
  (hash-table-update! ht 'key 'value)
  (check (hash-table-ref ht 'key) => 'value)
  (hash-table-update! ht 'key 'value1)
  (check (hash-table-ref ht 'key) => 'value1)
) ;let

(check-catch 'type-error (hash-table-update! "not-a-table" 'key 'value))

(check-report)
