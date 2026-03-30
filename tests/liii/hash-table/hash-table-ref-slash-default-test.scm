(import (liii check)
        (liii hash-table))

(check-set-mode! 'report-failed)

;; hash-table-ref/default
;; 读取哈希表中的值；未命中时返回默认值。
;;
;; 语法
;; ----
;; (hash-table-ref/default ht key default)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; key : any
;; 要读取的键。
;;
;; default : any | procedure?
;; 未命中时返回的值；如果是过程则会调用它。
;;
;; 返回值
;; ----
;; any
;; 命中时返回对应值，未命中时返回默认值或默认过程的结果。
;;
;; 注意
;; ----
;; 该函数不会修改原哈希表。
;;
;; 示例
;; ----
;; (hash-table-ref/default ht 'missing 'fallback) => 'fallback
;;
;; 错误处理
;; ----
;; 非哈希表输入时由底层实现报错。

(let ((ht (make-hash-table)))
  (check (hash-table-ref/default ht 'key 'value1) => 'value1)
  (check (hash-table-ref/default ht 'key (lambda () 3)) => 3)
  (hash-table-set! ht 'key 'value)
  (check (hash-table-ref/default ht 'key 'value1) => 'value)
) ;let

(check-report)
