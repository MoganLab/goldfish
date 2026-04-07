(import (liii check)
        (liii error)
        (liii hash-table)
) ;import

(check-set-mode! 'report-failed)

;; hash-table-set!
;; 将一个或多个键值对写入哈希表。
;;
;; 语法
;; ----
;; (hash-table-set! ht key value ...)
;;
;; 参数
;; ----
;; ht : hash-table
;; 目标哈希表。
;;
;; key value ... : any
;; 交替出现的键和值，至少需要一个键值对。
;;
;; 返回值
;; ----
;; 未指定
;; 本函数主要用于副作用。
;;
;; 注意
;; ----
;; 已存在的键会被覆盖；多个键值对会按顺序依次写入。
;;
;; 示例
;; ----
;; (hash-table-set! ht 'a 1) => 通过副作用写入 a->1
;;
;; 错误处理
;; ----
;; wrong-number-of-args
;; 当额外参数个数为 0 或奇数时抛出错误。
;; type-error
;; 当 ht 不是哈希表时抛出错误。

(let ((ht (make-hash-table)))
  (hash-table-set! ht 'a 1)
  (check (hash-table-ref ht 'a) => 1)
  (hash-table-set! ht 'a 2 'b 3)
  (check (hash-table-ref ht 'a) => 2)
  (check (hash-table-ref ht 'b) => 3)
) ;let

(check-catch 'wrong-number-of-args (hash-table-set! (make-hash-table)))
(check-catch 'wrong-number-of-args (hash-table-set! (make-hash-table) 'a))
(check-catch 'type-error (hash-table-set! "not-a-table" 'a 1))

(check-report)
