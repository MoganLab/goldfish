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
;; 说明
;; ----
;; 与 S7 原生函数 `s7-hash-table-set!` 的区别：
;; - `hash-table-set!` 写入 Goldfish 自己的哈希表（由 `make-hash-table` 创建），
;;   可以配合 hash-table-ref、hash-table-update! 等完整 API 使用。
;; - `s7-hash-table-set!` 写入 S7 原生哈希表（由 `s7-make-hash-table` 创建），
;;   只能写入 S7 原生表，两种哈希表互不通用。
;; - `hash-table-set!` 对表类型做检查并统一抛出 type-error，
;;   `s7-hash-table-set!` 交由 S7 内建报错。
;;
;; 选择建议：常规代码用 `hash-table-set!`（功能完整、错误类型统一）；
;; 需要操作 S7 原生哈希表时用 `s7-hash-table-set!`。
;;
;; 参见: gf doc liii/s7 "s7-hash-table-set!"
;;
;; 错误处理
;; ----
;; wrong-number-of-args
;; 当额外参数个数为 0 或奇数时抛出错误。
;; type-error
;; 当 ht 不是哈希表时抛出错误。

(import (liii check) (liii error) (liii hash-table))


(check-set-mode! 'report-failed)

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
