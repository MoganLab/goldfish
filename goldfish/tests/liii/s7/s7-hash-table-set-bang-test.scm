;; s7-hash-table-set!
;; S7 内置的向哈希表设置键值函数。
;;
;; 语法
;; ----
;; (s7-hash-table-set! table key value)
;;
;; 参数
;; ----
;; table : hash-table?
;; 目标哈希表。
;; key : any?
;; 要设置的键。
;; value : any?
;; 要关联的值。
;;
;; 返回值
;; ----
;; any?
;; 返回设置的值。
;;
;; 说明
;; ----
;; 1. 与 `hash-table-set!` 的区别：`s7-hash-table-set!` 写入 S7 原生哈希表
;;    （由 `s7-make-hash-table` 创建），只能写入 S7 原生表；
;;    `hash-table-set!` 写入 Goldfish 自己的哈希表（由 `make-hash-table` 创建），
;;    可配合完整 API 使用。两种哈希表互不通用。
;; 2. `s7-hash-table-set!` 不做 R7RS 的参数检查，非法参数会抛出 S7 原生错误。
;;
;; 选择建议：常规代码用 `hash-table-set!`；需要操作 S7 原生哈希表时用 `s7-hash-table-set!`。
;;
;; 参见: gf doc srfi/srfi-125 "hash-table-set!"

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(let ((ht (s7-make-hash-table)))
  (check (s7-hash-table-set! ht "foo" 42) => 42)
  (check (ht "foo") => 42)
  (s7-hash-table-set! ht 'bar "hello")
  (check (ht 'bar) => "hello")
) ;let

(check-report)
