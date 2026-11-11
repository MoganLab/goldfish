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
;; 向 S7 原生哈希表中插入或更新键值映射。
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
