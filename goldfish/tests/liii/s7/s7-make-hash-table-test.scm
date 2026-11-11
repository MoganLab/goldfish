;; s7-make-hash-table
;; S7 内置的创建哈希表函数。
;;
;; 语法
;; ----
;; (s7-make-hash-table [size] [eq-func] [typer])
;;
;; 参数
;; ----
;; size : integer?
;; 初始大小，可选，默认为 8。
;; eq-func : procedure?
;; 键等价性判断函数，可选。
;; typer : any?
;; 类型检查器，可选。
;;
;; 返回值
;; ----
;; hash-table?
;; 新创建的哈希表。
;;
;; 说明
;; ----
;; 1. 与 `make-hash-table` 的区别：`s7-make-hash-table` 创建的是 S7 原生哈希表，
;;    初始化更快，但只支持 S7 的基础接口（等价比较 + 固定容量），
;;    需要用 `s7-hash-table-set!` 写入；`make-hash-table` 创建的是 Goldfish 自己的
;;    哈希表，支持比较器（comparator），可配合完整 API 使用。
;; 2. 两种哈希表互不通用，不能把 `s7-make-hash-table` 得到的表交给
;;    `hash-table-set!` 等 Goldfish API。
;;
;; 选择建议：常规代码用 `make-hash-table`；需要与 S7 内建函数共享同一张原生哈希表
;; 或追求原生性能时用 `s7-make-hash-table`。
;;
;; 参见: gf doc srfi/srfi-125 "make-hash-table"

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(let ((ht (s7-make-hash-table)))
  (check (hash-table? ht) => #t)
) ;let

(let ((ht (s7-make-hash-table 16)))
  (check (hash-table? ht) => #t)
) ;let

(let ((ht (s7-make-hash-table 8 eq?)))
  (check (hash-table? ht) => #t)
) ;let

(check-report)
