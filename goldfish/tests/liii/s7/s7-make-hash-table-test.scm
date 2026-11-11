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
;; 创建 S7 原生哈希表。

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
