;; dedup
;; 列表元素去重，保留各元素首次出现的顺序（使用 memq 比较）。
;;
;; 语法
;; ----
;; (dedup l)
;;
;; 参数
;; ----
;; l : list  待去重的列表
;;
;; 返回值
;; ------
;; list
;; 去重后的新列表。

(import (scheme base) (liii check) (liii goldcheck type-hint util))

(check-set-mode! 'report-failed)

;; 1. 重复符号去重
(check (dedup '(a b a c b d)) => '(a b c d))

;; 2. 无重复列表原样返回
(check (dedup '(x y z)) => '(x y z))

;; 3. 空列表
(check (dedup '()) => '())

(check-report)
