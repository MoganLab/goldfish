;; find-first-non-empty-index
;; 查找行列表中第一个非空行的索引（从 0 开始）。
;;
;; 语法
;; ----
;; (find-first-non-empty-index lines)
;;
;; 参数
;; ----
;; lines : list of string   文本行列表
;;
;; 返回值
;; ------
;; integer 或 #f
;; 首个非空行的下标，若全为空行则返回 #f。

(import (scheme base) (liii check) (liii goldcheck unit-test file))

(check-set-mode! 'report-failed)

;; 1. 开头即为非空行
(check (find-first-non-empty-index '("hello" "" "world")) => 0)

;; 2. 开头有若干空行
(check (find-first-non-empty-index '("" "  " "\t" "foo" "bar")) => 3)

;; 3. 全为空行返回 #f
(check (find-first-non-empty-index '("" "   " "\n")) => #f)
(check (find-first-non-empty-index '()) => #f)

(check-report)
