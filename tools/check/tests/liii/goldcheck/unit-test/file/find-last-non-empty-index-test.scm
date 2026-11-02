;; find-last-non-empty-index
;; 查找行列表中最后一个非空行的索引（从 0 开始）。
;;
;; 语法
;; ----
;; (find-last-non-empty-index lines)
;;
;; 参数
;; ----
;; lines : list of string   文本行列表
;;
;; 返回值
;; ------
;; integer 或 #f
;; 最后一个非空行的下标，若全为空行则返回 #f。

(import (scheme base) (liii check) (liii goldcheck unit-test file))

(check-set-mode! 'report-failed)

;; 1. 末尾即为非空行
(check (find-last-non-empty-index '("hello" "" "world")) => 2)

;; 2. 末尾有若干空行
(check (find-last-non-empty-index '("foo" "bar" "" "  " "\t")) => 1)

;; 3. 全为空行返回 #f
(check (find-last-non-empty-index '("" "   " "\n")) => #f)
(check (find-last-non-empty-index '()) => #f)

(check-report)
