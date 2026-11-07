;; pi-format-range
;; 将行范围字典格式化为行号或区间字符串。
;;
;; 语法
;; ----
;; (pi-format-range range)
;;
;; 参数
;; ----
;; range : list?
;; 包含 start 和 end 字段的区间 alist。
;;
;; 返回值
;; ----
;; string?
;; 单行号或以连字符分隔的范围字符串。

(import (liii check)
        (liii pi simplify scope)
) ;import

(check-set-mode! 'report-failed)

;; 1. 单行范围（start == end）
(check (pi-format-range '((start . 5) (end . 5))) => "5")
(check (pi-format-range '((start . 42) (end . 42))) => "42")

;; 2. 多行区间（start < end）
(check (pi-format-range '((start . 10) (end . 20))) => "10-20")
(check (pi-format-range '((start . 1) (end . 2))) => "1-2")

(check-report)
