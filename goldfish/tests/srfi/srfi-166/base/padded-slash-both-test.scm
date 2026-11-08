;; padded/both
;; 两侧填充组合子：把内容居中补齐到目标宽度。
;;
;; 语法
;; ----
;; (padded/both width . ls)
;;
;; 参数
;; ----
;; width : exact-integer?
;; 目标显示宽度。
;;
;; ls : format?
;; 零个或多个格式化对象。
;;
;; 返回值
;; ----
;; format?
;; 输出至少占 width 列、内容居中的格式化组合子。
;;
;; 说明
;; ----
;; 多出的填充列无法平分时余数落在右侧；内容已超出宽度时
;; 原样输出，不裁剪。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (padded/both 6 "abc")) => " abc  ")
(check (show #f (padded/both 7 "abc")) => "  abc  ")
(check (show #f (padded/both 6 "")) => "      ")
(check (show #f (padded/both 2 "abcde")) => "abcde")

(check-report)
