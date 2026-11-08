;; fitted/both
;; 居中内容的定宽组合子：两侧填充、超出时裁剪。
;;
;; 语法
;; ----
;; (fitted/both width . ls)
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
;; 输出恰好占 width 列的格式化组合子。
;;
;; 说明
;; ----
;; 填充时余数落在右侧，裁剪时保留尾部内容。
;; `(liii format base)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base))

(check-set-mode! 'report-failed)

(check (show #f (fitted/both 6 "abc")) => " abc  ")
(check (show #f (fitted/both 6 "abcdefgh")) => "bcdefg")
(check (show #f (fitted/both 0 "abc")) => "")

(check-report)
