;; substring-terminal-width/wide
;; 按显示宽度区间截取字符串，歧义宽度字符按 2 列计算。
;;
;; 语法
;; ----
;; (substring-terminal-width/wide str lo hi)
;;
;; 参数
;; ----
;; str : string?
;; 待截取的字符串。
;;
;; lo : exact-integer?
;; 起始列。
;;
;; hi : exact-integer?
;; 结束列。
;;
;; 返回值
;; ----
;; string?
;; 落在宽度区间内的可视子串。
;;
;; 说明
;; ----
;; 与 substring-terminal-width 的区别仅在于东亚歧义宽度字符
;; （如 ±、×、→）是否按 2 列计算。

(import (liii check) (srfi srfi-166 unicode))

(check-set-mode! 'report-failed)

;; 歧义字符按 2 列：hi=2 时只留下 ±
(check (substring-terminal-width/wide "±x" 0 2) => "±")
(check (substring-terminal-width/wide "±x" 0 3) => "±x")

;; 全角字符每字 2 列
(check (substring-terminal-width/wide "日本" 0 2) => "日")
(check (substring-terminal-width/wide "" 0 0) => "")

(check-report)
