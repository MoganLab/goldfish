;; substring-terminal-width
;; 按显示宽度区间 [lo, hi) 截取字符串的可视部分。
;;
;; 语法
;; ----
;; (substring-terminal-width str lo hi)
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
;; 落在宽度区间内的可视子串；歧义字符按 1 列计算，
;; ANSI 转义序列随所在区间保留或跳过且不占宽度。
;;
;; 说明
;; ----
;; 与 substring-terminal-width/wide 的区别仅在于东亚歧义
;; 宽度字符是否按 2 列计算。

(import (liii check) (srfi srfi-166 unicode))

(check-set-mode! 'report-failed)

;; 全角字符每字 2 列
(check (substring-terminal-width "ａｂｃ" 0 4) => "ａｂ")
(check (substring-terminal-width "abc" 1 2) => "b")
(check (substring-terminal-width "abc" 0 0) => "")

;; 歧义字符按 1 列
(check (substring-terminal-width "±x" 0 2) => "±x")

;; ANSI 转义序列不占宽度
(check (substring-terminal-width "\x1b;[31mab" 0 2) => "ab")

(check-report)
