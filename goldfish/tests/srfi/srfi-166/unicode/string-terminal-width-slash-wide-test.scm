;; string-terminal-width/wide
;; 计算字符串在终端上的显示宽度，歧义宽度字符按 2 列计算。
;;
;; 语法
;; ----
;; (string-terminal-width/wide str [start [end]])
;;
;; 参数
;; ----
;; str : string?
;; 待计算宽度的字符串。
;;
;; start : exact-integer?
;; 可选，起始字符索引，默认 0。
;;
;; end : exact-integer?
;; 可选，结束字符索引，默认字符串长度。
;;
;; 返回值
;; ----
;; exact-integer?
;; 显示列宽；全角与歧义字符计 2 列，ANSI 转义序列计 0 列。
;;
;; 说明
;; ----
;; 与 string-terminal-width 的区别仅在于东亚歧义宽度字符
;; （如 ±、×、→）是否按 2 列计算。

(import (liii check) (srfi srfi-166 unicode))

(check-set-mode! 'report-failed)

;; 歧义字符按 2 列
(check (string-terminal-width/wide "±") => 2)
(check (string-terminal-width/wide "日本") => 4)
(check (string-terminal-width/wide "abc") => 3)

;; ANSI 转义序列计 0 列
(check (string-terminal-width/wide "\x1b;[31mred\x1b;[39m") => 3)

;; start / end 限定范围
(check (string-terminal-width/wide "±日本" 1) => 4)
(check (string-terminal-width/wide "±日本" 0 2) => 2)
(check (string-terminal-width/wide "" 0 0) => 0)

(check-report)
