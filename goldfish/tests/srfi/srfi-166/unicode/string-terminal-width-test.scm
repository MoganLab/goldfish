;; string-terminal-width
;; 计算字符串在终端上的显示宽度。
;;
;; 语法
;; ----
;; (string-terminal-width str [start [end]])
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
;; 显示列宽：全角字符计 2 列、半角计 1 列、
;; ANSI 转义序列计 0 列，歧义字符计 1 列。
;;
;; 说明
;; ----
;; 歧义宽度字符需要按 2 列计算时请使用同库的
;; string-terminal-width/wide。

(import (liii check) (srfi srfi-166 unicode))

(check-set-mode! 'report-failed)

(check (string-terminal-width "日本語") => 6)
(check (string-terminal-width "abc") => 3)
(check (string-terminal-width "\x1b;[31mred\x1b;[39m") => 3)

(check (substring-terminal-width "ａｂｃ" 0 6) => "ａｂｃ")
(check (substring-terminal-width "ａｂｃ" 0 4) => "ａｂ")
(check (substring-terminal-width "ａｂｃ" 2 6) => "ｂｃ")
(check (substring-terminal-width "ａｂｃ" 1 4) => "ａｂ")
(check (substring-terminal-width "ａｂｃ" 1 5) => "ａｂ")
(check (substring-terminal-width "ａｂｃ" 2 4) => "ｂ")
(check (substring-terminal-width "ａｂｃ" 2 3) => "")
(check (substring-terminal-width "ａｂｃ" -1 2) => "ａ")

(check-report)
