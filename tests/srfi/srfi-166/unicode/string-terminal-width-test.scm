(import (liii check)
        (srfi srfi-166 unicode))

(check-set-mode! 'report-failed)

;; string-terminal-width
;; 计算字符串在终端上的显示宽度，全角字符计为 2 列，半角字符计为 1 列，ANSI 转义序列计为 0 列。
;;
;; 语法
;; ----
;; (string-terminal-width str [start] [end])
;; (string-terminal-width/wide str [start] [end])
;; (substring-terminal-width str lo hi)
;; (substring-terminal-width/wide str lo hi)
;;
;; 示例
;; ----
;; (string-terminal-width "日本語") => 6
;; (substring-terminal-width "ａｂｃ" 0 4) => "ａｂ"

(check (string-terminal-width "日本語") => 6)
(check (string-terminal-width "abc") => 3)
(check (string-terminal-width "\x1b[31mred\x1b[39m") => 3)

(check (substring-terminal-width "ａｂｃ" 0 6) => "ａｂｃ")
(check (substring-terminal-width "ａｂｃ" 0 4) => "ａｂ")
(check (substring-terminal-width "ａｂｃ" 2 6) => "ｂｃ")
(check (substring-terminal-width "ａｂｃ" 1 4) => "ａｂ")
(check (substring-terminal-width "ａｂｃ" 1 5) => "ａｂ")
(check (substring-terminal-width "ａｂｃ" 2 4) => "ｂ")
(check (substring-terminal-width "ａｂｃ" 2 3) => "")
(check (substring-terminal-width "ａｂｃ" -1 2) => "ａ")

(check-report)
