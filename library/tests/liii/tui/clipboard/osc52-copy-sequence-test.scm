;; osc52-copy-sequence
;; osc52-sequence 的别名，将文本字符串转换为用于向剪贴板写入内容的 OSC 52 ANSI 转义序列。
;;
;; 语法
;; ----
;; (osc52-copy-sequence text)
;;
;; 参数
;; ----
;; text : string
;; 要写入剪贴板的文本字符串。
;;
;; 返回值
;; ----
;; string
;; 格式为 "\x1b;]52;c;<base64>\x07;" 的 OSC 52 转义序列。

(import (scheme base) (liii check) (liii tui clipboard))

(check-set-mode! 'report-failed)

;; 1. 基本复制转义序列
(check (osc52-copy-sequence "hello") => "\x1b;]52;c;aGVsbG8=\x07;")
(check (osc52-copy-sequence "") => "\x1b;]52;c;\x07;")

;; 2. 与 osc52-sequence 等价
(check (osc52-copy-sequence "foo bar") => (osc52-sequence "foo bar"))

(check-report)
