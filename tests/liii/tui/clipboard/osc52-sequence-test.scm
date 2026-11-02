;; osc52-sequence
;; 将文本字符串转换为用于向剪贴板写入内容的 OSC 52 ANSI 转义序列。
;;
;; 语法
;; ----
;; (osc52-sequence text)
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

;; 1. ASCII 字符串转义序列构造
(check (osc52-sequence "hello") => "\x1b;]52;c;aGVsbG8=\x07;")
(check (osc52-sequence "hello world\n") => "\x1b;]52;c;aGVsbG8gd29ybGQK\x07;")

;; 2. 空字符串
(check (osc52-sequence "") => "\x1b;]52;c;\x07;")

;; 3. UTF-8 中文字符串
(let ((seq (osc52-sequence "中文测试")))
  (check-true (string? seq))
  (check (substring seq 0 7) => "\x1b;]52;c;")
  (check (substring seq (- (string-length seq) 1) (string-length seq)) => "\x07;")
) ;let

(check-report)
