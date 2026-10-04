(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)

;; substring
;; 提取字符串的子串。
;;
;; 语法
;; ----
;; (substring string start end)
;;
;; 参数
;; ----
;; string : string
;; 源字符串。
;;
;; start : integer
;; 起始位置（基于字符位置，从 0 开始计数）。
;;
;; end : integer
;; 结束位置（不包含）。
;;
;; 返回值
;; ----
;; string
;; 从 start 到 end（不包含）的子字符串。
;;
;; 说明
;; ----
;; 1. substring 基于字符位置（而非 Unicode 码点位置）进行提取。
;; 2. 包含多字节字符（如中文）的字符串也按字符索引切片。
;;
;; 注意
;; ----
;; substring 是 R7RS (scheme base) 标准函数，返回完整的 Unicode 字符。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当参数类型不正确时。
;; out-of-range 当 start 或 end 超出字符串范围时。

;; 基本 ASCII 测试
(check (substring "Hello" 0 5) => "Hello")
(check (substring "Hello" 0 2) => "He")
(check (substring "Hello" 2 4) => "ll")
(check (substring "Hello" 1 5) => "ello")
(check (substring "Hello" 0 0) => "")
(check (substring "Hello" 3 3) => "")

;; 空字符串
(check (substring "" 0 0) => "")

;; 边界测试
(check (substring "abc" 0 1) => "a")
(check (substring "abc" 1 2) => "b")
(check (substring "abc" 2 3) => "c")

;; 错误处理
(check-catch 'out-of-range (substring "Hello" 0 6))
(check-catch 'out-of-range (substring "Hello" 6 6))
(check-catch 'out-of-range (substring "Hello" 3 2))


(check-catch 'wrong-type-arg (substring "中🐟a" "0" 2))
(check-catch 'wrong-type-arg (substring "中🐟a" 0 1.5))
(check-catch 'out-of-range (substring "中🐟a" 0 4))

(check-report)
