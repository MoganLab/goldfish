;; utf8-string-ref
;; 按 Unicode 码点位置读取字符串中的字符。
;;
;;
;; 语法
;; ----
;; (utf8-string-ref string k)
;;
;; 参数
;; ----
;; string : string?
;; UTF-8 编码的字符串。
;;
;; k : exact?
;; 码点序号（从 0 开始），必须满足 0 <= k < (utf8-string-length string)。
;;
;; 返回值
;; ----
;; char?
;; 位置 k 处的 Unicode 字符。
;;
;; 说明
;; ----
;; 1. 与 string-ref 不同，utf8-string-ref 按码点而非字节字符计数，
;;    多字节字符（中文、emoji 等）计为 1 个码点。
;; 2. 对纯 ASCII 字符串，与 string-ref 行为一致。
;;
;; 错误处理
;; ----
;; out-of-range
;; 当 k 为负数或大于等于码点数时抛出错误。
;;
;; type-error
;; 当 string 不是字符串时抛出错误。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; ASCII 测试（与 string-ref 一致）
(check (utf8-string-ref "hello" 0) => #\h)
(check (utf8-string-ref "hello" 4) => #\o)
(check (utf8-string-ref "a" 0) => #\a)
(check (utf8-string-ref "ABC" 2) => #\C)
(check (utf8-string-ref "!@#" 0) => #\!)
(check (utf8-string-ref "123" 0) => #\1)
(check (utf8-string-ref "   " 1) => #\space)
(check (utf8-string-ref "xyz" 2) => #\z)

;; 中文测试（每个汉字 1 个码点）
(check (utf8-string-ref "中" 0) => #\中)
(check (utf8-string-ref "中文" 0) => #\中)
(check (utf8-string-ref "中文" 1) => #\文)
(check (utf8-string-ref "你好世界" 2) => #\世)
(check (utf8-string-ref "你好世界" 3) => #\界)

;; 混合字符串测试
(check (utf8-string-ref "a中b" 0) => #\a)
(check (utf8-string-ref "a中b" 1) => #\中)
(check (utf8-string-ref "a中b" 2) => #\b)
(check (utf8-string-ref "Hello 你好" 6) => #\你)
(check (utf8-string-ref "Hello 你好" 7) => #\好)
(check (utf8-string-ref "Hello 你好" 5) => #\space)

;; Emoji 测试（每个 emoji 1 个码点，4 字节）
(check (utf8-string-ref "👍" 0) => #\👍)
(check (utf8-string-ref "👍🚀🎉" 0) => #\👍)
(check (utf8-string-ref "👍🚀🎉" 1) => #\🚀)
(check (utf8-string-ref "👍🚀🎉" 2) => #\🎉)
(check (utf8-string-ref "Hello 👍 World" 6) => #\👍)

;; 码点数值验证
(check (char->integer (utf8-string-ref "中" 0)) => 20013)
(check (char->integer (utf8-string-ref "👍" 0)) => 128077)

;; 与 utf8-string-length 一致性
(check (utf8-string-ref "你好世界" (- (utf8-string-length "你好世界") 1)) => #\界)

;; 错误处理
(check-catch 'out-of-range (utf8-string-ref "" 0))
(check-catch 'out-of-range (utf8-string-ref "abc" 3))
(check-catch 'out-of-range (utf8-string-ref "a" 1))
(check-catch 'out-of-range (utf8-string-ref "中" 1))
(check-catch 'out-of-range (utf8-string-ref "中文" 2))
(check-catch 'out-of-range (utf8-string-ref "👍🚀🎉" 3))
(check-catch 'out-of-range (utf8-string-ref "abc" -1))
(check-catch 'type-error (utf8-string-ref 123 0))
(check-catch 'type-error (utf8-string-ref 'symbol 0))

(check-report)
