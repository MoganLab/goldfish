;; string-drop
;; 从字符串开头移除指定数量的字符。
;;
;; 语法
;; ----
;; (string-drop str k)
;;
;; 参数
;; ----
;; str : string?
;; 源字符串，从中移除字符。
;;
;; k : integer?
;; 要移除的字符数量，必须是非负整数且不超过字符串长度。
;;
;; 返回值
;; ----
;; string
;; 返回一个新的字符串，包含源字符串从位置k开始的所有字符。
;;
;; 注意
;; ----
;; string-drop等价于(substring str k (string-length str))，但提供了更语义化的名称。
;; 对于多字节Unicode字符，操作基于字节位置而非字符位置。例如，每个中文字符占用3个字节，emoji字符通常占用4个字节。
;;
;; 错误处理
;; ----
;; out-of-range 当k大于字符串长度或k为负数时
;; type-error 当str不是字符串类型或k不是整数类型时

(import (liii check) (srfi srfi-13))


;;
;; 说明
;; ----
;; (liii string-cursor) 库中也提供了 string-drop 函数，
;; 该版本支持 Unicode 字符级别的操作，并提供 cursor-based API。
;; 参见: gf doc liii/string-cursor "string-drop"

(check (string-drop "MathAgape" 4) => "Agape")
(check (string-drop "MathAgape" 0) => "MathAgape")
(check (string-drop "MathAgape" 9) => "")
(check (string-drop "MathAgape" 8) => "e")
(check (string-drop "MathAgape" 1) => "athAgape")
(check (string-drop "MathAgape" 2) => "thAgape")
(check (string-drop "MathAgape" 3) => "hAgape")
(check (string-drop "MathAgape" 5) => "gape")
(check (string-drop "MathAgape" 6) => "ape")
(check (string-drop "MathAgape" 7) => "pe")
(check (string-drop "" 0) => "")
(check (string-drop "a" 1) => "")
(check (string-drop "Hello" 1) => "ello")
(check (string-drop "Hello" 5) => "")
(check (string-drop "Hello" 0) => "Hello")
(check (string-drop "abc" 2) => "c")
(check (string-drop "abc" 1) => "bc")
(check (string-drop "test123" 4) => "123")
(check (string-drop "test123" 3) => "t123")
(check (string-drop "test123" 6) => "3")
(check (string-drop "test123" 7) => "")
(check (string-drop "中文测试" 6) => "测试")
(check (string-drop "中文测试" 3) => "文测试")
(check (string-drop "中文测试" 12) => "")
(check (string-drop "🌟🎉" 4) => "🎉")
(check (string-drop "🌟🎉" 8) => "")

(check-catch 'out-of-range (string-drop "MathAgape" 20))
(check-catch 'out-of-range (string-drop "" 1))
(check-catch 'out-of-range (string-drop "Hello" -1))
(check-catch 'type-error (string-drop 123 4))
(check-catch 'type-error (string-drop "MathAgape" "4"))
(check-catch 'type-error (string-drop "MathAgape" 4.5))
(check-catch 'type-error (string-drop "MathAgape" 'a))

(check (string-drop "MathAgape" 8) => "e")
(check (string-drop "MathAgape" 9) => "")
(check (string-drop "MathAgape" 0) => "MathAgape")

(check-catch 'out-of-range (string-drop "MahtAgape" -1))
(check-catch 'out-of-range (string-drop "MathAgape" 20))

(check-report)
