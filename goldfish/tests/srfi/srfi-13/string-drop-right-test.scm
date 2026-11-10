;; string-drop-right
;; 从字符串末尾移除指定数量的字符。
;;
;; 语法
;; ----
;; (string-drop-right str k)
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
;; 返回一个新的字符串，包含源字符串前 (string-length str) - k 个字符。
;;
;; 注意
;; ----
;; string-drop-right 等价于 (substring str 0 (- (string-length str) k))，但提供了更语义化的名称。
;; 对于多字节Unicode字符，操作基于字节位置而非字符位置。例如，每个中文字符占用3个字节，emoji字符通常占用4个字节。
;;
;; 错误处理
;; ----
;; out-of-range 当k大于字符串长度或k为负数时
;; type-error 当str不是字符串类型或k不是整数类型时

(import (liii check) (srfi srfi-13))

(check (string-drop-right "MathAgape" 4) => "MathA")
(check (string-drop-right "MathAgape" 0) => "MathAgape")
(check (string-drop-right "MathAgape" 9) => "")
(check (string-drop-right "MathAgape" 8) => "M")
(check (string-drop-right "MathAgape" 1) => "MathAgap")
(check (string-drop-right "MathAgape" 2) => "MathAga")
(check (string-drop-right "MathAgape" 3) => "MathAg")
(check (string-drop-right "MathAgape" 5) => "Math")
(check (string-drop-right "MathAgape" 6) => "Mat")
(check (string-drop-right "MathAgape" 7) => "Ma")
(check (string-drop-right "" 0) => "")
(check (string-drop-right "a" 1) => "")
(check (string-drop-right "Hello" 1) => "Hell")
(check (string-drop-right "Hello" 5) => "")
(check (string-drop-right "Hello" 0) => "Hello")
(check (string-drop-right "abc" 2) => "a")
(check (string-drop-right "abc" 1) => "ab")
(check (string-drop-right "test123" 3) => "test")
(check (string-drop-right "test123" 4) => "tes")
(check (string-drop-right "test123" 6) => "t")
(check (string-drop-right "test123" 7) => "")
(check (string-drop-right "中文测试" 6) => "中文")
(check (string-drop-right "中文测试" 3) => "中文测")
(check (string-drop-right "中文测试" 12) => "")
(check (string-drop-right "🌟🎉" 4) => "🌟")
(check (string-drop-right "🌟🎉" 8) => "")

(check-catch 'out-of-range (string-drop-right "MathAgape" 20))
(check-catch 'out-of-range (string-drop-right "" 1))
(check-catch 'out-of-range (string-drop-right "Hello" -1))
(check-catch 'type-error (string-drop-right 123 4))
(check-catch 'type-error (string-drop-right "MathAgape" "4"))
(check-catch 'type-error (string-drop-right "MathAgape" 4.5))
(check-catch 'type-error (string-drop-right "MathAgape" 'a))

(check (string-drop-right "MathAgape" 5) => "Math")
(check (string-drop-right "MathAgape" 9) => "")
(check (string-drop-right "MathAgape" 0) => "MathAgape")

(check-catch 'out-of-range (string-drop-right "MathAgape" -1))
(check-catch 'out-of-range (string-drop-right "MathAgape" 20))

(check-report)
