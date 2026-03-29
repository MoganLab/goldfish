(import (liii check)
        (liii string))

;; string-take
;; 从字符串开头提取指定数量的字符。
;;
;; 语法
;; ----
;; (string-take str k)
;;
;; 参数
;; ----
;; str : string?
;; 源字符串，从中提取字符。
;;
;; k : integer?
;; 要提取的字符数量，必须是非负整数且不超过字符串长度。
;;
;; 返回值
;; ----
;; string
;; 包含源字符串前k个字符的新字符串。
;;
;; 注意
;; ----
;; string-take等价于(substring str 0 k)，但提供了更语义化的名称。
;; 对于多字节Unicode字符，操作基于字节位置而非字符位置。例如，每个中文字符占用3个字节，emoji字符通常占用4个字节。
;;
;; 示例
;; ----
;; (string-take "MathAgape" 4) => "Math"
;; (string-take "Hello" 0) => ""
;; (string-take "abc" 2) => "ab"
;;
;; 错误处理
;; ----
;; out-of-range 当k大于字符串长度或k为负数时
;; wrong-type-arg 当str不是字符串类型或k不是整数类型时

(check (string-take "MathAgape" 4) => "Math")
(check (string-take "MathAgape" 0) => "")
(check (string-take "MathAgape" 9) => "MathAgape")
(check (string-take "" 0) => "")
(check (string-take "a" 1) => "a")
(check (string-take "Hello" 1) => "H")
(check (string-take "abc" 2) => "ab")
(check (string-take "test123" 4) => "test")
(check (string-take "中文测试" 6) => "中文")
(check (string-take "🌟🎉" 4) => "🌟")
(check-catch 'out-of-range (string-take "MathAgape" 20))
(check-catch 'out-of-range (string-take "" 1))
(check-catch 'out-of-range (string-take "Hello" -1))
(check-catch 'wrong-type-arg (string-take 123 4))
(check-catch 'wrong-type-arg (string-take "MathAgape" "4"))
(check-catch 'wrong-type-arg (string-take "MathAgape" 4.5))
(check-catch 'wrong-type-arg (string-take "MathAgape" 'a))

(check-report)
