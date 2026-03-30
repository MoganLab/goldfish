(import (liii check)
        (liii string)
) ;import

;; string-starts?
;; 检查字符串是否以指定前缀开始。
;;
;; 语法
;; ----
;; (string-starts? str prefix)
;;
;; 参数
;; ----
;; str : string?
;; 要检查的源字符串。
;;
;; prefix : string?
;; 前缀字符串，用于检查str是否以其开始。
;;
;; 返回值
;; ----
;; boolean
;; 如果str以prefix开头返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 该函数默认使用标准SRFI-13中的string-prefix?实现。
;; 空字符串作为prefix时总是返回#t，因为任何字符串都以空字符串开始。
;; 当prefix长度大于str长度时，string-starts?返回#f。
;;
;; 错误处理
;; ----
;; type-error 当参数不是字符串类型时。需要两个参数都是字符串；非字符串参数会抛出type-error。

; Basic functionality tests
(check-true (string-starts? "MathAgape" "Ma"))
(check-true (string-starts? "MathAgape" ""))
(check-true (string-starts? "MathAgape" "MathAgape"))
(check-true (string-starts? "" ""))
(check-true (string-starts? "hello" "h"))
(check-true (string-starts? "hello" "he"))
(check-true (string-starts? "hello" "hello"))
(check-true (string-starts? "test123" "test"))
(check-true (string-starts? "中文测试" "中"))
(check-true (string-starts? "空格 测试" "空格"))

; False case tests
(check-false (string-starts? "MathAgape" "a"))
(check-false (string-starts? "hello" "world"))
(check-false (string-starts? "hello" "hello world"))
(check-false (string-starts? "hello" "ello"))
(check-false (string-starts? "hello" "Hello"))
(check-false (string-starts? "test" "test123"))
(check-false (string-starts? "a" "abc"))
(check-false (string-starts? "" "a"))

; Edge cases
(check-true (string-starts? "a" "a"))
(check-true (string-starts? "a" ""))
(check-false (string-starts? "a" "ab"))
(check-true (string-starts? "abc" ""))
(check-false (string-starts? "abc" "abcd"))
(check-true (string-starts? "中文文字" "中"))
(check-true (string-starts? "Mix3d" "Mix"))

; Error handling
(check-catch 'type-error (string-starts? 123 "hello"))
(check-catch 'type-error (string-starts? "hello" 123))
(check-catch 'type-error (string-starts? 'hello "hello"))
(check-catch 'type-error (string-starts? "hello" 'world))
(check-catch 'type-error (string-starts? '(a b c) "hello"))
(check-catch 'type-error (string-starts? "hello" '\n))

(check-report)
