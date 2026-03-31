(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; string=?
;; 比较两个字符串是否相等。
;;
;; 语法
;; ----
;; (string=? string1 string2 ...)
;;
;; 参数
;; ----
;; string1, string2, ... : string?
;; 至少两个字符串参数。
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有字符串都相等则返回 #t，否则返回 #f。比较区分大小写。
;;
;; 说明
;; ----
;; 1. 至少需要两个参数
;; 2. 所有参数必须是字符串类型
;; 3. 字符串比较区分大小写
;; 4. 当所有字符串内容完全相同返回 #t
;; 5. 返回布尔值结果
;;
;; 边界情况
;; --------
;; - 区分大小写：大写和小写字母被认为不同
;; - 空字符串比较：空字符串只与空字符串相等
;; - 特殊字符：所有字符都需要完全相同，包括空格、制表符等
;;
;; 错误处理
;; --------
;; wrong-number-of-args
;; 当参数数量少于2个时抛出错误。

;; string=? 基本测试
(check-true (string=? "hello" "hello"))
(check-true (string=? "" ""))
(check-true (string=? "a" "a"))

;; string=? 区分大小写测试
(check-false (string=? "Hello" "hello"))
(check-false (string=? "HELLO" "hello"))
(check-false (string=? "abc" "ABC"))

;; string=? 不同内容测试
(check-false (string=? "hello" "world"))
(check-false (string=? "abc" "def"))
(check-false (string=? "short" "longer"))

;; string=? 各种边界情况
(check-true (string=? "123" "123"))
(check-true (string=? "!@#$%" "!@#$%"))
(check-true (string=? "空格 测试" "空格 测试"))
(check-false (string=? "abc" "abcd"))
(check-false (string=? "abcd" "abc"))

;; string=? 特殊字符测试
(check-true (string=? "\n\t" "\n\t"))
(check-true (string=? "测试文本" "测试文本"))
(check-false (string=? "测试" "测试文本"))
(check-false (string=? "测试文本" "测试"))

;; string=? 空字符串测试
(check-true (string=? "" "" ""))
(check-true (string=? "a" "a" "a"))

;; string=? 多参数测试
(check-true (string=? "same" "same" "same"))
(check-false (string=? "same" "diff" "same"))
(check-false (string=? "one" "two" "three"))

;; string=? 二进制和Unicode字符串
(check-true (string=? "Hello, 世界!" "Hello, 世界!"))
(check-false (string=? "Hello, 世界!" "Hello, 世界! "))

;; 错误处理测试
(check-catch 'wrong-number-of-args (string=?))
(check-catch 'wrong-number-of-args (string=? "hello"))

(check-report)
