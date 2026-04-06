(import (liii check)
        (scheme char)
) ;import

(check-set-mode! 'report-failed)

;; string-ci<=?
;; 按大小写不敏感的方式比较字符串的字典序是否递增或相等。
;;
;; 语法
;; ----
;; (string-ci<=? str1 str2 str3 ...) → boolean?
;;
;; 参数
;; ----
;; str1, str2, str3, ... : string
;; 要比较的字符串
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有字符串在大小写不敏感的情况下按字典序递增或相等，则返回 #t，否则返回 #f
;;
;; 错误处理
;; ------
;; type-error
;; 所有参数必须是字符串类型，否则会抛出异常

;; 基本功能测试
(check (string-ci<=? "abc" "def") => #t)
(check (string-ci<=? "ABC" "def") => #t)
(check (string-ci<=? "abc" "DEF") => #t)
(check (string-ci<=? "ABC" "DEF") => #t)
(check (string-ci<=? "hello" "world") => #t)
(check (string-ci<=? "HELLO" "world") => #t)

;; 相等字符串测试
(check (string-ci<=? "hello" "hello") => #t)
(check (string-ci<=? "HELLO" "hello") => #t)
(check (string-ci<=? "hello" "HELLO") => #t)

;; 大于关系测试
(check (string-ci<=? "def" "abc") => #f)
(check (string-ci<=? "world" "hello") => #f)

;; 多参数测试（递增）
(check (string-ci<=? "a" "b" "c") => #t)
(check (string-ci<=? "A" "B" "C") => #t)
(check (string-ci<=? "a" "B" "c") => #t)

;; 多参数测试（包含相等）
(check (string-ci<=? "a" "a" "b") => #t)
(check (string-ci<=? "a" "A" "b") => #t)
(check (string-ci<=? "a" "b" "b") => #t)

;; 多参数测试（递减，应该失败）
(check (string-ci<=? "c" "b" "a") => #f)

;; 空字符串测试
(check (string-ci<=? "" "a") => #t)
(check (string-ci<=? "" "abc") => #t)
(check (string-ci<=? "" "") => #t)
(check (string-ci<=? "abc" "") => #f)

;; 前缀测试
(check (string-ci<=? "abc" "abcd") => #t)
(check (string-ci<=? "ABC" "abcd") => #t)
(check (string-ci<=? "abcd" "abc") => #f)

;; 错误处理测试
(check-catch 'type-error (string-ci<=? 1 "hello"))
(check-catch 'type-error (string-ci<=? "hello" 'symbol))
(check-catch 'wrong-number-of-args (string-ci<=?))
(check-catch 'wrong-number-of-args (string-ci<=? "hello"))

(check-report)
