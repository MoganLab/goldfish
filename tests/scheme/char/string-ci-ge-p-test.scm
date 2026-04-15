(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; string-ci>=?
;; 按大小写不敏感的方式比较字符串的字典序是否递减或相等。
;;
;; 语法
;; ----
;; (string-ci>=? str1 str2 str3 ...) → boolean?
;;
;; 参数
;; ----
;; str1, str2, str3, ... : string
;; 要比较的字符串
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有字符串在大小写不敏感的情况下按字典序递减或相等，则返回 #t，否则返回 #f
;;
;; 错误处理
;; ------
;; type-error
;; 所有参数必须是字符串类型，否则会抛出异常
;; 基本功能测试
(check (string-ci>=? "def" "abc") => #t)
(check (string-ci>=? "DEF" "abc") => #t)
(check (string-ci>=? "def" "ABC") => #t)
(check (string-ci>=? "DEF" "ABC") => #t)
(check (string-ci>=? "world" "hello") => #t)
(check (string-ci>=? "WORLD" "hello") => #t)
;; 相等字符串测试
(check (string-ci>=? "hello" "hello") => #t)
(check (string-ci>=? "HELLO" "hello") => #t)
(check (string-ci>=? "hello" "HELLO") => #t)
;; 小于关系测试
(check (string-ci>=? "abc" "def") => #f)
(check (string-ci>=? "hello" "world") => #f)
;; 多参数测试（递减）
(check (string-ci>=? "c" "b" "a") => #t)
(check (string-ci>=? "C" "B" "A") => #t)
(check (string-ci>=? "c" "B" "a") => #t)
;; 多参数测试（包含相等）
(check (string-ci>=? "b" "a" "a") => #t)
(check (string-ci>=? "b" "A" "a") => #t)
(check (string-ci>=? "c" "b" "b") => #t)
;; 多参数测试（递增，应该失败）
(check (string-ci>=? "a" "b" "c") => #f)
;; 空字符串测试
(check (string-ci>=? "a" "") => #t)
(check (string-ci>=? "abc" "") => #t)
(check (string-ci>=? "" "") => #t)
(check (string-ci>=? "" "abc") => #f)
;; 前缀测试
(check (string-ci>=? "abcd" "abc") => #t)
(check (string-ci>=? "ABCD" "abc") => #t)
(check (string-ci>=? "abc" "abcd") => #f)
;; 错误处理测试
(check-catch 'type-error (string-ci>=? 1 "hello"))
(check-catch 'type-error (string-ci>=? "hello" 'symbol))
(check-catch 'wrong-number-of-args (string-ci>=?))
(check-catch 'wrong-number-of-args (string-ci>=? "hello"))
(check-report)