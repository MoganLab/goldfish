(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; string-ci<?
;; 按大小写不敏感的方式比较字符串的字典序是否递增。
;;
;; 语法
;; ----
;; (string-ci<? str1 str2 str3 ...) → boolean?
;;
;; 参数
;; ----
;; str1, str2, str3, ... : string
;; 要比较的字符串
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有字符串在大小写不敏感的情况下按字典序递增，则返回 #t，否则返回 #f
;;
;; 错误处理
;; ------
;; type-error
;; 所有参数必须是字符串类型，否则会抛出异常
;; 基本功能测试
(check (string-ci<? "abc" "def") => #t)
(check (string-ci<? "ABC" "def") => #t)
(check (string-ci<? "abc" "DEF") => #t)
(check (string-ci<? "ABC" "DEF") => #t)
(check (string-ci<? "hello" "world")
  =>
  #t
) ;check
(check (string-ci<? "HELLO" "world")
  =>
  #t
) ;check
;; 相同字符串测试（不满足严格小于）
(check (string-ci<? "hello" "hello")
  =>
  #f
) ;check
(check (string-ci<? "HELLO" "hello")
  =>
  #f
) ;check
(check (string-ci<? "hello" "HELLO")
  =>
  #f
) ;check
;; 大于关系测试
(check (string-ci<? "def" "abc") => #f)
(check (string-ci<? "world" "hello")
  =>
  #f
) ;check
;; 多参数测试
(check (string-ci<? "a" "b" "c") => #t)
(check (string-ci<? "A" "B" "C") => #t)
(check (string-ci<? "a" "B" "c") => #t)
(check (string-ci<? "a" "a" "b") => #f)
(check (string-ci<? "c" "b" "a") => #f)
;; 空字符串测试
(check (string-ci<? "" "a") => #t)
(check (string-ci<? "" "abc") => #t)
(check (string-ci<? "abc" "") => #f)
(check (string-ci<? "" "") => #f)
;; 前缀测试
(check (string-ci<? "abc" "abcd") => #t)
(check (string-ci<? "ABC" "abcd") => #t)
(check (string-ci<? "abcd" "abc") => #f)
;; 混合测试
(check (string-ci<? "apple" "Banana")
  =>
  #t
) ;check
(check (string-ci<? "Apple" "banana")
  =>
  #t
) ;check
(check (string-ci<? "banana" "Apple")
  =>
  #f
) ;check
;; 错误处理测试
(check-catch 'type-error
  (string-ci<? 1 "hello")
) ;check-catch
(check-catch 'type-error
  (string-ci<? "hello" 'symbol)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-ci<?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-ci<? "hello")
) ;check-catch
(check-report)
