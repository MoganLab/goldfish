(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; string-ci=?
;; 按大小写不敏感的方式比较字符串是否相等。
;;
;; 语法
;; ----
;; (string-ci=? str1 str2 str3 ...) → boolean?
;;
;; 参数
;; ----
;; str1, str2, str3, ... : string
;; 要比较的字符串
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有字符串在大小写不敏感的情况下都相等，则返回 #t，否则返回 #f
;;
;; 注意
;; ----
;; - 对于相同字母的大小写不同形式（如 "hello" 和 "HELLO"），返回 #t
;; - 对于相同的字符串（无论大小写），返回 #t
;; - 对于不同的字符串，返回 #f
;; - 支持多个参数，只有当所有字符串在大小写不敏感的情况下都相等时才返回 #t
;; - 遵循 R7RS 标准规范
;;
;; 错误处理
;; ------
;; type-error
;; 所有参数必须是字符串类型，否则会抛出异常
;; 基本功能测试
(check (string-ci=? "hello" "HELLO")
  =>
  #t
) ;check
(check (string-ci=? "HELLO" "hello")
  =>
  #t
) ;check
(check (string-ci=? "Hello" "hElLo")
  =>
  #t
) ;check
(check (string-ci=? "abc" "ABC") => #t)
(check (string-ci=? "ABC" "abc") => #t)
;; 大小写一致测试
(check (string-ci=? "hello" "hello")
  =>
  #t
) ;check
(check (string-ci=? "HELLO" "HELLO")
  =>
  #t
) ;check
(check (string-ci=? "123" "123") => #t)
;; 不同大小写混合测试
(check (string-ci=? "abc" "def") => #f)
(check (string-ci=? "abc" "DEF") => #f)
(check (string-ci=? "ABC" "def") => #f)
(check (string-ci=? "ABC" "xyz") => #f)
(check (string-ci=? "XYZ" "abc") => #f)
;; 多参数测试
(check (string-ci=? "hello" "HELLO" "Hello")
  =>
  #t
) ;check
(check (string-ci=? "HELLO" "hello" "hello")
  =>
  #t
) ;check
(check (string-ci=? "abc" "ABC" "abc")
  =>
  #t
) ;check
(check (string-ci=? "abc" "def" "ABC")
  =>
  #f
) ;check
(check (string-ci=? "ABC" "DEF" "abc")
  =>
  #f
) ;check
;; 空字符串测试
(check (string-ci=? "" "") => #t)
(check (string-ci=? "" "a") => #f)
;; 数字字符测试（大小写不影响数字）
(check (string-ci=? "123" "123") => #t)
(check (string-ci=? "123" "456") => #f)
(check (string-ci=? "a1b2" "A1B2")
  =>
  #t
) ;check
;; 特殊字符测试
(check (string-ci=? "hello world"
         "HELLO WORLD"
       ) ;string-ci=?
  =>
  #t
) ;check
(check (string-ci=? "hello!" "HELLO!")
  =>
  #t
) ;check
(check (string-ci=? "hello@world"
         "HELLO@WORLD"
       ) ;string-ci=?
  =>
  #t
) ;check
;; 错误处理测试
(check-catch 'type-error
  (string-ci=? 1 "hello")
) ;check-catch
(check-catch 'type-error
  (string-ci=? "hello" 'symbol)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-ci=?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-ci=? "hello")
) ;check-catch
(check-report)
