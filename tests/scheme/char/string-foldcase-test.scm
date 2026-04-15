(import (liii check) (scheme char))
(check-set-mode! 'report-failed)
;; string-foldcase
;; 将字符串转换为大小写无关的折叠形式（通常为小写）。
;;
;; 语法
;; ----
;; (string-foldcase str) → string?
;;
;; 参数
;; ----
;; str : string
;; 要转换的字符串
;;
;; 返回值
;; ------
;; string?
;; 转换后的折叠形式字符串（小写）
;;
;; 注意
;; ----
;; - 大写字母被转换为小写
;; - 小写字母保持不变
;; - 非字母字符保持不变
;; - 结果与 string-downcase 相同（简化实现）
;;
;; 错误处理
;; ------
;; type-error
;; 参数必须是字符串类型，否则会抛出异常
;; 基本功能测试
(check (string-foldcase "HELLO")
  =>
  "hello"
) ;check
(check (string-foldcase "hello")
  =>
  "hello"
) ;check
(check (string-foldcase "Hello")
  =>
  "hello"
) ;check
(check (string-foldcase "hElLo")
  =>
  "hello"
) ;check
;; 空字符串测试
(check (string-foldcase "") => "")
;; 数字测试
(check (string-foldcase "123") => "123")
(check (string-foldcase "ABC123")
  =>
  "abc123"
) ;check
(check (string-foldcase "123ABC")
  =>
  "123abc"
) ;check
;; 特殊字符测试
(check (string-foldcase "HELLO WORLD")
  =>
  "hello world"
) ;check
(check (string-foldcase "HELLO!")
  =>
  "hello!"
) ;check
(check (string-foldcase "HELLO@WORLD")
  =>
  "hello@world"
) ;check
(check (string-foldcase "HELLO_WORLD")
  =>
  "hello_world"
) ;check
(check (string-foldcase "HELLO-WORLD")
  =>
  "hello-world"
) ;check
;; 混合测试
(check (string-foldcase "AbCdEfG123")
  =>
  "abcdefg123"
) ;check
(check (string-foldcase "Hello World!")
  =>
  "hello world!"
) ;check
;; 与 string-downcase 一致性测试
(check (string-foldcase "HELLO")
  =>
  (string-downcase "HELLO")
) ;check
(check (string-foldcase "Hello")
  =>
  (string-downcase "Hello")
) ;check
(check (string-foldcase "hello")
  =>
  (string-downcase "hello")
) ;check
;; 错误处理测试
(check-catch 'type-error
  (string-foldcase 123)
) ;check-catch
(check-catch 'type-error
  (string-foldcase 'hello)
) ;check-catch
(check-catch 'type-error
  (string-foldcase #\a)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-foldcase)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-foldcase "hello" "world")
) ;check-catch
(check-report)