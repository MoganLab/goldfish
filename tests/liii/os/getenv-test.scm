(import (liii check)
  (liii os)
  (liii string)
) ;import


(check-set-mode! 'report-failed)


;; getenv
;; 获取环境变量的值。
;;
;; 语法
;; ----
;; (getenv key)
;; (getenv key default)
;;
;; 参数
;; ----
;; key : string?
;; 环境变量的名称。
;;
;; default : any?
;; 当环境变量不存在时返回的默认值（可选）。
;;
;; 返回值
;; -----
;; string? 或 #f 或 default
;; 返回环境变量的值，不存在时返回 #f 或默认值。


;; ; 基本功能测试
(check-true (putenv "TEST_VAR" "123"))
(check (getenv "TEST_VAR") => "123")
(check-true (putenv "TEST_VAR" "456"))
(check (getenv "TEST_VAR") => "456")
(check-true (unsetenv "TEST_VAR"))
(check (getenv "TEST_VAR") => #f)


(check-false (string-null? (getenv "PATH"))
) ;check-false
(unsetenv "PATH")
(check (getenv "PATH") => #f)
(unsetenv "home")
(check (getenv "home") => #f)
(check (getenv "home" "value does not found")
  =>
  "value does not found"
) ;check


(check-report)
