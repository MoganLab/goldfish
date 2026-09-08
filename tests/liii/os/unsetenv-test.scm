(import (liii check) (liii os))


(check-set-mode! 'report-failed)


;; unsetenv
;; 删除环境变量。
;;
;; 语法
;; ----
;; (unsetenv key)
;;
;; 参数
;; ----
;; key : string?
;; 要删除的环境变量名称。
;;
;; 返回值
;; -----
;; boolean?
;; 成功返回 #t。
;;
;; 错误
;; ----
;; type-error
;; 当 key 不是字符串时抛出错误。
;;
;; 说明
;; ----
;; 删除指定的环境变量，如果变量不存在则不执行任何操作。


;; ; 基本功能测试
(check-true (putenv "TEST_VAR" "123"))
(check (getenv "TEST_VAR") => "123")
(check-true (unsetenv "TEST_VAR"))
(check (getenv "TEST_VAR") => #f)


;; ; 测试删除不存在的变量
(check-true (unsetenv "NONEXISTENT_VAR_12345"))


;; ; 错误测试
(check-catch 'type-error (unsetenv #\a))
(check-catch 'type-error (unsetenv 123))


(check-report)
