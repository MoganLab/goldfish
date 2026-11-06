;; putenv
;; 设置环境变量。
;;
;; 语法
;; ----
;; (putenv key value)
;;
;; 参数
;; ----
;; key : string?
;; 环境变量的名称。
;;
;; value : string?
;; 环境变量的值。
;;
;; 返回值
;; -----
;; boolean?
;; 成功返回 #t。
;;
;; 错误处理
;; ----
;; type-error
;; 当 key 或 value 不是字符串时抛出错误。

;; ; 基本功能测试

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

(check-true (putenv "TEST_VAR" "123"))
(check-true (putenv "TEST_VAR" "456"))

;; ; 值中含路径分隔符时不被拆分（如 X11 的 DISPLAY=":0"）
(check-true (putenv "TEST_VAR" ":0"))
(check (getenv "TEST_VAR") => ":0")
(check-true (putenv "TEST_VAR" "a:b:c"))
(check (getenv "TEST_VAR") => "a:b:c")

;; ; 错误测试
(check-catch 'type-error (putenv 123 "abc"))
(check-catch 'type-error (putenv "ABC" 123))
(check-catch 'type-error (putenv #\a "abc"))
(check-catch 'type-error (putenv "ABC" #\a))

(check-report)
