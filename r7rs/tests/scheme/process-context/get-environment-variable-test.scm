;; get-environment-variable
;; 读取环境变量。
;;
;; 语法
;; ----
;; (get-environment-variable key)
;;
;; 参数
;; ----
;; key : string?
;; 环境变量的名字。
;;
;; 返回值
;; ----
;; string 或 #f
;; 返回环境变量 key 对应的字符串值，不存在时返回 #f。
;;
;; 错误处理
;; --------
;; type-error
;; key 必须是 string?，传入其他类型应报 type-error
;; 而不是把对象当作 C 字符串指针导致崩溃 (devel/0143.md)

(import (liii check) (scheme process-context))

(check-set-mode! 'report-failed)

(check (string? (get-environment-variable "PATH")) => #t)
(check (get-environment-variable "NO_SUCH_ENV_0143") => #f)

;; key 必须是 string?，传入其他类型应报 type-error
(check-catch 'type-error (get-environment-variable 123))
;; C 层入口 g_get-environment-variable 走同一实现
(check-catch 'type-error (g_get-environment-variable 123))

(check-report)
