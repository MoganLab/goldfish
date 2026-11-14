;; make-pi-auth-args
;; 构造 <pi-auth-args> 记录。
;;
;; 语法
;; ----
;; (make-pi-auth-args help? subcommand provider key) -> <pi-auth-args>
;;
;; 参数
;; ----
;; help? : boolean?
;; 是否显示帮助。
;;
;; subcommand : string? | #f
;; auth 子命令名称。
;;
;; provider : string? | #f
;; 供应商名称。
;;
;; key : string? | #f
;; API Key 取值。
;;
;; 返回值
;; ------
;; <pi-auth-args>
;; 构造的 auth 参数记录。

(import (liii check) (liii pi code cli auth))

(check-set-mode! 'report-failed)

(let ((args (make-pi-auth-args #f "login" "deepseek" "sk-test")))
  (check-true (pi-auth-args? args))
  (check (pi-auth-args-help? args) => #f)
  (check (pi-auth-args-subcommand args) => "login")
  (check (pi-auth-args-provider args) => "deepseek")
  (check (pi-auth-args-key args) => "sk-test")
) ;let

(check-report)
