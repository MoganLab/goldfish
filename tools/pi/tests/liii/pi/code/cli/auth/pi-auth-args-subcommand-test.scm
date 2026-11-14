;; pi-auth-args-subcommand
;; 读取 <pi-auth-args> 记录的 subcommand 字段。
;;
;; 语法
;; ----
;; (pi-auth-args-subcommand args) -> string? | #f
;;
;; 参数
;; ----
;; args : <pi-auth-args>
;; auth 参数记录。
;;
;; 返回值
;; ------
;; string? | #f
;; 子命令名称。

(import (liii check) (liii pi code cli auth))

(check-set-mode! 'report-failed)

(check (pi-auth-args-subcommand (make-pi-auth-args #f "login" "deepseek" "sk"))
  =>
  "login"
) ;check
(check (pi-auth-args-subcommand (make-pi-auth-args #t #f #f #f)) => #f)

(check-report)
