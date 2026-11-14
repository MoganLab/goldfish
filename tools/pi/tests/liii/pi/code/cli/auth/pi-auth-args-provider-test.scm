;; pi-auth-args-provider
;; 读取 <pi-auth-args> 记录的 provider 字段。
;;
;; 语法
;; ----
;; (pi-auth-args-provider args) -> string? | #f
;;
;; 参数
;; ----
;; args : <pi-auth-args>
;; auth 参数记录。
;;
;; 返回值
;; ------
;; string? | #f
;; 供应商名称。

(import (liii check) (liii pi code cli auth))

(check-set-mode! 'report-failed)

(check (pi-auth-args-provider (make-pi-auth-args #f "print-api-key" "deepseek" #f))
  =>
  "deepseek"
) ;check
(check (pi-auth-args-provider (make-pi-auth-args #t #f #f #f)) => #f)

(check-report)
