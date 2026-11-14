;; pi-auth-args-key
;; 读取 <pi-auth-args> 记录的 key 字段。
;;
;; 语法
;; ----
;; (pi-auth-args-key args) -> string? | #f
;;
;; 参数
;; ----
;; args : <pi-auth-args>
;; auth 参数记录。
;;
;; 返回值
;; ------
;; string? | #f
;; API Key 字符串，未提供时为 #f。

(import (liii check) (liii pi code cli auth))

(check-set-mode! 'report-failed)

(check (pi-auth-args-key (make-pi-auth-args #f "login" "deepseek" "sk-123"))
  =>
  "sk-123"
) ;check
(check (pi-auth-args-key (make-pi-auth-args #f "print-api-key" "deepseek" #f)) => #f)

(check-report)
