;; pi-auth-args-help?
;; 读取 <pi-auth-args> 记录的 help? 字段。
;;
;; 语法
;; ----
;; (pi-auth-args-help? args) -> boolean?
;;
;; 参数
;; ----
;; args : <pi-auth-args>
;; auth 参数记录。
;;
;; 返回值
;; ------
;; boolean?
;; 是否请求显示帮助信息。

(import (liii check) (liii pi code cli auth))

(check-set-mode! 'report-failed)

(check-true (pi-auth-args-help? (make-pi-auth-args #t #f #f #f)))
(check-false (pi-auth-args-help? (make-pi-auth-args #f "print-api-key" "deepseek" #f)))

(check-report)
