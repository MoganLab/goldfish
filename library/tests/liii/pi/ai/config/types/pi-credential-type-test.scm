;; pi-credential-type
;; 获取服务商 API 凭据 record 的 type 字段。
;;
;; 语法
;; ----
;; (pi-credential-type obj)
;;
;; 参数
;; ----
;; obj : any?
;; 服务商 API 凭据 record 实例。
;;
;; 返回值
;; ----
;; string?
;; type 字段的值。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let* ((obj-a (%make-pi-credential "google" "secret-key" "api_key"))
       (obj-b (%make-pi-credential "kimi-coding" "other-key" "oauth"))
      ) ;
  (check (pi-credential-type obj-a) => "api_key")
  (check (pi-credential-type obj-b) => "oauth")
) ;let*

(check-report)
