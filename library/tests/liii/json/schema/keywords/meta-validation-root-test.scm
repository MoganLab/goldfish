;; meta-validation-root
;; 读取当前的元校验根资源。
;;
;; 语法
;; ----
;; (meta-validation-root)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; any?
;; 当前设置的元校验根资源，未设置时返回 #f。
;;
;; 注意
;; ----
;; 元校验根资源是全局状态，
;; 用于在 check-schema 过程中把 $dynamicRef 解析到元 Schema。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema keywords) (liii json schema model))

(check-set-mode! 'report-failed)

(define meta-root '(("type" . "object")))

;; 初始状态下没有元校验根资源。

(set-meta-validation-root! #f)
(check-false (meta-validation-root))

;; 设置后可以读到。

(set-meta-validation-root! meta-root)
(check (meta-validation-root) => meta-root)

;; 复位后回到未设置状态。

(set-meta-validation-root! #f)
(check-false (meta-validation-root))

(check-report)
