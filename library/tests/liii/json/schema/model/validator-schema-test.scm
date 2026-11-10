;; validator-schema
;; 读取校验器记录中的根 Schema。
;;
;; 语法
;; ----
;; (validator-schema validator)
;;
;; 参数
;; ----
;; validator : validator
;; 校验器记录。
;;
;; 返回值
;; ----
;; any?
;; 校验器使用的根 Schema。
;;
;; 注意
;; ----
;; 记录不可变，读取到的 Schema 始终是创建时传入的那一份。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema model))

(check-set-mode! 'report-failed)

(define schema '(("type" . "integer")))

(define validator (make-validator schema '()))

(check (validator-schema validator) => schema)

;; 布尔 Schema 也可以作为根 Schema。

(check (validator-schema (make-validator #f '())) => #f)

(check-report)
